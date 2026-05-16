#' ragnar-backed documentation store for explicaR
#'
#' Provides hybrid BM25 + vector-similarity retrieval over local package
#' documentation using the [ragnar](https://github.com/tidyverse/ragnar) package.
#'
#' The unified store lives at `.explicar/explicar.duckdb`.
#' Use [explicar_ingest()] to build it and [explicar_semantic_retrieve()] to
#' query it.  The functions in this file (`explicar_ragnar_build`,
#' `explicar_doc_retrieve`) are deprecated wrappers kept for compatibility.
#'
#' @name explicar_ragnar
NULL


# ---------------------------------------------------------------------------
# Internals — helpers
# ---------------------------------------------------------------------------

.ragnar_available <- function() {
  requireNamespace("ragnar", quietly = TRUE)
}

.ragnar_store_path <- function(project_dir) {
  .explicar_db_path(project_dir)
}

#' Build or reuse an embedding function, falling back gracefully
#' @noRd
.ragnar_embed_fn <- function(ollama_url = "http://localhost:11434",
                              model      = "nomic-embed-text") {
  tryCatch(
    ragnar::embed_ollama(model = model, base_url = ollama_url),
    error = function(e) NULL
  )
}

#' Chunk a single markdown text using ragnar::markdown_chunk(), with fallback
#' Returns a character vector of chunk texts.
#' @noRd
.ragnar_chunk <- function(text, target_size = 1000L) {
  if (!nchar(trimws(text))) return(character(0L))

  tryCatch({
    raw <- ragnar::markdown_chunk(text, target_size = as.integer(target_size))
    # ragnar returns a character vector (one element per chunk)
    if (is.character(raw)) return(raw)
    # Some versions may return a data frame with a "text" column
    if (is.data.frame(raw) && "text" %in% names(raw)) return(raw$text)
    as.character(raw)
  }, error = function(e) {
    # Degrade to our simple paragraph splitter
    chunks <- .chunk_markdown(text, "doc", max_chars = as.integer(target_size))
    vapply(chunks, `[[`, "", "content")
  })
}


# ---------------------------------------------------------------------------
# Chunk builders — one per source type
# ---------------------------------------------------------------------------

.ragnar_rd_chunks <- function(project_dir, source_id, pkg_name,
                               target_size = 1000L) {
  man_dir  <- file.path(project_dir, "man")
  rd_files <- if (dir.exists(man_dir)) {
    list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  } else {
    character(0L)
  }
  if (!length(rd_files)) return(NULL)

  rows <- list()
  for (rf in rd_files) {
    md <- .rd_to_markdown(rf, pkg_name)
    if (is.null(md)) next
    texts <- .ragnar_chunk(md$text, target_size)
    if (!length(texts)) next

    for (i in seq_along(texts)) {
      rows[[length(rows) + 1L]] <- list(
        origin     = paste0(source_id, "/man/", basename(rf), "/", i),
        text       = texts[[i]],
        source     = source_id,
        url        = paste0("file://", rf),
        page_title = md$title
      )
    }
  }
  .rows_to_df(rows)
}

.ragnar_source_chunks <- function(project_dir, source_id, exclude_names = character(0L),
                                   target_size = 1000L) {
  r_dir   <- file.path(project_dir, "R")
  r_files <- if (dir.exists(r_dir)) {
    list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  } else {
    character(0L)
  }
  if (!length(r_files)) return(NULL)

  rows <- list()
  for (rf in r_files) {
    docs <- .parse_roxygen_blocks(rf)
    for (doc in docs) {
      if (doc$name %in% exclude_names) next
      md  <- .format_roxydoc(doc)
      texts <- .ragnar_chunk(md$text, target_size)
      if (!length(texts)) next

      for (i in seq_along(texts)) {
        rows[[length(rows) + 1L]] <- list(
          origin     = paste0(source_id, "/source/", basename(rf), "/", doc$name, "/", i),
          text       = texts[[i]],
          source     = source_id,
          url        = paste0("file://", rf, "#L", doc$def_line),
          page_title = md$title
        )
      }
    }
  }
  .rows_to_df(rows)
}

.ragnar_readme_chunks <- function(project_dir, source_id, target_size = 1000L) {
  candidates <- c("README.md", "README.Rmd", "readme.md")
  readme <- Filter(file.exists,
                   file.path(project_dir, candidates))
  if (!length(readme)) return(NULL)
  readme <- readme[[1L]]

  text <- tryCatch(
    paste(readLines(readme, warn = FALSE), collapse = "\n"),
    error = function(e) NULL
  )
  if (is.null(text) || !nchar(trimws(text))) return(NULL)

  texts <- .ragnar_chunk(text, target_size)
  if (!length(texts)) return(NULL)

  .rows_to_df(lapply(seq_along(texts), function(i) list(
    origin     = paste0(source_id, "/readme/", i),
    text       = texts[[i]],
    source     = source_id,
    url        = paste0("file://", readme),
    page_title = "README"
  )))
}

.ragnar_vignette_chunks <- function(project_dir, source_id, target_size = 1000L) {
  vig_dir <- file.path(project_dir, "vignettes")
  vigs    <- if (dir.exists(vig_dir)) {
    list.files(vig_dir, pattern = "\\.(Rmd|qmd|md)$", full.names = TRUE)
  } else {
    character(0L)
  }
  if (!length(vigs)) return(NULL)

  rows <- list()
  for (vf in vigs) {
    text <- tryCatch(
      paste(readLines(vf, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    if (is.null(text) || !nchar(trimws(text))) next

    title  <- .first_heading(text) %||% tools::file_path_sans_ext(basename(vf))
    texts  <- .ragnar_chunk(text, target_size)
    if (!length(texts)) next

    for (i in seq_along(texts)) {
      rows[[length(rows) + 1L]] <- list(
        origin     = paste0(source_id, "/vignettes/", basename(vf), "/", i),
        text       = texts[[i]],
        source     = source_id,
        url        = paste0("file://", vf),
        page_title = title
      )
    }
  }
  .rows_to_df(rows)
}

#' Convert a list of row-lists to a data frame with origin/hash/text + extra cols
#' @noRd
.rows_to_df <- function(rows) {
  if (!length(rows)) return(NULL)
  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  df$hash <- vapply(df$text, rlang::hash, character(1L))
  df[, c("origin", "hash", "text", "source", "url", "page_title"), drop = FALSE]
}


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Build (or update) the ragnar documentation store
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use [explicar_ingest()] instead, which writes
#' to the unified `.explicar/explicar.duckdb` store.
#'
#' `"man"` (Rd file indexing) is not supported by [explicar_ingest()]; use
#' `include = "source"` for roxygen-based doc indexing.
#'
#' @param project_dir Path to the R project directory.  Default `"."`.
#' @param include Sources to index.
#' @param embed Logical; generate vector embeddings.  Default `TRUE`.
#' @param embed_model Ollama embedding model.  Default `"nomic-embed-text"`.
#' @param ollama_url Ollama API base URL.
#' @param target_size Ignored (kept for compatibility).
#' @param force Re-index even if already present.  Default `FALSE`.
#' @param quiet Suppress progress messages.  Default `FALSE`.
#'
#' @return Invisibly, the path to `.explicar/explicar.duckdb`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Deprecated \u2014 use explicar_ingest() instead:
#' explicar_ingest("path/to/project", embed = FALSE)
#' }
explicar_ragnar_build <- function(project_dir = ".",
                                   include     = c("man", "source",
                                                    "readme", "vignettes"),
                                   embed       = TRUE,
                                   embed_model = "nomic-embed-text",
                                   ollama_url  = "http://localhost:11434",
                                   target_size = 1000L,
                                   force       = FALSE,
                                   quiet       = FALSE) {
  .Deprecated("explicar_ingest",
    msg = paste0(
      "explicar_ragnar_build() is deprecated and will be removed in v1.0.\n",
      "Use explicar_ingest() which writes to the unified .explicar/explicar.duckdb store.\n",
      "Note: 'man' (Rd file) indexing is not available via explicar_ingest()."
    )
  )

  # Map old include values to those supported by explicar_ingest()
  ingest_include <- intersect(include, c("source", "readme", "vignettes"))
  if ("wiki" %in% include) ingest_include <- c(ingest_include, "wiki")

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  db_path     <- .explicar_db_path(project_dir)

  tryCatch(
    explicar_ingest(
      project_dir = project_dir,
      include     = if (length(ingest_include)) ingest_include else c("source", "readme"),
      embed       = embed,
      embed_model = embed_model,
      ollama_url  = ollama_url,
      force       = force,
      quiet       = quiet
    ),
    error = function(e) {
      if (!quiet) message("explicar_ragnar_build: ingest failed: ", conditionMessage(e))
    }
  )

  invisible(db_path)
}


#' Retrieve documentation chunks from the ragnar store
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use [explicar_semantic_retrieve()] instead,
#' which queries the unified `.explicar/explicar.duckdb` store.
#'
#' @param query Search query string.
#' @param project_dir Path to the R project directory.  Default `"."`.
#' @param n Maximum number of results to return.  Default `10L`.
#' @param bm25_only Force BM25-only search.  Default `FALSE`.
#'
#' @return A tibble of matching chunks.
#' @export
#'
#' @examples
#' \dontrun{
#' # Deprecated — use explicar_semantic_retrieve() instead:
#' explicar_semantic_retrieve("how does the parse dispatch work")
#' }
explicar_doc_retrieve <- function(query,
                                   project_dir = ".",
                                   n           = 10L,
                                   bm25_only   = FALSE) {
  .Deprecated("explicar_semantic_retrieve",
    msg = paste0(
      "explicar_doc_retrieve() is deprecated and will be removed in v1.0.\n",
      "Use explicar_semantic_retrieve() which queries the unified .explicar/explicar.duckdb store."
    )
  )
  explicar_semantic_retrieve(query,
                             project_dir = project_dir,
                             top_k       = as.integer(n),
                             bm25_only   = bm25_only)
}


#' Register the ragnar doc store as a retrieval tool in an ellmer chat
#'
#' Enables the LLM to call `retrieve()` to look up package documentation
#' during a conversation.  Requires both `ragnar` and `ellmer`.
#'
#' @param chat An `ellmer::Chat` object.
#' @param project_dir Path to the R project directory.
#' @param store_description Optional description of what the store contains,
#'   shown to the LLM.
#'
#' @return The modified `chat` object (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' chat <- chat_ollama(model = "llama3.2")
#' chat <- explicar_register_retrieve(chat)
#' chat$chat("How does explicar_parse work?")
#' }
explicar_register_retrieve <- function(chat, project_dir = ".",
                                        store_description = NULL) {
  if (!.ragnar_available()) {
    stop("The 'ragnar' package is required.", call. = FALSE)
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The 'ellmer' package is required.", call. = FALSE)
  }

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  store_path  <- .explicar_db_path(project_dir)

  if (!file.exists(store_path)) {
    stop(
      "No ragnar store found at '", store_path, "'.\n",
      "Run explicar_ingest() first.",
      call. = FALSE
    )
  }

  store <- ragnar::ragnar_store_connect(store_path)

  if (is.null(store_description)) {
    pkg_name <- .read_pkg_name(project_dir)
    store_description <- paste0(
      "Code graph, wiki pages, and documentation for the project '", pkg_name, "': ",
      "function reference, README, vignettes, and LLM-generated wiki pages."
    )
  }

  ragnar::ragnar_register_tool_retrieve(chat, store,
                                        store_description = store_description)
  invisible(chat)
}
