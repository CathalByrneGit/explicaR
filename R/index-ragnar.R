#' ragnar-backed documentation store for explicaR
#'
#' Provides hybrid BM25 + vector-similarity retrieval over local package
#' documentation (man pages, roxygen comments, README, vignettes) using the
#' [ragnar](https://github.com/tidyverse/ragnar) package.
#'
#' The ragnar store lives at `.explicar/docs.ragnar.duckdb`, separate from the
#' code-graph index.  All operations degrade gracefully when ragnar (or Ollama)
#' is not available.
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
  file.path(.index_dir(project_dir), "docs.ragnar.duckdb")
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
#' Extracts documentation from the package's own sources, chunks it with
#' `ragnar::markdown_chunk()`, and stores the result in a DuckDB-backed
#' ragnar store at `.explicar/docs.ragnar.duckdb`.  Hybrid BM25 + vector-
#' similarity retrieval is available automatically when Ollama is running.
#'
#' @param project_dir Path to the R project directory.  Default `"."`.
#' @param include Sources to index: any subset of `c("man", "source",
#'   "readme", "vignettes")`.
#' @param embed Logical; generate vector embeddings via Ollama for semantic
#'   (VSS) search.  Requires Ollama running locally.  Default `TRUE`.
#' @param embed_model Ollama embedding model.  Default `"nomic-embed-text"`.
#' @param ollama_url Ollama API base URL.
#' @param target_size Target chunk size in characters.  Default `1000L`.
#' @param force Re-index even if the store already contains entries for this
#'   package.  Default `FALSE`.
#' @param quiet Suppress progress messages.  Default `FALSE`.
#'
#' @return Invisibly, the path to the ragnar store file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Build with vector embeddings (Ollama must be running)
#' explicar_ragnar_build()
#'
#' # BM25-only (no Ollama needed)
#' explicar_ragnar_build(embed = FALSE)
#'
#' # Then retrieve
#' explicar_doc_retrieve("how does verb animation work")
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
  if (!.ragnar_available()) {
    stop(
      "The 'ragnar' package is required.\n",
      "Install with: install.packages('ragnar')",
      call. = FALSE
    )
  }

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  idx_dir     <- .index_dir(project_dir)
  if (!dir.exists(idx_dir)) dir.create(idx_dir, recursive = TRUE)

  store_path <- .ragnar_store_path(project_dir)
  pkg_name   <- .read_pkg_name(project_dir)
  source_id  <- paste0("local:", pkg_name)

  # Resolve embedding function
  embed_fn <- if (embed) .ragnar_embed_fn(ollama_url, embed_model) else NULL
  if (embed && is.null(embed_fn) && !quiet) {
    message("Ollama unavailable — building BM25-only store (no vector embeddings).")
  }

  # Create or open store
  if (force && file.exists(store_path)) {
    unlink(store_path)
    if (!quiet) message("Removed existing ragnar store.")
  }

  if (file.exists(store_path)) {
    store <- ragnar::ragnar_store_connect(store_path)

    # Check whether this source is already indexed
    n_existing <- tryCatch(
      nrow(DBI::dbGetQuery(store$con,
        paste0("SELECT 1 FROM chunks WHERE origin LIKE '",
               gsub("'", "''", source_id), "%' LIMIT 1"))),
      error = function(e) 0L
    )
    if (n_existing > 0L && !force) {
      if (!quiet) message("already indexed (", source_id, "). Use force = TRUE to rebuild.")
      return(invisible(store_path))
    }

    # Remove stale rows for this source before re-inserting
    tryCatch(
      DBI::dbExecute(store$con,
        paste0("DELETE FROM chunks WHERE origin LIKE '",
               gsub("'", "''", source_id), "%'")),
      error = function(e) invisible(NULL)
    )
  } else {
    store <- ragnar::ragnar_store_create(
      location   = store_path,
      embed      = embed_fn,
      extra_cols = data.frame(source = character(), url = character(),
                              page_title = character())
    )
  }

  if (!quiet) message("Building ragnar doc store for ", pkg_name, "...")

  # Collect chunks from each source type
  all_chunks <- list()

  if ("man" %in% include) {
    ch <- .ragnar_rd_chunks(project_dir, source_id, pkg_name, target_size)
    if (!is.null(ch) && nrow(ch) > 0L) {
      if (!quiet) message("  man/: ", nrow(ch), " chunk(s)")
      all_chunks[[length(all_chunks) + 1L]] <- ch
    }
  }

  # Track names already covered by Rd for deduplication with source chunks
  rd_names <- if ("man" %in% include && length(all_chunks)) {
    unique(all_chunks[[length(all_chunks)]]$page_title)
  } else {
    character(0L)
  }

  if ("source" %in% include) {
    ch <- .ragnar_source_chunks(project_dir, source_id, rd_names, target_size)
    if (!is.null(ch) && nrow(ch) > 0L) {
      if (!quiet) message("  R/: ", nrow(ch), " chunk(s)")
      all_chunks[[length(all_chunks) + 1L]] <- ch
    }
  }

  if ("readme" %in% include) {
    ch <- .ragnar_readme_chunks(project_dir, source_id, target_size)
    if (!is.null(ch) && nrow(ch) > 0L) {
      if (!quiet) message("  README: ", nrow(ch), " chunk(s)")
      all_chunks[[length(all_chunks) + 1L]] <- ch
    }
  }

  if ("vignettes" %in% include) {
    ch <- .ragnar_vignette_chunks(project_dir, source_id, target_size)
    if (!is.null(ch) && nrow(ch) > 0L) {
      if (!quiet) message("  vignettes/: ", nrow(ch), " chunk(s)")
      all_chunks[[length(all_chunks) + 1L]] <- ch
    }
  }

  if (!length(all_chunks)) {
    if (!quiet) message("No documentation found in ", project_dir)
    return(invisible(store_path))
  }

  chunks_df <- do.call(rbind, all_chunks)
  if (!quiet) message("Inserting ", nrow(chunks_df), " total chunk(s)...")

  ragnar::ragnar_store_insert(store, chunks_df)

  if (!quiet) message("Ragnar store saved: ", store_path)
  invisible(store_path)
}


#' Retrieve documentation chunks from the ragnar store
#'
#' Performs hybrid BM25 + vector-similarity search over the ragnar store built
#' by [explicar_ragnar_build()].  Falls back to BM25-only when no embeddings
#' are stored.
#'
#' @param query Search query string.
#' @param project_dir Path to the R project directory.  Default `"."`.
#' @param n Maximum number of results to return.  Default `10L`.
#' @param bm25_only Force BM25-only search (ignores any stored embeddings).
#'   Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with columns `origin`, `text`, `source`,
#'   `url`, `page_title`, and (when VSS is used) `similarity`.
#' @export
#'
#' @examples
#' \dontrun{
#' explicar_ragnar_build()
#' explicar_doc_retrieve("how does pivot_longer work")
#' explicar_doc_retrieve("filter rows", bm25_only = TRUE)
#' }
explicar_doc_retrieve <- function(query,
                                   project_dir = ".",
                                   n           = 10L,
                                   bm25_only   = FALSE) {
  if (!.ragnar_available()) {
    stop(
      "The 'ragnar' package is required.\n",
      "Install with: install.packages('ragnar')",
      call. = FALSE
    )
  }

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  store_path  <- .ragnar_store_path(project_dir)

  if (!file.exists(store_path)) {
    stop(
      "No ragnar doc store found at '", store_path, "'.\n",
      "Run explicar_ragnar_build() first.",
      call. = FALSE
    )
  }

  store <- ragnar::ragnar_store_connect(store_path)

  if (bm25_only) {
    return(ragnar::ragnar_retrieve_bm25(store, query, n = as.integer(n)))
  }

  tryCatch(
    ragnar::ragnar_retrieve(store, query, n = as.integer(n)),
    error = function(e) {
      # Hybrid failed (likely no embeddings) — degrade to BM25
      ragnar::ragnar_retrieve_bm25(store, query, n = as.integer(n))
    }
  )
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
  store_path  <- .ragnar_store_path(project_dir)

  if (!file.exists(store_path)) {
    stop("No ragnar store found. Run explicar_ragnar_build() first.", call. = FALSE)
  }

  store <- ragnar::ragnar_store_connect(store_path)

  if (is.null(store_description)) {
    pkg_name <- .read_pkg_name(project_dir)
    store_description <- paste0(
      "Documentation for the R package '", pkg_name, "': ",
      "function reference (man pages and roxygen comments), README, and vignettes."
    )
  }

  ragnar::ragnar_register_tool_retrieve(chat, store,
                                        store_description = store_description)
  invisible(chat)
}
