# ── Ingest pipeline (v0.3) ────────────────────────────────────────────────────
#
# Ingests wiki pages, roxygen docs, and README into the unified ragnar store
# at `.explicar/explicar.duckdb`.  The same DuckDB file holds both the ragnar
# store tables and the explicaR-owned code-graph / wiki tables.

#' Ingest project docs into the ragnar RAG store
#'
#' Reads wiki pages (from the `wiki` table built by [explicar_wiki_build()]),
#' roxygen source comments, and README into the ragnar-backed store so that
#' [explicar_doc_retrieve()] and [deep_research()] can query them.
#'
#' The store lives at `.explicar/explicar.duckdb` alongside the code-graph
#' tables.
#'
#' @param project_dir Project directory. Default `"."`.
#' @param db_path Path to the store. Defaults to `.explicar/explicar.duckdb`.
#' @param include Sources to ingest. Any subset of
#'   `c("wiki", "source", "readme", "vignettes")`.
#' @param embed Generate vector embeddings (requires Ollama). Default `FALSE`.
#' @param embed_model Ollama embedding model.
#' @param ollama_url Ollama API base URL.
#' @param force Re-ingest even if chunks already exist. Default `FALSE`.
#' @param quiet Suppress progress messages. Default `FALSE`.
#'
#' @return Invisibly, the total number of chunks ingested.
#' @export
#'
#' @examples
#' \dontrun{
#' # Build wiki first, then ingest everything
#' explicar_wiki_build("path/to/project")
#' explicar_ingest("path/to/project")
#'
#' # With vector embeddings for semantic search (Ollama required)
#' explicar_ingest("path/to/project", embed = TRUE)
#' }
explicar_ingest <- function(project_dir = ".",
                             db_path    = NULL,
                             include    = c("wiki", "source", "readme", "vignettes"),
                             embed      = FALSE,
                             embed_model = "nomic-embed-text",
                             ollama_url  = "http://localhost:11434",
                             force       = FALSE,
                             quiet       = FALSE) {

  if (!requireNamespace("ragnar", quietly = TRUE)) {
    stop("Package 'ragnar' is required for explicar_ingest().\n",
         "Install with: install.packages('ragnar')", call. = FALSE)
  }
  .require_duckdb()

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  db_path     <- db_path %||% .explicar_db_path(project_dir)

  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)

  # Build or connect to ragnar store
  embed_fn <- if (embed && requireNamespace("httr2", quietly = TRUE) &&
                  ollama_available(embed_model, ollama_url)) {
    tryCatch(ragnar::embed_ollama(model = embed_model, base_url = ollama_url),
             error = function(e) NULL)
  } else NULL

  store <- .ragnar_connect_or_create(db_path, embed_fn)
  if (is.null(store)) stop("Could not connect to ragnar store at: ", db_path, call. = FALSE)

  n_total <- 0L

  if ("wiki" %in% include) {
    n <- .ingest_wiki(store, db_path, force = force, quiet = quiet)
    n_total <- n_total + n
  }

  if ("source" %in% include) {
    r_scripts <- list.files(project_dir, pattern = "\\.R$",
                            full.names = TRUE, recursive = FALSE)
    r_scripts <- r_scripts[!grepl("/.explicar/|/.git/|/renv/|/packrat/", r_scripts)]
    n <- .ingest_source(store, r_scripts, force = force, quiet = quiet)
    n_total <- n_total + n
  }

  if ("readme" %in% include) {
    n <- .ingest_readme_file(store, project_dir, force = force, quiet = quiet)
    n_total <- n_total + n
  }

  if ("vignettes" %in% include) {
    n <- .ingest_vignettes_dir(store, project_dir, force = force, quiet = quiet)
    n_total <- n_total + n
  }

  if (!quiet) message("Building ragnar index…")
  tryCatch(ragnar::ragnar_store_build_index(store), error = function(e) invisible())

  if (!quiet) message("Ingested ", n_total, " chunks into ", db_path)
  invisible(n_total)
}


# ── Per-source ingest helpers ──────────────────────────────────────────────────

.ingest_wiki <- function(store, db_path, force, quiet) {
  # Open separate DBI connection to read wiki table (ragnar holds the store conn)
  wiki_rows <- tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    if (!DBI::dbExistsTable(con, "wiki")) {
      if (!quiet) message("  No wiki table — run explicar_wiki_build() first")
      return(0L)
    }
    DBI::dbGetQuery(con,
      "SELECT file, content FROM wiki WHERE content IS NOT NULL AND content != ''")
  }, error = function(e) NULL)

  if (is.null(wiki_rows) || nrow(wiki_rows) == 0L) return(0L)

  n <- 0L
  for (i in seq_len(nrow(wiki_rows))) {
    file_path <- wiki_rows$file[[i]]
    content   <- wiki_rows$content[[i]]
    source_id <- paste0("wiki:", file_path)

    if (!force && .store_has_source(store, source_id)) next

    page_title <- basename(file_path)
    chunks     <- .chunk_markdown(content, page_title)

    for (chunk in chunks) {
      .safe_store_insert(store, source_id, chunk$content, chunk$context,
                         origin = paste0("file://", file_path))
      n <- n + 1L
    }
  }
  if (!quiet && n > 0L) message("  wiki: ", n, " chunks")
  n
}

.ingest_source <- function(store, scripts, force, quiet) {
  n <- 0L
  for (script in scripts) {
    source_id <- paste0("source:", script)
    if (!force && .store_has_source(store, source_id)) next

    # Extract roxygen blocks
    tryCatch({
      lines <- readLines(script, warn = FALSE)
      roxy  <- grep("^#'", lines, value = TRUE)
      if (!length(roxy)) return()

      text  <- paste(sub("^#'\\s?", "", roxy), collapse = "\n")
      page_title <- paste0("roxygen: ", basename(script))

      for (chunk in .chunk_markdown(text, page_title)) {
        .safe_store_insert(store, source_id, chunk$content, chunk$context,
                           origin = paste0("file://", script))
        n <- n + 1L
      }
    }, error = function(e) invisible())
  }
  if (!quiet && n > 0L) message("  source roxygen: ", n, " chunks")
  n
}

.ingest_readme_file <- function(store, project_dir, force, quiet) {
  readme <- Filter(file.exists, c(
    file.path(project_dir, "README.md"),
    file.path(project_dir, "README.Rmd")
  ))
  if (!length(readme)) return(0L)

  readme    <- readme[[1L]]
  source_id <- paste0("readme:", readme)
  if (!force && .store_has_source(store, source_id)) return(0L)

  tryCatch({
    text   <- paste(readLines(readme, warn = FALSE), collapse = "\n")
    chunks <- .chunk_markdown(text, "README")
    n <- 0L
    for (chunk in chunks) {
      .safe_store_insert(store, source_id, chunk$content, chunk$context,
                         origin = paste0("file://", readme))
      n <- n + 1L
    }
    if (!quiet && n > 0L) message("  readme: ", n, " chunks")
    n
  }, error = function(e) 0L)
}

.ingest_vignettes_dir <- function(store, project_dir, force, quiet) {
  vdir    <- file.path(project_dir, "vignettes")
  if (!dir.exists(vdir)) return(0L)
  vfiles  <- list.files(vdir, pattern = "\\.(Rmd|qmd|md)$",
                        full.names = TRUE, recursive = TRUE)
  n <- 0L
  for (vf in vfiles) {
    source_id <- paste0("vignette:", vf)
    if (!force && .store_has_source(store, source_id)) next
    tryCatch({
      text   <- paste(readLines(vf, warn = FALSE), collapse = "\n")
      title  <- tools::file_path_sans_ext(basename(vf))
      chunks <- .chunk_markdown(text, title)
      for (chunk in chunks) {
        .safe_store_insert(store, source_id, chunk$content, chunk$context,
                           origin = paste0("file://", vf))
        n <- n + 1L
      }
    }, error = function(e) invisible())
  }
  if (!quiet && n > 0L) message("  vignettes: ", n, " chunks")
  n
}


# ── Ragnar store helpers ───────────────────────────────────────────────────────

.ragnar_connect_or_create <- function(db_path, embed_fn) {
  tryCatch(
    ragnar::ragnar_store_connect(db_path, read_only = FALSE),
    error = function(e) {
      tryCatch(
        ragnar::ragnar_store_create(db_path, embed = embed_fn, version = 1L),
        error = function(e2) NULL
      )
    }
  )
}

.store_has_source <- function(store, source_id) {
  tryCatch({
    res <- ragnar::ragnar_retrieve(store, source_id, top_k = 1L)
    nrow(res) > 0L
  }, error = function(e) FALSE)
}

.safe_store_insert <- function(store, source_id, text, context,
                                origin = NA_character_) {
  if (!nzchar(trimws(text))) return(invisible())
  df <- data.frame(
    origin     = origin,
    hash       = rlang::hash(text),
    text       = text,
    source     = source_id,
    url        = origin,
    page_title = context,
    stringsAsFactors = FALSE
  )
  tryCatch(ragnar::ragnar_store_insert(store, df), error = function(e) invisible())
}
