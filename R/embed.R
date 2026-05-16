# embed.R — v0.5 unified embeddings + semantic retrieval via ragnar

#' Embed code nodes into the ragnar vector store
#'
#' Converts parsed code-graph nodes (functions, scripts, variables) into
#' natural-language text descriptions and inserts them into the ragnar RAG
#' store at `.explicar/explicar.duckdb` so that vector-similarity search is
#' available via [explicar_semantic_retrieve()] and the MCP `search_code` tool.
#'
#' Run [explicar_index_build()] first to populate the nodes table, then call
#' this once (or whenever the project changes significantly).
#'
#' @param project_dir Project directory.  Default `"."`.
#' @param db_path Path to the unified explicaR DuckDB.  Defaults to
#'   `.explicar/explicar.duckdb`.
#' @param model Ollama embedding model.  Default `"nomic-embed-text"`.
#' @param ollama_url Ollama base URL.  Default `"http://localhost:11434"`.
#' @param include What to embed.  Any subset of
#'   `c("nodes", "wiki", "source", "readme", "vignettes")`.
#'   `"nodes"` embeds code-graph nodes directly; the rest delegate to
#'   [explicar_ingest()] with `embed = TRUE`.
#' @param batch_size Nodes per progress update.  Default `20L`.
#' @param force Re-embed even if already present.  Default `FALSE`.
#' @param quiet Suppress progress messages.  Default `FALSE`.
#'
#' @return Invisibly, the number of node chunks embedded.
#' @export
#'
#' @examples
#' \dontrun{
#' # Build index + wiki first, then embed
#' explicar_index_build()
#' explicar_wiki_build()
#' explicar_embed()
#'
#' # Nodes only (fastest)
#' explicar_embed(include = "nodes")
#'
#' # Then search semantically
#' explicar_semantic_retrieve("clean survey data")
#' }
explicar_embed <- function(project_dir = ".",
                           db_path     = NULL,
                           model       = "nomic-embed-text",
                           ollama_url  = "http://localhost:11434",
                           include     = c("nodes", "wiki"),
                           batch_size  = 20L,
                           force       = FALSE,
                           quiet       = FALSE) {

  if (!requireNamespace("ragnar", quietly = TRUE)) {
    stop("Package 'ragnar' is required for explicar_embed().\n",
         "Install with: install.packages('ragnar')", call. = FALSE)
  }
  .require_duckdb()

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  db_path     <- db_path %||% .explicar_db_path(project_dir)

  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)

  # Bail early if Ollama is not reachable
  if (!ollama_available(model, ollama_url)) {
    if (!quiet) message("explicaR: Ollama unavailable \u2014 skipping vector embeddings")
    return(invisible(0L))
  }
  embed_fn <- ragnar::embed_ollama(model = model, base_url = ollama_url)

  store <- .ragnar_connect_or_create(db_path, embed_fn)
  if (is.null(store)) {
    stop("Could not connect to ragnar store at: ", db_path, call. = FALSE)
  }

  n_nodes <- 0L

  if ("nodes" %in% include) {
    n_nodes <- .ingest_nodes_ragnar(store, db_path, batch_size, force, quiet)
  }

  other <- intersect(include, c("wiki", "source", "readme", "vignettes"))
  if (length(other)) {
    tryCatch(
      explicar_ingest(
        project_dir = project_dir,
        db_path     = db_path,
        include     = other,
        embed       = TRUE,
        embed_model = model,
        ollama_url  = ollama_url,
        force       = force,
        quiet       = quiet
      ),
      error = function(e) {
        if (!quiet) message("explicaR: ingest failed: ", conditionMessage(e))
      }
    )
  }

  if (!quiet) message("explicaR: embed complete \u2014 ", n_nodes, " node chunks")
  invisible(n_nodes)
}


#' Semantic search over the code graph and documentation
#'
#' Retrieves the most relevant nodes, wiki pages, and docs for `query` using
#' the ragnar RAG store built by [explicar_embed()] and/or [explicar_ingest()].
#' Uses vector-similarity search when embeddings are available; falls back to
#' BM25 keyword search otherwise.
#'
#' @param query Search query string.
#' @param project_dir Project directory.  Default `"."`.
#' @param top_k Maximum number of results.  Default `10L`.
#' @param bm25_only Force BM25 keyword search (skip VSS).  Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with columns `origin`, `hash`, `text`,
#'   `source`, `url`, `page_title`.
#' @export
#'
#' @examples
#' \dontrun{
#' # After explicar_embed()
#' explicar_semantic_retrieve("function that filters rows by date")
#' explicar_semantic_retrieve("pivot longer reshape", bm25_only = TRUE)
#' }
explicar_semantic_retrieve <- function(query,
                                       project_dir = ".",
                                       top_k       = 10L,
                                       bm25_only   = FALSE) {
  if (!requireNamespace("ragnar", quietly = TRUE)) {
    stop("Package 'ragnar' is required.\n",
         "Install with: install.packages('ragnar')", call. = FALSE)
  }

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  db_path     <- .explicar_db_path(project_dir)

  if (!file.exists(db_path)) {
    stop("No store found at '", db_path, "'.\n",
         "Run explicar_embed() or explicar_ingest() first.", call. = FALSE)
  }

  store <- tryCatch(
    ragnar::ragnar_store_connect(db_path, read_only = TRUE),
    error = function(e) NULL
  )
  if (is.null(store)) stop("Could not open ragnar store at: ", db_path, call. = FALSE)

  if (bm25_only) {
    return(tryCatch(
      ragnar::ragnar_retrieve_bm25(store, query, top_k = as.integer(top_k)),
      error = function(e) ragnar::ragnar_retrieve_bm25(store, query, n = as.integer(top_k))
    ))
  }

  tryCatch(
    ragnar::ragnar_retrieve(store, query, top_k = as.integer(top_k)),
    error = function(e) {
      tryCatch(
        ragnar::ragnar_retrieve_bm25(store, query, top_k = as.integer(top_k)),
        error = function(e2) ragnar::ragnar_retrieve_bm25(store, query, n = as.integer(top_k))
      )
    }
  )
}


# ── Internal: ingest nodes into ragnar ────────────────────────────────────────

.ingest_nodes_ragnar <- function(store, db_path, batch_size, force, quiet) {
  # Try the unified store first; fall back to the legacy index.duckdb.
  # explicar_index_build() writes to index.duckdb; explicar.duckdb holds the
  # ragnar store + wiki. We read nodes from whichever file has them.
  nodes <- .read_nodes_from_db(db_path) %||%
           .read_nodes_from_db(.index_path_from_db(db_path))

  if (is.null(nodes) || nrow(nodes) == 0L) {
    if (!quiet) message("  No nodes found \u2014 run explicar_index_build() first")
    return(0L)
  }

  source_id  <- "nodes:code-graph"
  batch_size <- as.integer(batch_size)

  if (!force && .store_has_source(store, source_id)) {
    if (!quiet) message("  nodes already embedded (use force=TRUE to re-embed)")
    return(0L)
  }

  n <- 0L
  for (i in seq_len(nrow(nodes))) {
    nd   <- nodes[i, ]
    text <- .node_to_text_embed(nd)
    if (!nzchar(trimws(text))) next

    origin <- if (!is.na(nd$file) && nzchar(as.character(nd$file))) {
      paste0("file://", nd$file)
    } else NA_character_

    .safe_store_insert(store, source_id, text,
                       context = paste0("code graph: ", nd$type %||% "node"),
                       origin  = origin)
    n <- n + 1L

    if (!quiet && n %% batch_size == 0L) {
      message("  nodes: ", n, " / ", nrow(nodes))
    }
  }

  if (!quiet && n > 0L) message("  nodes: ", n, " chunks embedded")
  n
}

.read_nodes_from_db <- function(db_path) {
  if (!file.exists(db_path)) return(NULL)
  tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    if (!DBI::dbExistsTable(con, "nodes")) return(NULL)
    rows <- DBI::dbGetQuery(con, "SELECT name, type, file, line, label FROM nodes")
    if (nrow(rows) == 0L) NULL else rows
  }, error = function(e) NULL)
}

# Derive the legacy index.duckdb path from the explicar.duckdb path.
.index_path_from_db <- function(explicar_db_path) {
  file.path(dirname(explicar_db_path), "index.duckdb")
}

#' Format a node row as searchable text
#' Pattern: "type: `name` - label \[file:line\]"
#' @noRd
.node_to_text_embed <- function(nd) {
  type_lbl <- switch(as.character(nd$type %||% ""),
    "function" = "function",
    "script"   = "script",
    "variable" = "variable",
    "source"   = "data source",
    as.character(nd$type %||% "node")
  )

  out  <- paste0(type_lbl, ": `", nd$name, "`")
  lbl_raw <- nd$label %||% NA_character_
  lbl  <- if (is.na(lbl_raw)) "" else as.character(lbl_raw)
  name_raw <- nd$name %||% NA_character_
  name <- if (is.na(name_raw)) "" else as.character(name_raw)

  if (nzchar(lbl) && lbl != name) {
    out <- paste0(out, " \u2014 ", lbl)
  }

  file_raw <- nd$file %||% NA_character_
  file <- if (is.na(file_raw)) "" else as.character(file_raw)
  if (nzchar(file)) {
    line <- nd$line
    loc  <- if (!is.null(line) && !is.na(line) && line > 0L) {
      paste0(file, ":", line)
    } else {
      file
    }
    out <- paste0(out, " [", loc, "]")
  }

  out
}

#' Parse node texts back to a tibble with name/type/file/line/label columns
#' Reverses the format produced by .node_to_text_embed().
#' @noRd
.parse_node_text <- function(texts) {
  pat <- "^(.+?):\\s+`([^`]+)`(?:\\s+\u2014\\s+(.+?))?(?:\\s+\\[(.+?)(?::(\\d+))?\\])?$"
  m   <- regmatches(texts, regexec(pat, texts, perl = TRUE))

  pick <- function(lst, idx, default = NA_character_) {
    vapply(lst, function(x) {
      if (length(x) >= idx && nzchar(x[[idx]])) x[[idx]] else default
    }, character(1L))
  }

  tibble::tibble(
    name       = pick(m, 3L),
    type       = pick(m, 2L),
    file       = pick(m, 5L),
    line       = suppressWarnings(as.integer(pick(m, 6L))),
    label      = pick(m, 4L),
    shape_info = NA_character_
  )
}
