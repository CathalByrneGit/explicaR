#' Persistent DuckDB-backed index for explicaR
#'
#' Stores parsed nodes, edges, and verbs in a `.explicar/index.duckdb` file
#' inside the project directory. On subsequent calls only changed files are
#' re-parsed, making repeated `explicar()` runs fast.
#'
#' For semantic (vector) search, run [explicar_embed()] after building the
#' index — it uses the ragnar VSS store in `.explicar/explicar.duckdb`.
#'
#' @name explicar_index
NULL

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#' Build or incrementally update the persistent explicaR index
#'
#' Parses R scripts in `project_dir` and stores nodes, edges, and verbs in a
#' DuckDB database at `.explicar/index.duckdb`.  On subsequent calls only files
#' whose modification time has changed are re-parsed, so the operation is fast
#' after the initial build.
#'
#' @param project_dir Path to the R project directory. Default `"."`.
#' @param pattern     Glob pattern for R scripts. Default `"*.R"`.
#' @param recursive   Recurse into sub-directories. Default `TRUE`.
#' @param force       Re-parse all files even if unchanged. Default `FALSE`.
#' @param quiet       Suppress progress messages. Default `FALSE`.
#'
#' @return Invisibly, the path to the index database file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Build the index for the current project
#' explicar_index_build()
#'
#' # Force a full rebuild
#' explicar_index_build(force = TRUE)
#'
#' # Then embed nodes for semantic search (uses ragnar, not httr2)
#' explicar_embed()
#' }
explicar_index_build <- function(project_dir = ".",
                                 pattern      = "*.R",
                                 recursive    = TRUE,
                                 force        = FALSE,
                                 quiet        = FALSE) {
  .require_duckdb()

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  idx_dir  <- .index_dir(project_dir)
  idx_path <- .index_path(project_dir)

  if (!dir.exists(idx_dir)) dir.create(idx_dir, recursive = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = idx_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .ensure_schema(con)

  scripts <- .find_scripts(project_dir, pattern, recursive)
  if (length(scripts) == 0L) {
    if (!quiet) message("No R scripts found in ", project_dir)
    return(invisible(idx_path))
  }

  stale <- if (force) scripts else .stale_scripts(con, scripts)

  if (length(stale) == 0L) {
    if (!quiet) message("Index is up to date (", length(scripts), " files)")
    return(invisible(idx_path))
  }

  if (!quiet) {
    message("Indexing ", length(stale), " of ", length(scripts), " file(s)...")
  }

  # Remove stale entries before re-inserting
  .remove_file_entries(con, stale)

  # Parse the full project to get complete edge topology across all files
  parse_result <- explicar_parse(project_dir, pattern = glob2rx(pattern),
                                 recursive = recursive)

  # Only insert nodes/verbs for stale files; replace all edges (graph topology)
  nodes <- dplyr::filter(parse_result$nodes,
                         is.na(.data$file) | .data$file %in% stale)

  .insert_nodes(con, nodes)
  .replace_edges(con, parse_result$edges)
  .update_mtimes(con, stale)
  .upsert_functions(con, nodes)   # populate spec's `functions` table

  if (!quiet) message("Index saved to: ", idx_path)
  invisible(idx_path)
}

#' Retrieve nodes from the persistent index
#'
#' Searches the index for nodes matching `query`.  If embeddings are available
#' a combined keyword + cosine-similarity ranking is used; otherwise a simple
#' case-insensitive keyword search over `name` and `label` is performed.
#'
#' @param query       Search query string.
#' @param project_dir Path to the R project directory. Default `"."`.
#' @param top_k       Maximum number of results. Default `10L`.
#' @param type        Optional node-type filter, e.g. `"function"`.
#' @return A [tibble][tibble::tibble] of matching nodes, ordered by relevance,
#'   with columns `name`, `type`, `file`, `line`, `label`, `shape_info`, and
#'   optionally `similarity` when the ragnar VSS store is available.
#' @export
#'
#' @examples
#' \dontrun{
#' explicar_index_retrieve("clean survey data")
#' explicar_index_retrieve("pivot", type = "function", top_k = 5)
#' }
explicar_index_retrieve <- function(query,
                                    project_dir = ".",
                                    top_k       = 10L,
                                    type        = NULL) {
  .require_duckdb()

  project_dir <- normalizePath(project_dir, mustWork = TRUE)

  # v0.5: try ragnar on unified explicar.duckdb first (uses VSS when available,
  # BM25 otherwise); .parse_node_text() is defined in embed.R
  explicar_db <- .explicar_db_path(project_dir)
  if (file.exists(explicar_db) && requireNamespace("ragnar", quietly = TRUE)) {
    result <- tryCatch({
      store <- ragnar::ragnar_store_connect(explicar_db, read_only = TRUE)
      res   <- ragnar::ragnar_retrieve(store, query, top_k = as.integer(top_k) * 2L)
      tryCatch(DBI::dbDisconnect(store@con, shutdown = TRUE), error = function(e) invisible())

      node_rows <- res[!is.na(res$source) & res$source == "nodes:code-graph", , drop = FALSE]
      if (nrow(node_rows) == 0L) return(NULL)

      parsed <- .parse_node_text(node_rows$text)
      if (!is.null(type)) {
        parsed <- parsed[!is.na(parsed$type) & parsed$type %in% type, , drop = FALSE]
      }
      head(parsed, as.integer(top_k))
    }, error = function(e) NULL)

    if (!is.null(result) && nrow(result) > 0L) return(result)
  }

  idx_path <- .index_path(project_dir)
  if (!file.exists(idx_path)) {
    stop("No index found at '", idx_path, "'. Run explicar_index_build() first.")
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = idx_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .keyword_search(con, query, top_k, type)
}

#' Open a live DuckDB connection to the explicaR index
#'
#' Returns a raw [DBI] connection to `.explicar/index.duckdb` for advanced
#' queries.  You are responsible for disconnecting when done:
#' `DBI::dbDisconnect(con, shutdown = TRUE)`.
#'
#' @param project_dir Path to the R project directory. Default `"."`.
#' @param read_only   Open in read-only mode. Default `TRUE`.
#'
#' @return A DBI connection object.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- explicar_index_connect()
#' DBI::dbListTables(con)
#' dplyr::tbl(con, "nodes") |> dplyr::collect()
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
explicar_index_connect <- function(project_dir = ".", read_only = TRUE) {
  .require_duckdb()

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  idx_path    <- .index_path(project_dir)

  if (!file.exists(idx_path)) {
    stop("No index found at '", idx_path, "'. Run explicar_index_build() first.")
  }

  DBI::dbConnect(duckdb::duckdb(), dbdir = idx_path, read_only = read_only)
}

# ---------------------------------------------------------------------------
# Schema helpers
# ---------------------------------------------------------------------------

.index_dir  <- function(project_dir) file.path(project_dir, ".explicar")
.index_path <- function(project_dir) file.path(.index_dir(project_dir), "index.duckdb")

.ensure_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS nodes (
      name       VARCHAR,
      type       VARCHAR,
      file       VARCHAR,
      line       INTEGER,
      label      VARCHAR,
      shape_info VARCHAR,
      PRIMARY KEY (name, type, file)
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS edges (
      from_node  VARCHAR NOT NULL,
      to_node    VARCHAR NOT NULL,
      type       VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS file_mtimes (
      file  VARCHAR PRIMARY KEY,
      mtime DOUBLE
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS docs (
      doc_id     VARCHAR PRIMARY KEY,
      source     VARCHAR,
      url        VARCHAR,
      page_title VARCHAR,
      chunk_idx  INTEGER,
      context    VARCHAR,
      content    TEXT,
      fetched_at DOUBLE
    )
  ")

  # ── v0.3+ schema additions ────────────────────────────────────────────────
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS files (
      path          VARCHAR PRIMARY KEY,
      language      VARCHAR,
      lines         INTEGER,
      last_modified DOUBLE,
      description   VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS functions (
      name      VARCHAR,
      file      VARCHAR,
      line      INTEGER,
      language  VARCHAR,
      exported  BOOLEAN,
      signature VARCHAR,
      description VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS wiki (
      file          VARCHAR PRIMARY KEY,
      model         VARCHAR,
      generated_at  DOUBLE,
      last_modified DOUBLE,
      content       TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS _meta (
      key   VARCHAR PRIMARY KEY,
      value VARCHAR
    )
  ")

  invisible(con)
}

# ---------------------------------------------------------------------------
# File helpers
# ---------------------------------------------------------------------------

.find_scripts <- function(project_dir, pattern, recursive) {
  files <- list.files(
    project_dir,
    pattern   = glob2rx(pattern),
    recursive = recursive,
    full.names = TRUE
  )
  # Exclude common non-project directories
  files[!grepl("/.explicar/|/\\.git/|/renv/|/packrat/", files)]
}

# Paths relative to project_dir (used to match against nodes$file)
.rel <- function(paths, base) {
  base <- sub("/*$", "/", base)
  sub(paste0("^", gsub("([.\\|*^$?{}\\[\\]+()])", "\\\\\\1", base)), "", paths)
}

.stale_scripts <- function(con, scripts) {
  if (!DBI::dbExistsTable(con, "file_mtimes")) return(scripts)

  stored  <- DBI::dbGetQuery(con, "SELECT file, mtime FROM file_mtimes")
  current <- as.numeric(file.info(scripts)$mtime)

  Filter(function(path) {
    mtime <- current[match(path, scripts)]
    row   <- stored[stored$file == path, , drop = FALSE]
    nrow(row) == 0L || abs(row$mtime[[1L]] - mtime) > 0.01
  }, scripts)
}

# ---------------------------------------------------------------------------
# DML helpers
# ---------------------------------------------------------------------------

.remove_file_entries <- function(con, files) {
  if (length(files) == 0L) return(invisible())
  for (f in files) {
    DBI::dbExecute(con, "DELETE FROM nodes       WHERE file = ?", list(f))
    DBI::dbExecute(con, "DELETE FROM functions   WHERE file = ?", list(f))
    DBI::dbExecute(con, "DELETE FROM file_mtimes WHERE file = ?", list(f))
  }
  invisible()
}

.insert_nodes <- function(con, nodes) {
  if (nrow(nodes) == 0L) return(invisible())
  df <- data.frame(
    name       = as.character(nodes$name),
    type       = as.character(nodes$type),
    file       = as.character(nodes$file %||% NA_character_),
    line       = as.integer(nodes$line   %||% NA_integer_),
    label      = as.character(nodes$label      %||% NA_character_),
    shape_info = as.character(nodes$shape_info %||% NA_character_),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "nodes", df, append = TRUE)
}

.replace_edges <- function(con, edges) {
  DBI::dbExecute(con, "DELETE FROM edges")
  if (nrow(edges) == 0L) return(invisible())
  df <- data.frame(
    from_node = as.character(edges$from),
    to_node   = as.character(edges$to),
    type      = as.character(edges$type),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "edges", df, append = TRUE)
}

.update_mtimes <- function(con, files) {
  if (length(files) == 0L) return(invisible())
  for (f in files) DBI::dbExecute(con, "DELETE FROM file_mtimes WHERE file = ?", list(f))
  df <- data.frame(file = files, mtime = as.numeric(file.info(files)$mtime),
                   stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "file_mtimes", df, append = TRUE)
}

# Populate the spec's `functions` table from function-type nodes so it is
# available to MCP query_graph and analytics SQL.
.upsert_functions <- function(con, nodes) {
  fns <- dplyr::filter(nodes, .data$type == "function")
  if (nrow(fns) == 0L) return(invisible())

  # Remove existing entries for these files before re-inserting
  files_quoted <- unique(na.omit(fns$file))
  if (length(files_quoted)) {
    q <- paste0("'", gsub("'", "''", files_quoted), "'", collapse = ", ")
    DBI::dbExecute(con, paste0("DELETE FROM functions WHERE file IN (", q, ")"))
  }

  df <- data.frame(
    name        = as.character(fns$name),
    file        = as.character(fns$file %||% NA_character_),
    line        = as.integer(fns$line  %||% NA_integer_),
    language    = "r",
    exported    = FALSE,
    signature   = NA_character_,
    description = as.character(fns$label %||% NA_character_),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "functions", df, append = TRUE)
  invisible()
}

# ---------------------------------------------------------------------------
# Search helpers
# ---------------------------------------------------------------------------

.type_clause <- function(type) {
  if (is.null(type)) "" else paste0(" AND type = ", DBI::dbQuoteString(DBI::ANSI(), type))
}

.keyword_search <- function(con, query, top_k, type) {
  like_pat <- paste0("%", query, "%")
  type_filter <- if (!is.null(type)) " AND type = ?" else ""
  sql <- paste0(
    "SELECT name, type, file, line, label, shape_info FROM nodes",
    " WHERE (name ILIKE ? OR label ILIKE ?)", type_filter,
    " ORDER BY length(name) ASC LIMIT ", as.integer(top_k)
  )
  params <- if (!is.null(type)) list(like_pat, like_pat, type) else list(like_pat, like_pat)
  tibble::as_tibble(DBI::dbGetQuery(con, sql, params = params))
}

# ---------------------------------------------------------------------------
# Misc
# ---------------------------------------------------------------------------

.require_duckdb <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI",    quietly = TRUE)) {
    stop(
      "Packages 'duckdb' and 'DBI' are required for index operations.\n",
      "Install with: install.packages(c('duckdb', 'DBI'))",
      call. = FALSE
    )
  }
}

# ---------------------------------------------------------------------------
# Markdown chunker (shared by index-docs.R and future callers)
# ---------------------------------------------------------------------------

# Split markdown text into retrievable chunks, splitting at ## headings and
# then further at paragraph boundaries if a section exceeds max_chars.
# Returns a list of list(context, content).  An empty or whitespace-only
# text returns an empty list.
.chunk_markdown <- function(text, page_title, max_chars = 1200L) {
  text <- trimws(text)
  if (!nchar(text)) return(list())

  # Split at every ## (or deeper) heading, keeping the heading with its section
  parts <- strsplit(text, "(?m)(?=^#{2,}\\s)", perl = TRUE)[[1L]]
  parts <- Filter(function(p) nchar(trimws(p)) > 0L, parts)

  # If there are no sub-headings the whole text is a single chunk
  if (!length(parts)) {
    return(list(list(context = page_title, content = text)))
  }

  chunks <- list()

  for (part in parts) {
    part <- trimws(part)
    if (!nchar(part)) next

    # Derive a context label from the leading heading (if present)
    hm      <- regmatches(part, regexpr("^#{1,6}\\s+[^\n]+", part))
    heading <- if (length(hm)) trimws(sub("^#{1,6}\\s+", "", hm)) else page_title
    context <- if (identical(heading, page_title)) page_title
               else paste0(page_title, " \u203a ", heading)   # › separator

    if (nchar(part) <= max_chars) {
      chunks[[length(chunks) + 1L]] <- list(context = context, content = part)
      next
    }

    # Section too large — split further at paragraph boundaries
    paras   <- strsplit(part, "\n{2,}")[[1L]]
    current <- ""

    for (para in paras) {
      candidate <- if (nchar(current)) paste0(current, "\n\n", para) else para
      if (nchar(candidate) <= max_chars) {
        current <- candidate
      } else {
        if (nchar(trimws(current))) {
          chunks[[length(chunks) + 1L]] <- list(context = context,
                                                content = trimws(current))
        }
        current <- para
      }
    }
    if (nchar(trimws(current))) {
      chunks[[length(chunks) + 1L]] <- list(context = context,
                                            content = trimws(current))
    }
  }

  if (!length(chunks)) {
    return(list(list(context = page_title, content = text)))
  }
  chunks
}

