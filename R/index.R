#' Persistent DuckDB-backed index for explicaR
#'
#' Stores parsed nodes, edges, and verbs in a `.explicar/index.duckdb` file
#' inside the project directory. On subsequent calls only changed files are
#' re-parsed, making repeated `explicar()` runs fast.
#'
#' When the `httr2` package is available and Ollama is running, embeddings can
#' be stored alongside the graph for semantic (vector) retrieval.
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
#' @param embed       Generate text embeddings for semantic retrieval.
#'   Requires Ollama to be running and the `httr2` package.  Default `FALSE`.
#' @param embed_model Ollama embedding model. Default `"nomic-embed-text"`.
#' @param ollama_url  Ollama API base URL. Default `"http://localhost:11434"`.
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
#' # Also embed nodes for semantic search (requires Ollama)
#' explicar_index_build(embed = TRUE, embed_model = "nomic-embed-text")
#' }
explicar_index_build <- function(project_dir = ".",
                                 pattern      = "*.R",
                                 recursive    = TRUE,
                                 embed        = FALSE,
                                 embed_model  = "nomic-embed-text",
                                 ollama_url   = "http://localhost:11434",
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

  # Parse the full project (CodeDepends needs all files together for edges)
  parse_result <- explicar_parse(project_dir, pattern = glob2rx(pattern),
                                 recursive = recursive)

  # Only insert nodes/verbs for stale files; replace all edges (graph topology)
  stale_rel <- .rel(stale, project_dir)
  nodes <- dplyr::filter(parse_result$nodes,
                         is.na(.data$file) | .data$file %in% stale_rel)
  verbs <- dplyr::filter(parse_result$verbs, .data$file %in% stale_rel)

  .insert_nodes(con, nodes)
  .replace_edges(con, parse_result$edges)
  .insert_verbs(con, verbs)
  .update_mtimes(con, stale)

  if (embed) {
    if (!requireNamespace("httr2", quietly = TRUE)) {
      if (!quiet) message("Skipping embeddings: 'httr2' package not installed.")
    } else if (!ollama_available(embed_model, ollama_url)) {
      if (!quiet) {
        message("Skipping embeddings: Ollama not running or model '",
                embed_model, "' unavailable.")
      }
    } else {
      if (!quiet) message("Generating embeddings...")
      .embed_nodes(con, embed_model, ollama_url, quiet = quiet)
    }
  }

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
#' @param embed_model Ollama model used to embed the query (must match the
#'   model used at index time). Default `"nomic-embed-text"`.
#' @param ollama_url  Ollama API base URL.
#'
#' @return A [tibble][tibble::tibble] of matching nodes, ordered by relevance,
#'   with columns `name`, `type`, `file`, `line`, `label`, `shape_info`, and
#'   optionally `similarity` when vector search is used.
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
                                    type        = NULL,
                                    embed_model = "nomic-embed-text",
                                    ollama_url  = "http://localhost:11434") {
  .require_duckdb()

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  idx_path    <- .index_path(project_dir)

  if (!file.exists(idx_path)) {
    stop("No index found at '", idx_path, "'. Run explicar_index_build() first.")
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = idx_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Try vector search if embeddings are present
  if (.has_embeddings(con) &&
      requireNamespace("httr2", quietly = TRUE) &&
      ollama_available(embed_model, ollama_url)) {
    q_vec <- .ollama_embed(query, embed_model, ollama_url)
    if (!is.null(q_vec)) {
      return(.vector_search(con, q_vec, top_k, type))
    }
  }

  # Fallback: keyword search
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
    CREATE TABLE IF NOT EXISTS verbs (
      file       VARCHAR,
      line       INTEGER,
      fn_name    VARCHAR,
      input_var  VARCHAR,
      output_var VARCHAR,
      pkg        VARCHAR
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

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS doc_embeddings (
      doc_id    VARCHAR PRIMARY KEY,
      embedding FLOAT[]
    )
  ")

  invisible(con)
}

.ensure_embeddings_table <- function(con, dim) {
  if (!DBI::dbExistsTable(con, "embeddings")) {
    DBI::dbExecute(con, sprintf("
      CREATE TABLE embeddings (
        name      VARCHAR,
        file      VARCHAR,
        embedding FLOAT[%d],
        PRIMARY KEY (name, file)
      )
    ", as.integer(dim)))
  }
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
  quoted <- paste0("'", gsub("'", "''", files), "'", collapse = ", ")
  DBI::dbExecute(con, paste0("DELETE FROM nodes       WHERE file IN (", quoted, ")"))
  DBI::dbExecute(con, paste0("DELETE FROM verbs       WHERE file IN (", quoted, ")"))
  DBI::dbExecute(con, paste0("DELETE FROM file_mtimes WHERE file IN (", quoted, ")"))
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

.insert_verbs <- function(con, verbs) {
  if (nrow(verbs) == 0L) return(invisible())
  # Drop the list-column 'args' — not DuckDB-serialisable without extra work
  df <- data.frame(
    file       = as.character(verbs$file),
    line       = as.integer(verbs$line),
    fn_name    = as.character(verbs$fn_name),
    input_var  = as.character(verbs$input_var),
    output_var = as.character(verbs$output_var),
    pkg        = as.character(verbs$pkg),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "verbs", df, append = TRUE)
}

.update_mtimes <- function(con, files) {
  if (length(files) == 0L) return(invisible())
  df <- data.frame(
    file  = files,
    mtime = as.numeric(file.info(files)$mtime),
    stringsAsFactors = FALSE
  )
  quoted <- paste0("'", gsub("'", "''", files), "'", collapse = ", ")
  DBI::dbExecute(con, paste0("DELETE FROM file_mtimes WHERE file IN (", quoted, ")"))
  DBI::dbWriteTable(con, "file_mtimes", df, append = TRUE)
}

# ---------------------------------------------------------------------------
# Embedding helpers
# ---------------------------------------------------------------------------

.has_embeddings <- function(con) {
  DBI::dbExistsTable(con, "embeddings") &&
    nrow(DBI::dbGetQuery(con, "SELECT 1 FROM embeddings LIMIT 1")) > 0L
}

.embed_nodes <- function(con, model, ollama_url, quiet) {
  # Find nodes that don't yet have embeddings
  candidates <- DBI::dbGetQuery(con, "
    SELECT n.name, n.file, n.type, n.label
    FROM nodes n
    LEFT JOIN embeddings e ON n.name = e.name AND n.file = e.file
    WHERE e.name IS NULL
  ")

  if (nrow(candidates) == 0L) {
    if (!quiet) message("All nodes are already embedded.")
    return(invisible())
  }

  texts  <- paste0(candidates$type, ": ", candidates$name, ". ",
                   ifelse(is.na(candidates$label), "", candidates$label))
  total  <- nrow(candidates)
  failed <- 0L

  for (i in seq_len(total)) {
    emb <- .ollama_embed(texts[[i]], model, ollama_url)
    if (is.null(emb)) {
      failed <- failed + 1L
      next
    }

    # Create table on first successful embedding (dimension is model-specific)
    if (!DBI::dbExistsTable(con, "embeddings")) {
      .ensure_embeddings_table(con, length(emb))
    }

    emb_literal <- paste0("[", paste(emb, collapse = ","), "]")
    dim_type    <- sprintf("FLOAT[%d]", length(emb))
    DBI::dbExecute(con, sprintf(
      "INSERT OR REPLACE INTO embeddings VALUES ('%s', '%s', %s::%s)",
      gsub("'", "''", candidates$name[[i]]),
      gsub("'", "''", candidates$file[[i]]),
      emb_literal,
      dim_type
    ))

    if (!quiet && i %% 20L == 0L) {
      message("  Embedded ", i, " / ", total)
    }
  }

  if (!quiet) {
    message("Embeddings complete: ", total - failed, " stored, ", failed, " skipped.")
  }
  invisible()
}

#' @keywords internal
.ollama_embed <- function(text, model, ollama_url) {
  tryCatch({
    resp <- httr2::request(paste0(ollama_url, "/api/embed")) |>
      httr2::req_body_json(list(model = model, input = text)) |>
      httr2::req_timeout(30L) |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    unlist(body$embeddings[[1L]])
  }, error = function(e) NULL)
}

# ---------------------------------------------------------------------------
# Search helpers
# ---------------------------------------------------------------------------

.type_clause <- function(type) {
  if (is.null(type)) "" else paste0(" AND type = ", DBI::dbQuoteString(DBI::ANSI(), type))
}

.keyword_search <- function(con, query, top_k, type) {
  like_pat <- paste0("%", gsub("'", "''", query), "%")
  sql <- paste0(
    "SELECT name, type, file, line, label, shape_info FROM nodes",
    " WHERE (name ILIKE '", like_pat, "' OR label ILIKE '", like_pat, "')",
    .type_clause(type),
    " ORDER BY length(name) ASC",   # shorter names first as proxy for relevance
    " LIMIT ", as.integer(top_k)
  )
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

.vector_search <- function(con, query_vec, top_k, type) {
  dim      <- length(query_vec)
  vec_lit  <- paste0("[", paste(query_vec, collapse = ","), "]")
  dim_type <- sprintf("FLOAT[%d]", dim)

  sql <- paste0(
    "SELECT n.name, n.type, n.file, n.line, n.label, n.shape_info,",
    "  (1 - array_cosine_distance(e.embedding, ", vec_lit, "::", dim_type, ")) AS similarity",
    " FROM nodes n",
    " JOIN embeddings e ON n.name = e.name AND n.file = e.file",
    " WHERE array_length(e.embedding) = ", dim,
    .type_clause(type),
    " ORDER BY similarity DESC",
    " LIMIT ", as.integer(top_k)
  )
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
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

