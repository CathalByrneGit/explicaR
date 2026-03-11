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
  parse_result <- explicar_parse(project_dir, pattern = pattern,
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

# ===========================================================================
# Documentation fetching — DeepWiki MCP integration
# ===========================================================================

#' Fetch and index documentation from DeepWiki
#'
#' Calls the [DeepWiki MCP server](https://mcp.deepwiki.com) to retrieve
#' auto-generated wiki pages for a GitHub repository, chunks them by section,
#' and stores the chunks in the `.explicar/index.duckdb` `docs` table alongside
#' the code graph.  Chunks can be embedded for semantic retrieval via
#' [explicar_index_retrieve()].
#'
#' The DeepWiki MCP server is free and requires no API key for public
#' repositories.  It exposes a `read_wiki_contents` tool that returns all wiki
#' pages as structured markdown with links back to the source repository.
#'
#' @param repo        GitHub repository in `"owner/repo"` format.  If `NULL`
#'   the repository is auto-detected from the git remote or the `URL:` field
#'   in `DESCRIPTION`.
#' @param project_dir Path to the R project directory.  Default `"."`.
#' @param embed       Embed each chunk via Ollama for vector retrieval.
#'   Default `FALSE`.
#' @param embed_model Ollama embedding model.  Default `"nomic-embed-text"`.
#' @param ollama_url  Ollama API base URL.
#' @param force       Re-fetch even if docs for this repo are already stored.
#' @param quiet       Suppress progress messages.
#'
#' @return Invisibly, the number of chunks stored.
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-detect repo from git remote, fetch DeepWiki docs
#' explicar_index_fetch_docs()
#'
#' # Specify repo explicitly and embed for semantic search
#' explicar_index_fetch_docs("CathalByrneGit/explicaR", embed = TRUE)
#'
#' # After fetching, retrieve semantically
#' explicar_index_retrieve("how does verb animation work")
#' }
explicar_index_fetch_docs <- function(repo        = NULL,
                                      project_dir = ".",
                                      embed       = FALSE,
                                      embed_model = "nomic-embed-text",
                                      ollama_url  = "http://localhost:11434",
                                      force       = FALSE,
                                      quiet       = FALSE) {
  .require_duckdb()
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required. Install with: install.packages('httr2')",
         call. = FALSE)
  }

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  idx_path    <- .index_path(project_dir)

  if (!file.exists(idx_path)) {
    if (!quiet) message("No index found; building first...")
    explicar_index_build(project_dir, quiet = quiet)
  }

  repo <- repo %||% .detect_github_repo(project_dir)
  if (is.null(repo)) {
    stop(
      "Cannot detect GitHub repository. ",
      "Provide 'repo' as \"owner/repo\" (e.g. \"CathalByrneGit/explicaR\").",
      call. = FALSE
    )
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = idx_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .ensure_schema(con)   # adds docs / doc_embeddings if not yet present

  source_id <- paste0("deepwiki:", repo)

  if (!force) {
    n_existing <- DBI::dbGetQuery(
      con,
      sprintf("SELECT COUNT(*) AS n FROM docs WHERE source = '%s'",
              gsub("'", "''", source_id))
    )$n
    if (n_existing > 0L) {
      if (!quiet) {
        message("Docs already indexed (", n_existing, " chunks). Use force=TRUE to re-fetch.")
      }
      return(invisible(n_existing))
    }
  }

  if (!quiet) message("Fetching DeepWiki docs for ", repo, " ...")

  pages <- .deepwiki_pages(repo)
  if (length(pages) == 0L) {
    if (!quiet) message("No pages returned from DeepWiki for ", repo)
    return(invisible(0L))
  }

  if (!quiet) message("  Got ", length(pages), " page(s). Chunking...")

  chunks <- .pages_to_chunks(pages, source_id, repo)
  if (nrow(chunks) == 0L) return(invisible(0L))

  # Remove old docs for this source before inserting
  DBI::dbExecute(
    con,
    sprintf("DELETE FROM docs WHERE source = '%s'", gsub("'", "''", source_id))
  )
  DBI::dbWriteTable(con, "docs", chunks, append = TRUE)

  if (!quiet) message("  Stored ", nrow(chunks), " chunk(s).")

  if (embed) {
    if (!ollama_available(embed_model, ollama_url)) {
      if (!quiet) message("Skipping embeddings: Ollama unavailable.")
    } else {
      if (!quiet) message("Embedding doc chunks...")
      .embed_doc_chunks(con, embed_model, ollama_url, quiet = quiet)
    }
  }

  invisible(nrow(chunks))
}

#' Ask a question about the repository via DeepWiki
#'
#' Sends a question to the DeepWiki MCP `ask_question` tool and returns the
#' answer as a character string.  This is a live network call — no local index
#' is required.
#'
#' @param question    The question to ask.
#' @param repo        GitHub repository in `"owner/repo"` format.  Auto-detected
#'   if `NULL`.
#' @param project_dir Path used for auto-detecting the repo.  Default `"."`.
#'
#' @return A character string with the answer, or `NA` on failure.
#' @export
#'
#' @examples
#' \dontrun{
#' explicar_ask_deepwiki("How does verb animation work?")
#' explicar_ask_deepwiki("What does explicar_parse return?")
#' }
explicar_ask_deepwiki <- function(question,
                                   repo        = NULL,
                                   project_dir = ".") {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required.", call. = FALSE)
  }

  repo <- repo %||% .detect_github_repo(normalizePath(project_dir, mustWork = TRUE))
  if (is.null(repo)) {
    stop("Cannot detect GitHub repository. Provide 'repo'.", call. = FALSE)
  }

  result <- .deepwiki_call("ask_question",
                            list(repoName = repo, question = question))
  if (is.null(result)) return(NA_character_)
  .extract_mcp_text(result)
}

# ---------------------------------------------------------------------------
# DeepWiki MCP helpers
# ---------------------------------------------------------------------------

# Returns a list of page objects: list(slug, title, depth, content)
.deepwiki_pages <- function(repo) {
  result <- .deepwiki_call("read_wiki_contents", list(repoName = repo))
  if (is.null(result)) return(list())

  text <- .extract_mcp_text(result)
  if (is.na(text) || !nchar(text)) return(list())

  # The text field is itself a JSON document with a "pages" array
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) return(list())

  parsed$pages %||% list()
}

# POST a single tools/call to the DeepWiki MCP HTTP endpoint.
# Returns the parsed JSON result object, or NULL on error.
.deepwiki_call <- function(tool, args) {
  body <- list(
    jsonrpc = "2.0",
    id      = 1L,
    method  = "tools/call",
    params  = list(name = tool, arguments = args)
  )

  tryCatch({
    resp <- httr2::request("https://mcp.deepwiki.com/mcp") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "Accept"       = "application/json, text/event-stream"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(120L) |>
      httr2::req_error(is_error = \(r) FALSE) |>   # handle HTTP errors manually
      httr2::req_perform()

    raw_body <- httr2::resp_body_string(resp)
    .parse_mcp_body(raw_body)
  }, error = function(e) {
    warning("DeepWiki MCP call failed: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}

# Parse either a plain JSON response or an SSE stream from the MCP server.
.parse_mcp_body <- function(body) {
  body <- trimws(body)
  if (!nchar(body)) return(NULL)

  # SSE: lines begin with "data:"
  if (startsWith(body, "data:") || grepl("\ndata:", body, fixed = TRUE)) {
    lines      <- strsplit(body, "\n")[[1L]]
    data_lines <- grep("^data:", lines, value = TRUE)

    # Walk events from the end; the last non-ping event is the result
    for (dl in rev(data_lines)) {
      json_text <- sub("^data: ?", "", dl)
      parsed    <- tryCatch(
        jsonlite::fromJSON(json_text, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.null(parsed) && !is.null(parsed$result)) return(parsed$result)
    }
    return(NULL)
  }

  # Plain JSON
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) NULL
  )
  parsed$result %||% NULL
}

# Extract the text string from the MCP result's content array.
.extract_mcp_text <- function(result) {
  content <- result$content %||% list()
  texts   <- vapply(content, function(c) {
    if (identical(c$type, "text")) c$text %||% "" else ""
  }, character(1L))
  text <- paste(texts[nchar(texts) > 0L], collapse = "\n")
  if (!nchar(text)) NA_character_ else text
}

# ---------------------------------------------------------------------------
# Chunking
# ---------------------------------------------------------------------------

# Convert a list of DeepWiki page objects into a data.frame ready for DuckDB.
.pages_to_chunks <- function(pages, source_id, repo, max_chars = 1200L) {
  base_url   <- paste0("https://deepwiki.com/", repo, "/")
  fetched_at <- as.numeric(Sys.time())
  rows       <- list()

  for (page in pages) {
    slug    <- page$slug    %||% "unknown"
    title   <- page$title   %||% slug
    content <- page$content %||% ""
    url     <- paste0(base_url, slug)

    chunks <- .chunk_markdown(content, title, max_chars)

    for (i in seq_along(chunks)) {
      doc_id <- paste0(source_id, "/", slug, "/", i)
      rows[[length(rows) + 1L]] <- data.frame(
        doc_id     = doc_id,
        source     = source_id,
        url        = url,
        page_title = title,
        chunk_idx  = i,
        context    = chunks[[i]]$context,
        content    = chunks[[i]]$content,
        fetched_at = fetched_at,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(doc_id = character(), source = character(),
                      url = character(), page_title = character(),
                      chunk_idx = integer(), context = character(),
                      content = character(), fetched_at = double()))
  }
  do.call(rbind, rows)
}

# Split a markdown string into chunks, staying under max_chars.
# Returns a list of list(context, content).
.chunk_markdown <- function(md, page_title, max_chars = 1200L) {
  if (!nchar(trimws(md))) return(list())

  # Split at ## / ### headings
  sections <- strsplit(md, "(?=\n#{1,3} )", perl = TRUE)[[1L]]
  sections <- Filter(function(s) nchar(trimws(s)) > 0L, sections)

  chunks <- list()

  for (sec in sections) {
    # Extract the heading as context label
    heading <- regmatches(sec, regexpr("^#+[^\n]*", sec))
    ctx     <- if (length(heading)) trimws(sub("^#+\\s*", "", heading)) else page_title
    ctx     <- if (!nchar(ctx)) page_title else paste0(page_title, " > ", ctx)

    if (nchar(sec) <= max_chars) {
      chunks <- c(chunks, list(list(context = ctx, content = trimws(sec))))
      next
    }

    # Long section: split at paragraph breaks
    paras   <- strsplit(sec, "\n{2,}")[[1L]]
    current <- ""

    for (p in paras) {
      if (nchar(current) + nchar(p) + 2L > max_chars && nchar(current) > 0L) {
        chunks  <- c(chunks, list(list(context = ctx, content = trimws(current))))
        current <- p
      } else {
        current <- if (!nchar(current)) p else paste(current, p, sep = "\n\n")
      }
    }
    if (nchar(trimws(current)) > 0L) {
      chunks <- c(chunks, list(list(context = ctx, content = trimws(current))))
    }
  }

  chunks
}

# ---------------------------------------------------------------------------
# Doc embedding helpers
# ---------------------------------------------------------------------------

.embed_doc_chunks <- function(con, model, ollama_url, quiet) {
  candidates <- DBI::dbGetQuery(con, "
    SELECT d.doc_id, d.context, d.content
    FROM docs d
    LEFT JOIN doc_embeddings e ON d.doc_id = e.doc_id
    WHERE e.doc_id IS NULL
  ")

  if (nrow(candidates) == 0L) return(invisible())

  total  <- nrow(candidates)
  failed <- 0L

  for (i in seq_len(total)) {
    text <- paste0(candidates$context[[i]], "\n\n", candidates$content[[i]])
    emb  <- .ollama_embed(text, model, ollama_url)
    if (is.null(emb)) { failed <- failed + 1L; next }

    dim_type    <- sprintf("FLOAT[%d]", length(emb))
    emb_literal <- paste0("[", paste(emb, collapse = ","), "]")
    DBI::dbExecute(con, sprintf(
      "INSERT OR REPLACE INTO doc_embeddings VALUES ('%s', %s::%s)",
      gsub("'", "''", candidates$doc_id[[i]]),
      emb_literal,
      dim_type
    ))

    if (!quiet && i %% 20L == 0L) message("  Embedded ", i, " / ", total, " doc chunks")
  }

  if (!quiet) {
    message("Doc embeddings complete: ", total - failed, " stored, ", failed, " skipped.")
  }
  invisible()
}

# ---------------------------------------------------------------------------
# Repo detection
# ---------------------------------------------------------------------------

.detect_github_repo <- function(project_dir) {
  # 1. Try git remote
  remote <- tryCatch(
    system2("git", c("-C", shQuote(project_dir), "remote", "get-url", "origin"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0L)
  )

  url <- if (length(remote) && nchar(remote[[1L]])) {
    remote[[1L]]
  } else {
    # 2. Fallback: URL field in DESCRIPTION
    desc_path <- file.path(project_dir, "DESCRIPTION")
    if (!file.exists(desc_path)) return(NULL)
    lines    <- readLines(desc_path, warn = FALSE)
    url_line <- grep("^URL:", lines, value = TRUE)
    if (!length(url_line)) return(NULL)
    trimws(strsplit(sub("^URL:\\s*", "", url_line[[1L]]), "[,\n]")[[1L]][[1L]])
  }

  m <- regmatches(url, regexpr("github\\.com[/:]([^/]+/[^/.]+)", url))
  if (!length(m)) return(NULL)
  sub("^github\\.com[/:]", "", m[[1L]])
}
