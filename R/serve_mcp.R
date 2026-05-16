# ── MCP stdio server (v0.4) ───────────────────────────────────────────────────
#
# Exposes the explicar code graph and wiki as MCP tools so Claude Desktop
# and Claude Code can answer questions about the project directly.
#
# Transport: stdio (JSON-RPC 2.0 over stdin/stdout)
# Protocol:  MCP 2024-11-05
#
# Tools exposed:
#   search_code  — BM25/VSS search over ragnar store (falls back to keyword)
#   query_graph  — SELECT SQL against nodes/edges/wiki/verbs tables
#   get_wiki     — wiki page for a specific source file
#   list_files   — all source files + function summaries

#' Start the explicaR MCP stdio server
#'
#' Launches a [Model Context Protocol](https://modelcontextprotocol.io) server
#' over stdio so Claude Desktop / Claude Code can query the code graph and
#' wiki pages for this project.
#'
#' Add to your `claude_desktop_config.json`:
#'
#' ```json
#' {
#'   "mcpServers": {
#'     "my-project": {
#'       "command": "Rscript",
#'       "args": ["-e", "explicaR::serve_explicar_mcp('path/to/project')"]
#'     }
#'   }
#' }
#' ```
#'
#' The server exposes four tools:
#' - **`search_code`** — hybrid BM25 / keyword search over the codebase
#' - **`query_graph`** — run `SELECT` SQL against `nodes`, `edges`, `wiki`,
#'   `verbs` tables
#' - **`get_wiki`** — retrieve the LLM-generated wiki page for a file
#' - **`list_files`** — list all source files with function summaries
#'
#' @param project_dir Project to serve. Default `"."`.
#' @param db_path Explicar DuckDB path. Defaults to `.explicar/explicar.duckdb`.
#'
#' @return Called for its side effect (blocks until stdin closes).
#' @export
#'
#' @examples
#' \dontrun{
#' # Build the index first, then start the server
#' explicar_index_build("path/to/project")
#' serve_explicar_mcp("path/to/project")
#' }
serve_explicar_mcp <- function(project_dir = ".", db_path = NULL) {
  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  db_path     <- db_path %||% .explicar_db_path(project_dir)

  con <- if (file.exists(db_path) &&
             requireNamespace("duckdb", quietly = TRUE) &&
             requireNamespace("DBI", quietly = TRUE)) {
    tryCatch(
      DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE),
      error = function(e) NULL
    )
  } else NULL

  on.exit({
    if (!is.null(con)) try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }, add = TRUE)

  # ragnar store connection (optional)
  store <- if (!is.null(con) && requireNamespace("ragnar", quietly = TRUE)) {
    tryCatch(
      ragnar::ragnar_store_connect(db_path, read_only = TRUE),
      error = function(e) NULL
    )
  } else NULL
  on.exit({
    if (!is.null(store)) try(ragnar::ragnar_store_disconnect(store), silent = TRUE)
  }, add = TRUE)

  .mcp_write <- function(obj) {
    cat(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null"), "\n",
        sep = "", file = stdout())
    flush(stdout())
  }

  repeat {
    line <- tryCatch(readLines(stdin(), n = 1L, warn = FALSE), error = function(e) NULL)
    if (is.null(line) || !length(line)) break
    if (!nzchar(trimws(line))) next

    req <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(req)) next

    method <- req[["method"]] %||% ""
    id     <- req[["id"]]

    if (method == "initialize") {
      .mcp_write(list(
        jsonrpc = "2.0", id = id,
        result  = list(
          protocolVersion = "2024-11-05",
          serverInfo      = list(name = "explicaR", version = "0.1.0"),
          capabilities    = list(tools = list())
        )
      ))

    } else if (method == "notifications/initialized") {
      # no-op acknowledgement

    } else if (method == "tools/list") {
      .mcp_write(list(
        jsonrpc = "2.0", id = id,
        result  = list(tools = .mcp_tool_definitions())
      ))

    } else if (method == "tools/call") {
      tool_name <- req[["params"]][["name"]] %||% ""
      tool_args <- req[["params"]][["arguments"]] %||% list()
      result    <- .mcp_call(tool_name, tool_args, con, store, project_dir)
      .mcp_write(list(jsonrpc = "2.0", id = id, result = result))

    } else if (!is.null(id)) {
      .mcp_write(list(
        jsonrpc = "2.0", id = id,
        error   = list(code = -32601L, message = paste("Unknown method:", method))
      ))
    }
  }

  invisible()
}


# ── Tool definitions ───────────────────────────────────────────────────────────

.mcp_tool_definitions <- function() {
  list(
    list(
      name        = "search_code",
      description = paste(
        "Search the codebase for functions, files, topics, or concepts.",
        "Uses BM25 full-text search (falls back to keyword) over the",
        "wiki pages, roxygen docs, and README."
      ),
      inputSchema = list(
        type       = "object",
        properties = list(
          query = list(type = "string",
                       description = "What to search for"),
          top_k = list(type = "integer",
                       description = "Maximum results to return (default 10)")
        ),
        required = list("query")
      )
    ),
    list(
      name        = "query_graph",
      description = paste(
        "Run a SELECT SQL query against the code graph.",
        "Available tables: nodes (name, type, file, line, label, shape_info),",
        "edges (from_node, to_node, type), wiki (file, model, content),",
        "verbs (file, line, fn_name, input_var, output_var, pkg),",
        "functions (name, file, line, language, exported, signature, description).",
        "Only SELECT statements are permitted."
      ),
      inputSchema = list(
        type       = "object",
        properties = list(
          sql = list(type = "string", description = "SQL SELECT statement to execute")
        ),
        required = list("sql")
      )
    ),
    list(
      name        = "get_wiki",
      description = "Get the LLM-generated wiki documentation page for a specific source file.",
      inputSchema = list(
        type       = "object",
        properties = list(
          file = list(type = "string",
                      description = "File path or basename, e.g. 'parse.R' or 'R/parse.R'")
        ),
        required = list("file")
      )
    ),
    list(
      name        = "list_files",
      description = "List all source files in the project with their functions and wiki summaries.",
      inputSchema = list(
        type       = "object",
        properties = list()
      )
    )
  )
}


# ── Tool dispatch ──────────────────────────────────────────────────────────────

.mcp_call <- function(name, args, con, store, project_dir) {
  tryCatch(
    switch(name,
      search_code  = .mcp_search_code(args, con, store),
      query_graph  = .mcp_query_graph(args, con),
      get_wiki     = .mcp_get_wiki(args, con),
      list_files   = .mcp_list_files(con),
      stop("Unknown tool: ", name)
    ),
    error = function(e) {
      list(
        content = list(list(type = "text", text = paste("Error:", conditionMessage(e)))),
        isError = TRUE
      )
    }
  )
}

.mcp_search_code <- function(args, con, store) {
  query <- args[["query"]] %||% ""
  top_k <- as.integer(args[["top_k"]] %||% 10L)

  # Prefer ragnar BM25/VSS
  text <- if (!is.null(store)) {
    tryCatch({
      res <- ragnar::ragnar_retrieve(store, query, top_k = top_k)
      if (nrow(res) == 0L) return(.mcp_text("No results found."))
      paste(vapply(seq_len(nrow(res)), function(i) {
        r <- res[i, ]
        paste0("### ", r$page_title %||% "", "\n",
               "_Source: ", r$origin %||% r$source %||% "", "_\n\n",
               r$text %||% "")
      }, character(1L)), collapse = "\n\n---\n\n")
    }, error = function(e) NULL)
  } else NULL

  # Keyword search fallback
  if (is.null(text) && !is.null(con) && DBI::dbExistsTable(con, "nodes")) {
    rows <- tryCatch(
      .keyword_search(con, query, top_k, NULL),
      error = function(e) NULL
    )
    text <- if (!is.null(rows) && nrow(rows) > 0L) {
      paste(apply(rows, 1, function(r) {
        lbl <- if (!is.na(r[["label"]]) && r[["label"]] != r[["name"]]) paste0(" — ", r[["label"]]) else ""
        loc <- if (!is.na(r[["file"]])) paste0(" (", r[["file"]], ":", r[["line"]], ")") else ""
        paste0("- **`", r[["name"]], "`** [", r[["type"]], "]", lbl, loc)
      }), collapse = "\n")
    } else "No results found."
  }

  .mcp_text(text %||% "No results found.")
}

.mcp_query_graph <- function(args, con) {
  sql <- trimws(args[["sql"]] %||% "")
  if (!nzchar(sql)) stop("sql argument is required")

  # Safety: only SELECT
  if (!grepl("^SELECT\\b", sql, ignore.case = TRUE)) {
    stop("Only SELECT statements are permitted. Received: ", substr(sql, 1L, 60L))
  }

  if (is.null(con)) stop("No database index found. Run explicar_index_build() first.")

  rows <- DBI::dbGetQuery(con, sql)

  if (nrow(rows) == 0L) return(.mcp_text("Query returned 0 rows."))

  # Format as markdown table
  cols   <- names(rows)
  header <- paste(cols, collapse = " | ")
  sep    <- paste(rep("---", length(cols)), collapse = " | ")
  body   <- apply(rows, 1, function(r) paste(r %||% "NA", collapse = " | "))
  .mcp_text(paste(c(header, sep, body), collapse = "\n"))
}

.mcp_get_wiki <- function(args, con) {
  file_q <- args[["file"]] %||% ""
  if (!nzchar(file_q)) stop("file argument is required")

  if (is.null(con)) stop("No database. Run explicar_index_build() first.")
  if (!DBI::dbExistsTable(con, "wiki")) {
    stop("No wiki pages found. Run explicar_wiki_build() first.")
  }

  row <- DBI::dbGetQuery(
    con,
    sprintf("SELECT file, content FROM wiki WHERE file ILIKE '%%%s%%' LIMIT 1",
            gsub("'", "''", file_q))
  )

  if (nrow(row) == 0L) stop("No wiki page found matching: ", file_q)
  .mcp_text(paste0("# ", basename(row$file[[1L]]), "\n\n", row$content[[1L]]))
}

.mcp_list_files <- function(con) {
  if (is.null(con)) stop("No database. Run explicar_index_build() first.")

  if (!DBI::dbExistsTable(con, "nodes")) {
    stop("No nodes table. Run explicar_index_build() first.")
  }

  scripts <- DBI::dbGetQuery(
    con,
    "SELECT name, file FROM nodes WHERE type = 'script' ORDER BY name"
  )

  if (nrow(scripts) == 0L) return(.mcp_text("No source files found."))

  wiki_exists <- DBI::dbExistsTable(con, "wiki")

  lines <- vapply(seq_len(nrow(scripts)), function(i) {
    nm   <- scripts$name[[i]]
    path <- scripts$file[[i]]

    # Count functions defined in this file
    fn_count <- tryCatch({
      r <- DBI::dbGetQuery(
        con,
        sprintf("SELECT COUNT(*) AS n FROM nodes WHERE type='function' AND file='%s'",
                gsub("'", "''", path %||% nm))
      )
      r$n[[1L]]
    }, error = function(e) NA_integer_)

    # Wiki overview snippet
    snippet <- if (wiki_exists) {
      tryCatch({
        w <- DBI::dbGetQuery(
          con,
          sprintf("SELECT content FROM wiki WHERE file='%s' LIMIT 1",
                  gsub("'", "''", path %||% nm))
        )
        if (nrow(w) > 0L) {
          ov <- .llmstxt_section(w$content[[1L]], "Overview")
          if (nzchar(ov)) paste0("\n  > ", gsub("\n", "\n  > ", trimws(ov))) else ""
        } else ""
      }, error = function(e) "")
    } else ""

    paste0("- **", nm, "**",
           if (!is.na(fn_count) && fn_count > 0L) paste0(" (", fn_count, " functions)") else "",
           snippet)
  }, character(1L))

  .mcp_text(paste(lines, collapse = "\n\n"))
}

.mcp_text <- function(text) {
  list(content = list(list(type = "text", text = text %||% "")))
}
