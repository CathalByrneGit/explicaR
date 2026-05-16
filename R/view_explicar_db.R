# ── Tier-3 interactive server (v0.4) ─────────────────────────────────────────
#
# Serves the analytics dashboard via httpuv.
# Endpoints:
#   GET  /           → analytics.html (with embedded graph + wiki)
#   POST /chat       → JSON {message} → LLM response via ellmer + ragnar
#   POST /search     → JSON {query}   → BM25/VSS results from ragnar store
#   GET  /graph.json → current nodes + edges as JSON
#   GET  /wiki.json  → all wiki pages as JSON
#   GET  /health     → {"ok": true}

#' Launch the Tier-3 interactive analytics server
#'
#' Starts a local HTTP server serving the full analytics dashboard at
#' `http://host:port/`.  The dashboard includes:
#' - The Mermaid pipeline graph with clickable node detail
#' - LLM-generated wiki pages per source file
#' - BM25 keyword search over the ragnar RAG store
#' - LLM chat panel backed by ragnar retrieval (when `llm_chat` is supplied)
#'
#' The server blocks until interrupted (Ctrl+C).  Stop programmatically with
#' `httpuv::stopServer(srv)` on the returned object.
#'
#' @param project_dir Project to serve. Default `"."`.
#' @param db_path Explicar DuckDB path. Defaults to `.explicar/explicar.duckdb`.
#' @param llm_chat An `ellmer::Chat` object.  When `NULL`, the chat panel is
#'   disabled.  Retrieval from the ragnar store is wired automatically when
#'   the store exists.
#' @param top_k Chunks retrieved per query. Default `10L`.
#' @param port HTTP port. Default `8080L`.
#' @param host Host to bind. Default `"127.0.0.1"` (localhost only).
#' @param open Open the browser automatically. Default `TRUE`.
#'
#' @return Invisibly, the httpuv server object.  Returned only when the
#'   blocking loop is exited (e.g. via timeout or Ctrl+C in non-interactive
#'   use).
#' @export
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' # With Ollama running locally
#' view_explicar_db(llm_chat = chat_ollama(model = "llama3.2"))
#'
#' # Search only (no LLM)
#' view_explicar_db()
#' }
view_explicar_db <- function(project_dir = ".",
                              db_path    = NULL,
                              llm_chat   = NULL,
                              top_k      = 10L,
                              port       = 8080L,
                              host       = "127.0.0.1",
                              open       = TRUE) {

  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop("Package 'httpuv' is required for view_explicar_db().\n",
         "Install with: install.packages('httpuv')", call. = FALSE)
  }

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  db_path     <- db_path %||% .explicar_db_path(project_dir)

  # Wire ragnar retrieval to the chat if store exists
  store <- NULL
  if (!is.null(llm_chat) && file.exists(db_path) &&
      requireNamespace("ragnar", quietly = TRUE)) {
    tryCatch({
      store <- ragnar::ragnar_store_connect(db_path, read_only = TRUE)
      ragnar::ragnar_register_tool_retrieve(
        llm_chat, store,
        tool_description = paste(
          "Search documentation and wiki pages for the", basename(project_dir),
          "codebase. Use this to answer questions about the project's code,",
          "functions, and implementation details."
        )
      )
      message("explicaR: ragnar retrieval registered on chat")
    }, error = function(e) {
      message("explicaR: ragnar store unavailable: ", conditionMessage(e))
    })
  }

  # Pre-build analytics HTML (re-built on GET / to stay fresh on file changes)
  .build <- function() {
    pr        <- tryCatch(explicar_parse(project_dir), error = function(e) .empty_parse_result())
    wiki_data <- .server_read_wiki(db_path)
    .build_analytics_html(pr, wiki_data, project_dir, has_chat = !is.null(llm_chat))
  }

  cached_html <- .build()

  app <- list(
    call = function(req) {
      path   <- req$PATH_INFO
      method <- req$REQUEST_METHOD

      # CORS pre-flight
      if (method == "OPTIONS") {
        return(list(status = 204L,
                    headers = list("Access-Control-Allow-Origin"  = "*",
                                   "Access-Control-Allow-Methods" = "GET,POST",
                                   "Access-Control-Allow-Headers" = "Content-Type"),
                    body = ""))
      }

      if (path %in% c("/", "") && method == "GET") {
        list(status = 200L,
             headers = list("Content-Type" = "text/html; charset=utf-8"),
             body = .build())   # re-build on each request for freshness

      } else if (path == "/health" && method == "GET") {
        .srv_json(list(ok = TRUE))

      } else if (path == "/graph.json" && method == "GET") {
        pr <- tryCatch(explicar_parse(project_dir), error = function(e) .empty_parse_result())
        .srv_json(list(nodes = pr$nodes, edges = pr$edges))

      } else if (path == "/wiki.json" && method == "GET") {
        .srv_json(.server_read_wiki(db_path))

      } else if (path == "/search" && method == "POST") {
        body  <- .srv_read_body(req)
        query <- tryCatch(jsonlite::fromJSON(body)$query %||% "", error = function(e) "")

        results <- if (!is.null(store) && nzchar(trimws(query))) {
          tryCatch(
            ragnar::ragnar_retrieve(store, query, top_k = as.integer(top_k)),
            error = function(e) data.frame()
          )
        } else {
          data.frame()
        }
        .srv_json(list(results = results, query = query))

      } else if (path == "/chat" && method == "POST") {
        if (is.null(llm_chat)) {
          return(.srv_json(list(error = "No LLM configured. Pass llm_chat to view_explicar_db()."),
                           status = 400L))
        }
        body    <- .srv_read_body(req)
        msg     <- tryCatch(jsonlite::fromJSON(body)$message %||% "", error = function(e) "")
        if (!nzchar(trimws(msg))) {
          return(.srv_json(list(error = "Empty message"), status = 400L))
        }
        response <- tryCatch(llm_chat$chat(msg), error = function(e) paste("Error:", conditionMessage(e)))
        .srv_json(list(response = response))

      } else {
        list(status = 404L,
             headers = list("Content-Type" = "text/plain"),
             body = "Not found")
      }
    }
  )

  url <- paste0("http://", host, ":", port)
  message("explicaR Tier 3 running at: ", url)
  message("Endpoints: /search, /chat, /graph.json, /wiki.json")
  message("Press Ctrl+C to stop.")

  srv <- httpuv::startServer(host = host, port = as.integer(port), app = app)
  on.exit({
    httpuv::stopServer(srv)
    if (!is.null(store)) {
      tryCatch(ragnar::ragnar_store_disconnect(store), error = function(e) invisible())
    }
    message("explicaR server stopped.")
  }, add = TRUE)

  if (open) utils::browseURL(url)

  # Event loop — blocks until interrupted
  tryCatch(
    repeat { httpuv::service(timeoutMs = 1000L) },
    interrupt = function(e) invisible()
  )

  invisible(srv)
}


# ── Analytics HTML builder ─────────────────────────────────────────────────────

.build_analytics_html <- function(parse_result, wiki_data, project_dir, has_chat) {
  tmpl_path <- system.file("templates", "analytics.html", package = "explicaR")
  if (!nzchar(tmpl_path) || !file.exists(tmpl_path)) {
    # Fallback to wasm.html if analytics.html missing
    tmpl_path <- system.file("templates", "wasm.html", package = "explicaR")
  }
  if (!nzchar(tmpl_path) || !file.exists(tmpl_path)) {
    stop("No viewer template found. Reinstall explicaR.", call. = FALSE)
  }

  graph_text <- tryCatch(explicar_graph(parse_result), error = function(e) "")

  node_data <- lapply(seq_len(nrow(parse_result$nodes)), function(i) {
    n <- parse_result$nodes[i, ]
    list(name = n$name, type = n$type,
         file = if (is.na(n$file)) "" else n$file,
         line = if (is.na(n$line)) 0L else n$line,
         label = if (is.na(n$label)) n$name else n$label,
         shape_info = if (is.na(n$shape_info)) "" else n$shape_info)
  })

  edge_data <- lapply(seq_len(nrow(parse_result$edges)), function(i) {
    e <- parse_result$edges[i, ]
    list(from = e$from, to = e$to, type = e$type)
  })

  id_map <- .mermaid_id_map(parse_result$nodes)
  title  <- paste0("explicaR — ", basename(project_dir))
  stats  <- paste0(nrow(parse_result$nodes), " nodes · ",
                   nrow(parse_result$edges), " edges · ",
                   length(wiki_data), " wiki pages")

  tmpl <- paste(readLines(tmpl_path, warn = FALSE), collapse = "\n")
  html <- tmpl
  html <- gsub("{{TITLE}}",          .html_esc(title),                    html, fixed = TRUE)
  html <- gsub("{{STATS}}",          .html_esc(stats),                    html, fixed = TRUE)
  html <- gsub("{{GENERATED_AT}}",   format(Sys.time(), "%Y-%m-%d %H:%M"), html, fixed = TRUE)
  html <- gsub("{{MERMAID_GRAPH}}",  graph_text,                           html, fixed = TRUE)
  html <- gsub("{{NODE_DATA_JSON}}", jsonlite::toJSON(node_data, auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{EDGE_DATA_JSON}}", jsonlite::toJSON(edge_data, auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{WIKI_DATA_JSON}}", jsonlite::toJSON(wiki_data, auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{ID_MAP_JSON}}",    jsonlite::toJSON(id_map,    auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{HAS_CHAT}}",       tolower(as.character(has_chat)),      html, fixed = TRUE)
  # Clear any remaining unfilled placeholders
  gsub("\\{\\{[A-Z_]+\\}\\}", "", html)
}


# ── Shared server helpers ──────────────────────────────────────────────────────

.srv_json <- function(data, status = 200L) {
  list(
    status  = status,
    headers = list(
      "Content-Type"                = "application/json",
      "Access-Control-Allow-Origin" = "*"
    ),
    body = jsonlite::toJSON(data, auto_unbox = TRUE, null = "null",
                             dataframe = "rows")
  )
}

.srv_read_body <- function(req) {
  tryCatch(rawToChar(req$rook.input$read()), error = function(e) "")
}

.server_read_wiki <- function(db_path) {
  if (!file.exists(db_path)) return(list())
  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) return(list())
  tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    if (!DBI::dbExistsTable(con, "wiki")) return(list())
    rows <- DBI::dbGetQuery(con, "SELECT file, content FROM wiki WHERE content IS NOT NULL")
    setNames(as.list(rows$content), rows$file)
  }, error = function(e) list())
}
