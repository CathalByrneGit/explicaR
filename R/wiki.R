# ── Wiki generation (v0.3) ────────────────────────────────────────────────────
#
# Generates one markdown wiki page per source file via an ellmer LLM.
# Roxygen-extraction fallback is used when no LLM is available.
# Change detection: files are skipped when their mtime hasn't changed.
# Results are stored in the `wiki` table of `.explicar/explicar.duckdb`.

#' Generate LLM wiki pages for project source files
#'
#' Produces one markdown wiki page per R or Python source file.  Pages are
#' stored in the `wiki` table of `.explicar/explicar.duckdb` and can be
#' ingested into the ragnar RAG store via [explicar_ingest()].
#'
#' Each page contains: Overview, Key Functions / Objects, How It Works, Usage
#' Example, and a per-file Mermaid dependency diagram.
#'
#' **Change detection**: files whose `mtime` matches the stored
#' `last_modified` are skipped automatically.  Pass `force = TRUE` to
#' regenerate all pages.
#'
#' **Fallback**: when no LLM is available (or when ellmer is not installed),
#' pages are built from roxygen `#'` comments and the parse graph.
#'
#' @param project_dir Project directory. Default `"."`.
#' @param db_path Path to the explicar DuckDB. Defaults to
#'   `.explicar/explicar.duckdb`.
#' @param llm_chat An `ellmer::Chat` object.  When `NULL`, a default
#'   `chat_ollama(model)` is attempted; if Ollama is unavailable the roxygen
#'   fallback is used.
#' @param model Ollama model name used when `llm_chat` is `NULL`.
#' @param ollama_url Ollama API base URL.
#' @param languages Languages to process. Default `c("r", "python")`.
#' @param force Regenerate even for unchanged files. Default `FALSE`.
#' @param quiet Suppress progress messages. Default `FALSE`.
#'
#' @return Invisibly, the number of pages generated (not skipped).
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-detect Ollama
#' explicar_wiki_build("path/to/project")
#'
#' # Bring your own ellmer chat (any provider)
#' library(ellmer)
#' chat <- chat_openai(model = "gpt-4o-mini")
#' explicar_wiki_build("path/to/project", llm_chat = chat)
#'
#' # Roxygen fallback only (no LLM)
#' explicar_wiki_build("path/to/project", llm_chat = FALSE)
#' }
explicar_wiki_build <- function(project_dir = ".",
                                db_path     = NULL,
                                llm_chat    = NULL,
                                model       = "llama3.2",
                                ollama_url  = "http://localhost:11434",
                                languages   = c("r", "python"),
                                force       = FALSE,
                                quiet       = FALSE) {
  .require_duckdb()
  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  db_path     <- db_path %||% .explicar_db_path(project_dir)

  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .ensure_wiki_table(con)

  # Resolve LLM: NULL = auto, FALSE = fallback only
  use_llm  <- !isFALSE(llm_chat)
  if (is.null(llm_chat)) llm_chat <- .wiki_auto_chat(model, ollama_url, quiet)
  if (is.null(llm_chat)) use_llm  <- FALSE

  # Discover scripts
  lang_pats <- c(r = "\\.R$", python = "\\.py$")
  pats      <- lang_pats[names(lang_pats) %in% tolower(languages)]
  pattern   <- paste(pats, collapse = "|")

  scripts <- list.files(project_dir, pattern = pattern,
                        full.names = TRUE, recursive = FALSE)
  scripts <- scripts[!grepl("/.explicar/|/.git/|/renv/|/packrat/", scripts)]

  if (length(scripts) == 0L) {
    if (!quiet) message("No scripts found in: ", project_dir)
    return(invisible(0L))
  }

  parse_result <- tryCatch(
    explicar_parse(project_dir, languages = languages),
    error = function(e) .empty_parse_result()
  )

  n_gen  <- 0L
  n_skip <- 0L
  model_label <- if (use_llm && !is.null(llm_chat)) model else "fallback"

  for (script in scripts) {
    script_name <- basename(script)
    file_mtime  <- as.numeric(file.info(script)$mtime)

    if (!force && .wiki_is_current(con, script, file_mtime)) {
      n_skip <- n_skip + 1L
      if (!quiet) message("  skip (unchanged): ", script_name)
      next
    }

    if (!quiet) message("  generating: ", script_name,
                        if (use_llm) "" else " [fallback]")

    content <- if (use_llm) {
      tryCatch(
        .wiki_generate_page(script, script_name, parse_result, llm_chat),
        error = function(e) {
          if (!quiet) message("    LLM failed — using fallback: ", conditionMessage(e))
          .wiki_fallback_page(script, script_name, parse_result)
        }
      )
    } else {
      .wiki_fallback_page(script, script_name, parse_result)
    }

    .wiki_upsert(con, script, model_label, file_mtime, content)
    n_gen <- n_gen + 1L
  }

  if (!quiet) message("Wiki complete: ", n_gen, " generated, ", n_skip, " unchanged")
  invisible(n_gen)
}


#' Multi-turn iterative research using an ellmer Chat
#'
#' Runs up to `max_iterations` turns of a research conversation.  On each
#' turn the LLM may call any retrieval tools registered on the chat (e.g.
#' via `ragnar::ragnar_register_tool_retrieve()`).  The loop terminates
#' early when the response begins with `CONCLUSION:`.
#'
#' @param chat An `ellmer::Chat` object, typically with a retrieval tool
#'   already wired via [ragnar::ragnar_register_tool_retrieve()].
#' @param question The research question.
#' @param max_iterations Maximum number of LLM turns (including the initial
#'   planning turn). Default `5L`.
#'
#' @return Invisibly, the updated `chat` object (full history is preserved).
#' @export
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' library(ragnar)
#' store <- ragnar_store_connect(".explicar/explicar.duckdb")
#' chat  <- chat_ollama(model = "llama3.2")
#' ragnar_register_tool_retrieve(chat, store)
#' deep_research(chat, "How does the parse dispatch layer work?")
#' cat(chat$last_turn()$text)
#' }
deep_research <- function(chat, question, max_iterations = 5L) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required. Install with: install.packages('ellmer')",
         call. = FALSE)
  }

  chat$chat(paste0(
    "You are researching a codebase to answer this question:\n\n",
    question,
    "\n\nFirst, create a research plan listing the key things to investigate."
  ))

  for (i in seq_len(max(0L, as.integer(max_iterations) - 1L)) ) {
    result <- chat$chat(paste0(
      "Continue your research (iteration ", i, " of ",
      max_iterations - 1L, "). ",
      "Use any available retrieval tools to find relevant information. ",
      "When you have enough information to fully answer the question, ",
      "start your response with exactly: CONCLUSION:"
    ))
    if (grepl("^CONCLUSION:", trimws(result))) break
  }

  invisible(chat)
}


# ── Page generation ────────────────────────────────────────────────────────────

.wiki_generate_page <- function(script, script_name, parse_result, llm_chat) {
  code <- paste(readLines(script, warn = FALSE), collapse = "\n")
  if (nchar(code) > 5000L) code <- paste0(substr(code, 1L, 5000L), "\n... [truncated]")

  lang <- if (grepl("\\.py$", script_name)) "Python" else "R"

  prompt <- paste0(
    "You are documenting ", lang, " source code for a developer wiki.\n",
    "Generate a concise markdown wiki page for `", script_name, "` ",
    "with exactly these four sections:\n\n",
    "## Overview\n",
    "2-3 sentences: what this file does and its role in the project.\n\n",
    "## Key Functions / Objects\n",
    "Bullet list: - `name()` — one-line description per item.\n\n",
    "## How It Works\n",
    "2-4 sentences on the implementation approach or key patterns.\n\n",
    "## Usage Example\n",
    "A short ", lang, " code block showing typical usage.\n\n",
    "Rules: under 400 words total. Stick to facts visible in the code. ",
    "Do not repeat the filename as a top-level heading.\n\n",
    "FILE: ", script_name, "\n",
    "LANGUAGE: ", lang, "\n\n",
    "```", tolower(lang), "\n", code, "\n```"
  )

  result <- llm_chat$chat(prompt)

  # Prepend per-file dependency diagram
  local_mermaid <- .wiki_local_mermaid(script_name, parse_result)
  if (nzchar(local_mermaid)) {
    result <- paste0(
      "## Dependency Graph\n\n```mermaid\n", local_mermaid, "\n```\n\n",
      result
    )
  }

  result
}

.wiki_fallback_page <- function(script, script_name, parse_result) {
  lang <- if (grepl("\\.py$", script_name)) "Python" else "R"

  file_nodes <- dplyr::filter(
    parse_result$nodes,
    .data$file == script | .data$file == script_name | .data$file == basename(script)
  )

  # Extract roxygen / docstring lines
  lines    <- readLines(script, warn = FALSE)
  roxy_pat <- if (lang == "R") "^#'" else "^\\s*\"\"\"|^\\s*#"
  roxy_lines <- sub("^#'\\s?", "", grep("^#'", lines, value = TRUE))

  sections <- paste0("## Overview\n\n")
  if (length(roxy_lines) > 0L) {
    sections <- paste0(sections, paste(head(roxy_lines, 8L), collapse = "\n"), "\n\n")
  } else {
    sections <- paste0(sections,
      "_No documentation found._",
      " Run `explicar_wiki_build()` with an LLM for enriched pages.\n\n")
  }

  fn_nodes  <- dplyr::filter(file_nodes, .data$type == "function")
  var_nodes <- dplyr::filter(file_nodes, .data$type == "variable")

  if (nrow(fn_nodes) > 0L) {
    sections <- paste0(sections, "## Key Functions\n\n")
    bullets  <- paste0(
      "- `", fn_nodes$name, "()`",
      ifelse(!is.na(fn_nodes$label) & fn_nodes$label != fn_nodes$name,
             paste0(" — ", fn_nodes$label), ""),
      collapse = "\n"
    )
    sections <- paste0(sections, bullets, "\n\n")
  }

  if (nrow(var_nodes) > 0L) {
    sections <- paste0(sections, "## Key Objects\n\n")
    bullets  <- paste0(
      "- `", var_nodes$name, "`",
      ifelse(!is.na(var_nodes$shape_info),
             paste0(" (", var_nodes$shape_info, ")"), ""),
      collapse = "\n"
    )
    sections <- paste0(sections, bullets, "\n\n")
  }

  local_mermaid <- .wiki_local_mermaid(script_name, parse_result)
  if (nzchar(local_mermaid)) {
    sections <- paste0(
      "## Dependency Graph\n\n```mermaid\n", local_mermaid, "\n```\n\n",
      sections
    )
  }

  sections
}

# Per-file Mermaid diagram (what this file depends on + what depends on it)
.wiki_local_mermaid <- function(script_name, parse_result) {
  mid <- function(x) gsub("[^A-Za-z0-9]", "_", x)

  out_edges <- dplyr::filter(parse_result$edges, .data$from == script_name)
  in_edges  <- dplyr::filter(parse_result$edges, .data$to   == script_name)

  if (nrow(out_edges) == 0L && nrow(in_edges) == 0L) return("")

  node_lines <- paste0("  ", mid(script_name), '["', script_name, '"]')

  edge_lines <- character(0L)
  all_edges  <- dplyr::bind_rows(out_edges, in_edges) |> dplyr::distinct()

  for (i in seq_len(nrow(all_edges))) {
    e     <- all_edges[i, ]
    arrow <- if (e$type %in% c("calls")) "-.->" else "-->"
    label <- paste0("|", e$type, "|")

    # Ensure both endpoint nodes appear
    for (nm in c(e$from, e$to)) {
      if (nm != script_name) {
        node_lines <- c(node_lines, paste0("  ", mid(nm), '("', nm, '")'))
      }
    }
    edge_lines <- c(edge_lines,
      paste0("  ", mid(e$from), " ", arrow, " ", label, " ", mid(e$to))
    )
  }

  paste(c("flowchart LR", unique(node_lines), edge_lines), collapse = "\n")
}


# ── LLM helpers ───────────────────────────────────────────────────────────────

.wiki_auto_chat <- function(model, ollama_url, quiet) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    if (!quiet) message("'ellmer' not installed — using roxygen fallback")
    return(NULL)
  }
  tryCatch(
    ellmer::chat_ollama(model = model, base_url = ollama_url),
    error = function(e) {
      if (!quiet) message("Ollama unavailable — using roxygen fallback")
      NULL
    }
  )
}


# ── DuckDB helpers ────────────────────────────────────────────────────────────

#' Canonical path for the unified explicar DuckDB store
#' @noRd
.explicar_db_path <- function(project_dir) {
  file.path(project_dir, ".explicar", "explicar.duckdb")
}

.ensure_wiki_table <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS wiki (
      file          VARCHAR PRIMARY KEY,
      model         VARCHAR,
      generated_at  DOUBLE,
      last_modified DOUBLE,
      content       TEXT
    )
  ")
  invisible(con)
}

.wiki_is_current <- function(con, file_path, current_mtime) {
  if (!DBI::dbExistsTable(con, "wiki")) return(FALSE)
  row <- DBI::dbGetQuery(
    con,
    sprintf("SELECT last_modified FROM wiki WHERE file = '%s'",
            gsub("'", "''", file_path))
  )
  nrow(row) > 0L && abs(row$last_modified[[1L]] - current_mtime) < 0.01
}

.wiki_upsert <- function(con, file_path, model, file_mtime, content) {
  DBI::dbExecute(
    con,
    sprintf("DELETE FROM wiki WHERE file = '%s'", gsub("'", "''", file_path))
  )
  DBI::dbWriteTable(con, "wiki",
    data.frame(file = file_path, model = model,
               generated_at  = as.numeric(Sys.time()),
               last_modified = file_mtime,
               content       = content,
               stringsAsFactors = FALSE),
    append = TRUE
  )
  invisible()
}
