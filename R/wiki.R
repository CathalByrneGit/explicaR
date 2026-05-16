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
#' library(ellmer)
#'
#' # Ollama — local, free, no key
#' explicar_wiki_build("path/to/project",
#'   llm_chat = chat_ollama(model = "llama3.2"))
#'
#' # OpenAI (reads OPENAI_API_KEY)
#' explicar_wiki_build("path/to/project",
#'   llm_chat = chat_openai(model = "gpt-4o-mini"))
#'
#' # Anthropic (reads ANTHROPIC_API_KEY)
#' explicar_wiki_build("path/to/project",
#'   llm_chat = chat_anthropic(model = "claude-haiku-4-5"))
#'
#' # Google Gemini, Groq, AWS Bedrock, llama.cpp — same pattern:
#' # chat_google_gemini() / chat_groq() / chat_aws_bedrock() /
#' # chat_openai_compatible(base_url = "http://localhost:8080")
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
          if (!quiet) message("    LLM failed \u2014 using fallback: ", conditionMessage(e))
          .wiki_fallback_page(script, script_name, parse_result)
        }
      )
    } else {
      .wiki_fallback_page(script, script_name, parse_result)
    }

    .wiki_upsert(con, script, model_label, file_mtime, content)
    .wiki_write_md(script, content, project_dir)
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


.wiki_graph_context <- function(script, script_name, parse_result) {
  nodes <- parse_result$nodes
  edges <- parse_result$edges

  # Functions DEFINED here = produced by this script via assignment
  fns  <- edges[edges$from == script_name & edges$type == "produces", "to", drop = TRUE]
  fn_nodes <- nodes[nodes$name %in% fns & nodes$type == "function", ]
  fns  <- fn_nodes$name

  # Variables produced by this script
  vars <- edges[edges$from == script_name & edges$type == "produces", "to", drop = TRUE]
  var_nodes <- nodes[nodes$name %in% vars & nodes$type == "variable", ]
  vars <- var_nodes$name

  # External functions called (not defined here)
  calls_out <- edges[edges$from == script_name & edges$type == "calls", "to",
                     drop = TRUE]
  calls_out <- setdiff(calls_out, fns)

  consumers  <- edges[edges$type == "consumes" & edges$to == script_name, "from",
                      drop = TRUE]
  upstream   <- unique(edges[edges$type == "produces" & edges$from != script_name &
                               edges$to %in% consumers, "from", drop = TRUE])

  produced   <- edges[edges$from == script_name & edges$type == "produces", "to",
                      drop = TRUE]
  downstream <- unique(setdiff(
    edges[edges$type == "consumes" & edges$from %in% produced, "to", drop = TRUE],
    script_name
  ))

  parts <- character(0L)
  if (length(fns)        > 0L) parts <- c(parts, paste0("Defines: ",            paste(paste0(fns, "()"), collapse = ", ")))
  if (length(vars)       > 0L) parts <- c(parts, paste0("Produces objects: ",   paste(vars, collapse = ", ")))
  if (length(calls_out)  > 0L) parts <- c(parts, paste0("Calls: ",              paste(head(calls_out, 15L), collapse = ", ")))
  if (length(upstream)   > 0L) parts <- c(parts, paste0("Receives data from: ", paste(upstream, collapse = ", ")))
  if (length(downstream) > 0L) parts <- c(parts, paste0("Feeds data into: ",    paste(downstream, collapse = ", ")))

  if (length(parts) == 0L) return("")
  paste(parts, collapse = "\n")
}


# ── Page generation ────────────────────────────────────────────────────────────

.wiki_generate_page <- function(script, script_name, parse_result, llm_chat) {
  code    <- paste(readLines(script, warn = FALSE), collapse = "\n")
  if (nchar(code) > 6000L) code <- paste0(substr(code, 1L, 6000L), "\n... [truncated]")
  lang    <- if (grepl("\\.py$", script_name)) "Python" else "R"
  context <- .wiki_graph_context(script, script_name, parse_result)

  prompt <- paste0(
    "You are writing a developer wiki page for `", script_name,
    "` in a ", lang, " project.\n\n",
    if (nzchar(context)) paste0(
      "PROJECT GRAPH CONTEXT (how this file relates to the rest of the codebase):\n",
      context, "\n\n"
    ),
    "Generate a markdown wiki page with these sections:\n\n",
    "## Overview\n",
    "3-4 sentences: what this file does, its role in the project, and the key ",
    "design decision or pattern it embodies (e.g. singleton, factory, pure functions).\n\n",
    "## Functions\n",
    "For each exported or important function: `name(key_params)` on its own line, ",
    "followed by 1-2 sentences on what it does, its parameters, and return value. ",
    "Group related functions together.\n\n",
    "## How It Works\n",
    "3-5 sentences on the implementation approach: what pattern is used and WHY, ",
    "what the key data structures are, how errors are handled, any non-obvious behaviour.\n\n",
    "## Usage\n",
    "A realistic ", lang, " code example showing the main use case.\n\n",
    "Rules: 500-700 words. Stick to facts visible in the code and graph context. ",
    "Do not repeat the filename as a heading. Use backticks for all names.\n\n",
    "SOURCE CODE:\n```", tolower(lang), "\n", code, "\n```"
  )

  result <- llm_chat$chat(prompt)

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
             paste0(" \u2014 ", fn_nodes$label), ""),
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
    if (!quiet) message("'ellmer' not installed \u2014 using roxygen fallback")
    return(NULL)
  }
  tryCatch(
    ellmer::chat_ollama(model = model, base_url = ollama_url),
    error = function(e) {
      if (!quiet) message("Ollama unavailable \u2014 using roxygen fallback")
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

.wiki_write_md <- function(script, content, project_dir) {
  wiki_dir <- file.path(project_dir, "explicar", "wiki")
  if (!dir.exists(wiki_dir)) dir.create(wiki_dir, recursive = TRUE)
  md_name  <- paste0(tools::file_path_sans_ext(basename(script)), ".md")
  writeLines(content, file.path(wiki_dir, md_name))
  invisible()
}
