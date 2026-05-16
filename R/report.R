#' Generate the explicaR pipeline viewer
#'
#' Main entry point.  Parses the project, optionally builds LLM wiki pages and
#' ingests them into the ragnar store, then writes a self-contained HTML viewer.
#'
#' @param project_dir Path to the project (local directory or future: remote URL).
#'   Alias `project_path` also accepted.
#' @param output_file Output HTML path.  Defaults to
#'   `<project_dir>/explicar_viewer.html`.
#' @param output_dir Directory for all output files (overrides `output_file` base
#'   directory when set).
#' @param title Page title / `pkg_name`. Defaults to
#'   `"explicaR — <project name>"`.
#' @param pkg_name Alias for `title`.
#' @param languages Languages to parse: `c("r")` (default), `c("r","python")`.
#' @param snapshots Optional named list of intermediate dataframes (from
#'   [with_pipeline_trace()] or [explicar_targets()]).
#' @param llm Logical; generate LLM wiki pages (requires ellmer + Ollama or
#'   an explicit `llm_chat`). Default `FALSE`.
#' @param llm_chat An `ellmer::Chat` object.  When supplied and `llm = TRUE`
#'   (or when `llm_chat` is non-NULL), wiki pages are generated using it.
#' @param enrich Logical; enrich undocumented function-node labels via Ollama
#'   (uses the lightweight httr2 path, not ellmer). Default `FALSE`.
#' @param llm_model Ollama model for `enrich = TRUE`.
#' @param embed Logical or `NULL`; embed wiki chunks into the ragnar store.
#'   `NULL` (default) = embed only when Ollama is available.
#' @param embed_model Ollama embedding model.
#' @param ingest Logical; ingest wiki + docs into ragnar after wiki build.
#'   Ignored when `llm = FALSE`. Default `TRUE`.
#' @param direction Mermaid graph direction: `"TD"` (default), `"LR"`, etc.
#' @param open Open the viewer in the browser. Default `TRUE`.
#' @param db_extensions Logical; reserved for future DuckDB extension support.
#'   Default `FALSE`.
#'
#' @return Invisibly, the path to the generated HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Minimal — just the graph
#' explicar("path/to/project")
#'
#' # With LLM wiki (Ollama running locally)
#' explicar("path/to/project", llm = TRUE)
#'
#' # Bring-your-own chat (any ellmer provider)
#' library(ellmer)
#' chat <- chat_openai(model = "gpt-4o-mini")
#' explicar("path/to/project", llm_chat = chat)
#'
#' # Mixed R + Python project
#' explicar("path/to/project", languages = c("r", "python"))
#' }
explicar <- function(project_dir  = ".",
                     output_file  = NULL,
                     output_dir   = NULL,
                     title        = NULL,
                     pkg_name     = NULL,
                     languages    = "r",
                     snapshots    = NULL,
                     llm          = FALSE,
                     llm_chat     = NULL,
                     enrich       = FALSE,
                     llm_model    = "qwen2.5-coder:3b",
                     embed        = NULL,
                     embed_model  = "nomic-embed-text",
                     ingest       = TRUE,
                     direction    = "TD",
                     open         = TRUE,
                     db_extensions = FALSE,
                     # Legacy aliases
                     project_path = NULL) {

  # Alias support
  if (!is.null(project_path)) project_dir <- project_path
  if (!is.null(pkg_name) && is.null(title)) title <- pkg_name

  project_dir <- normalizePath(project_dir, mustWork = TRUE)

  if (is.null(title)) {
    title <- paste0("explicaR — ", basename(project_dir))
  }

  # Resolve output path
  if (is.null(output_file)) {
    out_base   <- output_dir %||% project_dir
    output_file <- file.path(out_base, "explicar_viewer.html")
  }

  message("explicaR: parsing ", project_dir)

  mode <- explicar_mode(project_dir)
  message("explicaR: mode = ", mode)

  if (mode == "targets") {
    message("explicaR: reading targets network")
    tnet         <- targets_network(project_dir)
    parse_result <- explicar_parse(project_dir, languages = languages)
    parse_result$nodes <- tnet$nodes
    parse_result$edges <- tnet$edges
    if (is.null(snapshots)) snapshots <- shapes_from_targets(project_dir)
  } else {
    parse_result <- explicar_parse(project_dir, languages = languages)
  }

  if (!is.null(snapshots) && length(snapshots) > 0L) {
    parse_result <- attach_shapes(parse_result, snapshots)
  }

  if (enrich) {
    parse_result <- .enrich_parse_result(parse_result, model = llm_model)
  }

  # ── LLM wiki generation ────────────────────────────────────────────────────
  do_wiki <- isTRUE(llm) || !is.null(llm_chat)
  if (do_wiki) {
    message("explicaR: generating wiki pages")
    tryCatch(
      explicar_wiki_build(
        project_dir = project_dir,
        llm_chat    = llm_chat,
        languages   = languages,
        quiet       = FALSE
      ),
      error = function(e) message("explicaR: wiki build failed: ", conditionMessage(e))
    )

    # ── Ragnar ingest ──────────────────────────────────────────────────────
    if (ingest && requireNamespace("ragnar", quietly = TRUE)) {
      message("explicaR: ingesting into ragnar store")
      do_embed <- if (is.null(embed)) {
        requireNamespace("httr2", quietly = TRUE) &&
          ollama_available(embed_model, "http://localhost:11434")
      } else isTRUE(embed)

      tryCatch(
        explicar_ingest(
          project_dir = project_dir,
          embed       = do_embed,
          embed_model = embed_model,
          quiet       = FALSE
        ),
        error = function(e) message("explicaR: ingest failed: ", conditionMessage(e))
      )
    }
  }

  # ── Read wiki data for embedding in viewer ─────────────────────────────────
  db_path   <- .explicar_db_path(project_dir)
  wiki_data <- tryCatch(.server_read_wiki(db_path), error = function(e) list())

  generate_viewer(
    parse_result = parse_result,
    title        = title,
    output_file  = output_file,
    snapshots    = snapshots,
    wiki_data    = wiki_data,
    direction    = direction,
    open         = open,
    quiet        = FALSE
  )
}


# ── Backward-compat wrappers ───────────────────────────────────────────────────

#' Build the pipeline graph (deprecated wrapper)
#' @description Thin wrapper around [explicar_graph()].
#' @inheritParams explicar_graph
#' @return A Mermaid flowchart string.
#' @export
explicar_report <- function(parse_result, ...) {
  explicar_graph(parse_result, ...)
}

#' @noRd
.enrich_parse_result <- function(parse_result, model) {
  enrich_parse_result(parse_result, model = model, quiet = FALSE)
}
