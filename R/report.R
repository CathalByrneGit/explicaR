#' Generate the explicaR pipeline viewer
#'
#' Orchestrates parsing, graph generation, and HTML output. Produces a
#' self-contained HTML viewer at `output_file` (or opens it in the browser
#' when `open = TRUE`).
#'
#' @param project_dir Path to the R project directory to analyse.
#' @param output_file Path for the output HTML file. Defaults to
#'   `"explicar_viewer.html"` inside `project_dir`.
#' @param title Page title. Defaults to `"explicaR — <project name>"`.
#' @param snapshots Optional named list of intermediate dataframes (from
#'   [with_pipeline_trace()] or [explicar_targets()]). When `NULL`, illustrative
#'   data is used for before/after tables.
#' @param enrich Logical; enrich undocumented function-node labels via a local
#'   Ollama LLM (requires `httr2` and Ollama running). Default `FALSE`.
#' @param llm_model Ollama model name used when `enrich = TRUE`.
#' @param direction Mermaid graph direction: `"TD"` (default), `"LR"`, etc.
#' @param open Logical; open the viewer in the browser after generation.
#'   Default `TRUE`.
#'
#' @return Invisibly, the path to the generated HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' explicar("path/to/project")
#' }
explicar <- function(project_dir  = ".",
                     output_file  = file.path(project_dir, "explicar_viewer.html"),
                     title        = paste0("explicaR — ",
                                           basename(normalizePath(project_dir))),
                     snapshots    = NULL,
                     enrich       = FALSE,
                     llm_model    = "qwen2.5-coder:3b",
                     direction    = "TD",
                     open         = TRUE) {

  message("explicaR: parsing ", normalizePath(project_dir))

  mode <- explicar_mode(project_dir)
  message("explicaR: mode = ", mode)

  if (mode == "targets") {
    message("explicaR: reading targets network")
    tnet         <- targets_network(project_dir)
    parse_result <- explicar_parse(project_dir)
    parse_result$nodes <- tnet$nodes
    parse_result$edges <- tnet$edges
    if (is.null(snapshots)) snapshots <- shapes_from_targets(project_dir)
  } else {
    parse_result <- explicar_parse(project_dir)
  }

  if (!is.null(snapshots) && length(snapshots) > 0L) {
    parse_result <- attach_shapes(parse_result, snapshots)
  }

  if (enrich) {
    parse_result <- .enrich_parse_result(parse_result, model = llm_model)
  }

  generate_viewer(
    parse_result = parse_result,
    title        = title,
    output_file  = output_file,
    snapshots    = snapshots,
    direction    = direction,
    open         = open,
    quiet        = FALSE
  )
}


# ── Kept for backward compatibility ──────────────────────────────────────────

#' Build the pipeline graph widget (Mermaid string)
#'
#' @description
#' Deprecated thin wrapper around [explicar_graph()]. Returns the Mermaid
#' diagram text string. Use [explicar_graph()] directly.
#'
#' @inheritParams explicar_graph
#' @return A character string of Mermaid flowchart syntax.
#' @export
explicar_report <- function(parse_result, ...) {
  explicar_graph(parse_result, ...)
}


#' Enrich node labels using LLM (internal, called when enrich=TRUE)
#' @noRd
.enrich_parse_result <- function(parse_result, model) {
  enrich_parse_result(parse_result, model = model, quiet = FALSE)
}
