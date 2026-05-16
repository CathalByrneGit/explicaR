#' Generate a self-contained HTML pipeline viewer
#'
#' Combines the Mermaid pipeline graph, node metadata, and optionally LLM
#' wiki pages into a single HTML file that works offline (except for the
#' Mermaid.js CDN request).
#'
#' @param parse_result Output from [explicar_parse()].
#' @param title Page title. Defaults to `"explicaR — <project>"`.
#' @param output_file Path for the output HTML file.
#' @param wiki_data Named list of `file → wiki_markdown` strings.  When
#'   provided (e.g. from [explicar_wiki_build()]), clicking a script node
#'   shows its wiki page in the detail panel.  Default `NULL`.
#' @param direction Mermaid graph direction: `"TD"` (default), `"LR"`, etc.
#' @param open Logical; open the file in the browser when done. Default `TRUE`.
#' @param quiet Suppress progress messages. Default `FALSE`.
#'
#' @return Invisibly, the path to the generated HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' pr <- explicar_parse("path/to/project")
#' generate_viewer(pr, output_file = "pipeline.html")
#' }
generate_viewer <- function(parse_result,
                            title       = NULL,
                            output_file = "explicar_viewer.html",
                            wiki_data   = NULL,
                            direction   = "TD",
                            open        = TRUE,
                            quiet       = FALSE) {

  if (is.null(title)) title <- "explicaR Pipeline"
  if (is.null(wiki_data)) wiki_data <- list()

  if (!quiet) message("explicaR: building Mermaid graph")
  graph_text <- explicar_graph(parse_result, direction = direction)

  # Node metadata for JS lookup
  node_data <- lapply(seq_len(nrow(parse_result$nodes)), function(i) {
    n <- parse_result$nodes[i, ]
    list(
      name       = n$name,
      type       = n$type,
      file       = if (is.na(n$file))  "" else n$file,
      line       = if (is.na(n$line))  0L else n$line,
      label      = if (is.na(n$label)) n$name else n$label,
      shape_info = if (is.na(n$shape_info)) "" else n$shape_info
    )
  })

  stats <- paste0(
    nrow(parse_result$nodes), " nodes \u00B7 ",
    nrow(parse_result$edges), " edges"
  )

  id_map <- .mermaid_id_map(parse_result$nodes)

  tmpl_path <- system.file("templates", "viewer.html", package = "explicaR")
  if (!nzchar(tmpl_path) || !file.exists(tmpl_path)) {
    stop("Viewer template not found. Reinstall the explicaR package.", call. = FALSE)
  }
  tmpl <- paste(readLines(tmpl_path, warn = FALSE), collapse = "\n")

  html <- tmpl
  html <- gsub("{{TITLE}}",          .html_esc(title),                    html, fixed = TRUE)
  html <- gsub("{{STATS}}",          .html_esc(stats),                    html, fixed = TRUE)
  html <- gsub("{{GENERATED_AT}}",   format(Sys.time(), "%Y-%m-%d %H:%M"), html, fixed = TRUE)
  html <- gsub("{{MERMAID_GRAPH}}",  graph_text,                           html, fixed = TRUE)
  html <- gsub("{{NODE_DATA_JSON}}", jsonlite::toJSON(node_data,  auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{ID_MAP_JSON}}",    jsonlite::toJSON(id_map,     auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{WIKI_DATA_JSON}}", jsonlite::toJSON(wiki_data,  auto_unbox = TRUE), html, fixed = TRUE)

  writeLines(html, output_file)
  if (!quiet) message("explicaR: viewer saved to ", output_file)

  if (open && interactive()) utils::browseURL(output_file)

  invisible(output_file)
}


# ── Helper ────────────────────────────────────────────────────────────────────

.html_esc <- function(x) {
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x
}
