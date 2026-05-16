#' Generate a Tier-2 WASM-powered pipeline viewer
#'
#' Like [generate_viewer()] but embeds an in-browser DuckDB-WASM engine that
#' lets users run arbitrary SQL queries (e.g.
#' `SELECT * FROM nodes WHERE type = 'function'`) against the code graph
#' without any server. Requires an internet connection for DuckDB-WASM and
#' Mermaid CDN assets.
#'
#' The viewer exposes three tables:
#' - **nodes** — `name, type, file, line, label, shape_info`
#' - **edges** — `from_node, to_node, type`
#' - **verbs** — `file, line, fn_name, input_var, output_var, pkg`
#'
#' @param parse_result Output from [explicar_parse()].
#' @param title Page title. Defaults to `"explicaR — WASM Viewer"`.
#' @param output_file Path for the output HTML file.
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
#' generate_wasm_viewer(pr, output_file = "pipeline_wasm.html")
#' }
generate_wasm_viewer <- function(parse_result,
                                 title       = NULL,
                                 output_file = "explicar_wasm.html",
                                 direction   = "TD",
                                 open        = TRUE,
                                 quiet       = FALSE) {

  if (is.null(title)) title <- "explicaR \u2014 WASM Viewer"

  if (!quiet) message("explicaR: building Mermaid graph")
  graph_text <- explicar_graph(parse_result, direction = direction)

  # Serialise nodes
  node_data <- lapply(seq_len(nrow(parse_result$nodes)), function(i) {
    n <- parse_result$nodes[i, ]
    list(
      name       = n$name,
      type       = n$type,
      file       = if (is.na(n$file))       "" else n$file,
      line       = if (is.na(n$line))       0L else n$line,
      label      = if (is.na(n$label))      n$name else n$label,
      shape_info = if (is.na(n$shape_info)) "" else n$shape_info
    )
  })

  # Serialise edges (column names: from, to, type → from_node, to_node)
  edge_data <- lapply(seq_len(nrow(parse_result$edges)), function(i) {
    e <- parse_result$edges[i, ]
    list(from = e$from, to = e$to, type = e$type)
  })

  # Serialise verbs (drop list-column args — not JSON-serialisable generically)
  verb_data <- lapply(seq_len(nrow(parse_result$verbs)), function(i) {
    v <- parse_result$verbs[i, ]
    list(
      file       = if (is.na(v$file))       "" else v$file,
      line       = if (is.na(v$line))       0L else v$line,
      fn_name    = if (is.na(v$fn_name))    "" else v$fn_name,
      input_var  = if (is.na(v$input_var))  "" else v$input_var,
      output_var = if (is.na(v$output_var)) "" else v$output_var,
      pkg        = if (is.na(v$pkg))        "" else v$pkg
    )
  })

  stats <- paste0(
    nrow(parse_result$nodes), " nodes \u00B7 ",
    nrow(parse_result$edges), " edges \u00B7 ",
    nrow(parse_result$verbs), " verb calls"
  )

  id_map <- .mermaid_id_map(parse_result$nodes)

  tmpl_path <- system.file("templates", "wasm.html", package = "explicaR")
  if (!nzchar(tmpl_path) || !file.exists(tmpl_path)) {
    stop("WASM viewer template not found. Reinstall the explicaR package.", call. = FALSE)
  }
  tmpl <- paste(readLines(tmpl_path, warn = FALSE), collapse = "\n")

  html <- tmpl
  html <- gsub("{{TITLE}}",         .html_esc(title),                    html, fixed = TRUE)
  html <- gsub("{{STATS}}",         .html_esc(stats),                    html, fixed = TRUE)
  html <- gsub("{{GENERATED_AT}}",  format(Sys.time(), "%Y-%m-%d %H:%M"), html, fixed = TRUE)
  html <- gsub("{{MERMAID_GRAPH}}", graph_text,                           html, fixed = TRUE)
  html <- gsub("{{NODE_DATA_JSON}}", jsonlite::toJSON(node_data, auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{EDGE_DATA_JSON}}", jsonlite::toJSON(edge_data, auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{VERB_DATA_JSON}}", jsonlite::toJSON(verb_data, auto_unbox = TRUE), html, fixed = TRUE)
  html <- gsub("{{ID_MAP_JSON}}",    jsonlite::toJSON(id_map,    auto_unbox = TRUE), html, fixed = TRUE)

  writeLines(html, output_file)
  if (!quiet) message("explicaR: WASM viewer saved to ", output_file)

  if (open && interactive()) utils::browseURL(output_file)

  invisible(output_file)
}


#' Read explicaR file-filter configuration
#'
#' Returns the active configuration for file filtering — either from a
#' per-project `.explicar/config.yml` (if it exists and the `yaml` package is
#' installed) or from the package-level `inst/config/defaults.yml`.
#'
#' @param project_dir Project root. Default `"."`.
#'
#' @return A named list with keys `exclude_dirs`, `exclude_extensions`,
#'   `max_file_size_kb`, `default_languages`, and `max_depth`.
#' @export
#'
#' @examples
#' explicar_config()
explicar_config <- function(project_dir = ".") {
  defaults_path <- system.file("config", "defaults.yml", package = "explicaR")
  project_path  <- file.path(project_dir, ".explicar", "config.yml")

  .read_config_yml <- function(path) {
    if (!file.exists(path)) return(NULL)
    if (!requireNamespace("yaml", quietly = TRUE)) {
      message("Install the 'yaml' package to read config files.")
      return(NULL)
    }
    tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  }

  defaults <- .read_config_yml(defaults_path)
  if (is.null(defaults)) {
    defaults <- list(
      exclude_dirs       = c(".git", ".explicar", "renv", "packrat",
                             "node_modules", ".Rproj.user", "_targets"),
      exclude_extensions = character(0L),
      max_file_size_kb   = 500L,
      default_languages  = "r",
      max_depth          = NULL
    )
  }

  override <- .read_config_yml(project_path)
  if (!is.null(override)) {
    for (key in names(override)) defaults[[key]] <- override[[key]]
  }

  defaults
}
