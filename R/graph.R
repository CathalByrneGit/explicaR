#' Build and render the macro pipeline graph
#'
#' Takes the output of [explicar_parse()] and renders an interactive
#' `visNetwork` graph showing the full pipeline DAG.
#'
#' @param parse_result Output from [explicar_parse()].
#' @param layout One of `"sugiyama"` (default, hierarchical DAG), `"fr"`
#'   (Fruchterman-Reingold), or `"kk"` (Kamada-Kawai).
#' @param width,height Widget dimensions (passed to `visNetwork`).
#'
#' @return A `visNetwork` htmlwidget.
#' @export
#'
#' @examples
#' \dontrun{
#' pr <- explicar_parse("path/to/project")
#' explicar_graph(pr)
#' }
explicar_graph <- function(parse_result,
                           layout = c("sugiyama", "fr", "kk"),
                           width  = "100%",
                           height = "600px") {
  layout <- match.arg(layout)

  nodes <- parse_result$nodes
  edges <- parse_result$edges

  if (nrow(nodes) == 0) {
    message("No nodes to render.")
    return(invisible(NULL))
  }

  # Assign visual properties
  vis_nodes <- .build_vis_nodes(nodes)
  vis_edges <- .build_vis_edges(edges, nodes)

  # Compute layout positions
  positions <- .compute_layout(vis_nodes, vis_edges, layout)
  vis_nodes <- dplyr::left_join(vis_nodes, positions, by = "id")

  # Build visNetwork widget
  net <- visNetwork::visNetwork(vis_nodes, vis_edges,
                                width = width, height = height) |>
    visNetwork::visOptions(
      highlightNearest    = list(enabled = TRUE, degree = 2, hover = TRUE),
      nodesIdSelection    = TRUE,
      selectedBy          = "group"
    ) |>
    visNetwork::visEdges(
      arrows = "to",
      smooth = list(type = "cubicBezier", forceDirection = "vertical")
    ) |>
    visNetwork::visLayout(randomSeed = 42) |>
    visNetwork::visInteraction(
      navigationButtons = TRUE,
      tooltipDelay      = 100
    ) |>
    .add_legend()

  net
}


# ── Internal helpers ─────────────────────────────────────────────────────────

# Node type → visual encoding
.node_shapes <- list(
  script   = "box",
  variable = "ellipse",
  "function" = "diamond",
  source   = "database",
  output   = "star"
)

.node_colours <- list(
  script   = "#4A90D9",
  variable = "#7ED321",
  "function" = "#F5A623",
  source   = "#9B59B6",
  output   = "#E74C3C"
)

#' Convert parsed nodes to visNetwork node dataframe
#' @noRd
.build_vis_nodes <- function(nodes) {
  nodes |>
    dplyr::mutate(
      id      = name,
      label   = dplyr::coalesce(label, name),
      # Append shape badge if present
      label   = dplyr::if_else(
        !is.na(shape_info),
        paste0(label, "\n", shape_info),
        label
      ),
      shape   = dplyr::recode(type, !!!.node_shapes, .default = "ellipse"),
      color   = dplyr::recode(type, !!!.node_colours, .default = "#95A5A6"),
      group   = type,
      title   = .build_tooltip(nodes),
      font.size = 12L,
      borderWidth = 2L
    ) |>
    dplyr::select(id, label, shape, color, group, title, font.size, borderWidth,
                  dplyr::any_of(c("x", "y", "fixed")))
}


#' Build HTML tooltip for each node
#' @noRd
.build_tooltip <- function(nodes) {
  purrr::pmap_chr(nodes, function(name, type, file, line, label, shape_info, ...) {
    parts <- c(
      paste0("<b>", htmltools::htmlEscape(name), "</b>"),
      paste0("Type: ", type)
    )
    if (!is.na(file))       parts <- c(parts, paste0("File: ", basename(file)))
    if (!is.na(line))       parts <- c(parts, paste0("Line: ", line))
    if (!is.na(shape_info)) parts <- c(parts, paste0("Shape: ", shape_info))
    paste(parts, collapse = "<br>")
  })
}


#' Convert parsed edges to visNetwork edge dataframe
#' @noRd
.build_vis_edges <- function(edges, nodes) {
  edge_colours <- c(
    produces = "#4A90D9",
    consumes = "#7ED321",
    calls    = "#F5A623",
    reads    = "#9B59B6",
    writes   = "#E74C3C"
  )
  edge_dashes <- c(
    produces = FALSE,
    consumes = FALSE,
    calls    = TRUE,
    reads    = TRUE,
    writes   = FALSE
  )

  # Only keep edges whose from/to nodes exist
  valid_names <- nodes$name
  edges |>
    dplyr::filter(from %in% valid_names, to %in% valid_names) |>
    dplyr::mutate(
      color  = dplyr::recode(type, !!!as.list(edge_colours), .default = "#95A5A6"),
      dashes = dplyr::recode(type, !!!as.list(edge_dashes),  .default = FALSE),
      title  = type,
      label  = type,
      font.size = 9L,
      font.align = "middle"
    ) |>
    dplyr::select(from, to, color, dashes, title, label, font.size, font.align)
}


#' Compute node x/y positions using igraph layout
#' @noRd
.compute_layout <- function(vis_nodes, vis_edges, layout) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    return(tibble::tibble(id = vis_nodes$id, x = NA_real_, y = NA_real_))
  }

  g <- igraph::graph_from_data_frame(
    d        = vis_edges[, c("from", "to")],
    vertices = vis_nodes[, "id", drop = FALSE],
    directed = TRUE
  )

  coords <- switch(layout,
    sugiyama = {
      lay <- igraph::layout_with_sugiyama(g)
      lay$layout
    },
    fr  = igraph::layout_with_fr(g),
    kk  = igraph::layout_with_kk(g),
    igraph::layout_with_sugiyama(g)$layout
  )

  # Scale to visNetwork coordinate space
  coords <- as.data.frame(coords)
  names(coords) <- c("x", "y")
  coords$x <- coords$x * 200
  coords$y <- coords$y * 150

  tibble::tibble(id = vis_nodes$id, x = coords$x, y = coords$y)
}


#' Add a visual legend to the visNetwork widget
#' @noRd
.add_legend <- function(net) {
  legend_nodes <- tibble::tibble(
    label = c("Script", "Variable", "Function", "Source file", "Output"),
    shape = c("box",    "ellipse",  "diamond",  "database",    "star"),
    color = c("#4A90D9","#7ED321",  "#F5A623",  "#9B59B6",     "#E74C3C")
  )
  visNetwork::visLegend(net, addNodes = legend_nodes, useGroups = FALSE,
                        position = "right", width = 0.15)
}


#' Attach data-shape information to variable nodes
#'
#' Call this after running your pipeline (or reading from targets cache) to
#' annotate variable nodes with their `nrow × ncol` badge.
#'
#' @param parse_result Output from [explicar_parse()].
#' @param shapes A named list where each element is a dataframe/tibble and
#'   the name matches the variable name in the parse result.
#'
#' @return The modified `parse_result` with `shape_info` populated on
#'   matching variable nodes.
#' @export
attach_shapes <- function(parse_result, shapes) {
  if (!is.list(shapes)) stop("`shapes` must be a named list of dataframes.")

  parse_result$nodes <- parse_result$nodes |>
    dplyr::mutate(shape_info = purrr::map_chr(name, function(nm) {
      if (nm %in% names(shapes)) {
        df <- shapes[[nm]]
        if (is.data.frame(df)) {
          return(paste0(nrow(df), " \u00d7 ", ncol(df)))
        }
      }
      NA_character_
    }))

  parse_result
}
