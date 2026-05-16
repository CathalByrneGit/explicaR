#' Build a Mermaid flowchart from a parse result
#'
#' Returns a Mermaid `flowchart` diagram as a plain character string.
#' Embed it in HTML with [generate_viewer()] or paste into any Mermaid
#' renderer (GitHub, Quarto, Obsidian, etc.).
#'
#' @param parse_result Output from [explicar_parse()].
#' @param direction One of `"TD"` (top-down, default), `"LR"` (left-right),
#'   `"BT"` (bottom-top), or `"RL"` (right-left).
#' @param max_label Integer; truncate long node labels to this many characters.
#'   Default `40L`.
#' @param level `"files"` (default) shows only script nodes with file-to-file
#'   data-flow edges — readable for any project size.  `"all"` shows every
#'   function, variable, and source node.
#'
#' @return A single character string containing Mermaid flowchart syntax.
#' @export
#'
#' @examples
#' \dontrun{
#' pr  <- explicar_parse("path/to/project")
#' cat(explicar_graph(pr))               # file-level (default)
#' cat(explicar_graph(pr, level = "all")) # full graph
#' }
explicar_graph <- function(parse_result,
                           direction = c("TD", "LR", "BT", "RL"),
                           max_label = 40L,
                           level     = c("files", "all")) {
  direction <- match.arg(direction)
  level     <- match.arg(level)

  nodes <- parse_result$nodes
  edges <- parse_result$edges

  # File-level view: collapse to script nodes with synthesised data-flow edges
  if (level == "files") {
    nodes <- dplyr::filter(nodes, .data$type == "script")
    # Derive script→script edges: script A produces var V, script B consumes V
    prod_raw  <- edges[edges$type == "produces", , drop = FALSE]
    cons_raw  <- edges[edges$type == "consumes", , drop = FALSE]
    producers <- data.frame(script_from = prod_raw$from, var = prod_raw$to,
                            stringsAsFactors = FALSE)
    consumers <- data.frame(var = cons_raw$from, script_to = cons_raw$to,
                            stringsAsFactors = FALSE)
    shared <- dplyr::inner_join(producers, consumers, by = "var")
    if (nrow(shared) > 0L) {
      s2s <- unique(data.frame(
        from = shared$script_from, to = shared$script_to,
        type = "feeds", stringsAsFactors = FALSE
      ))
      s2s <- s2s[s2s$from != s2s$to &
                   s2s$from %in% nodes$name & s2s$to %in% nodes$name, ]
    } else {
      s2s <- data.frame(from = character(), to = character(), type = character(),
                        stringsAsFactors = FALSE)
    }
    edges <- s2s
  }

  if (nrow(nodes) == 0L) {
    return(paste0("flowchart ", direction, "\n  empty[No nodes found]"))
  }

  lines <- c(
    paste0("flowchart ", direction),
    "  classDef script   fill:#4A90D9,stroke:#2c5f8a,color:#fff,font-weight:bold",
    "  classDef variable fill:#7ED321,stroke:#4d8214,color:#fff",
    "  classDef fn       fill:#F5A623,stroke:#c4841b,color:#000",
    "  classDef source   fill:#9B59B6,stroke:#6c3d80,color:#fff"
  )

  # Node definitions
  mids <- .mermaid_id(nodes$name)
  for (i in seq_len(nrow(nodes))) {
    node  <- nodes[i, ]
    mid   <- mids[i]
    label <- if (!is.na(node$label) && nzchar(node$label)) node$label else node$name
    if (!is.na(node$shape_info) && nzchar(node$shape_info)) {
      label <- paste0(label, "\n", node$shape_info)
    }
    if (nchar(label) > max_label) {
      label <- paste0(substr(label, 1L, max_label - 1L), "\u2026")
    }
    label <- .mermaid_escape(label)

    shape_str <- switch(node$type,
      script     = paste0('["', label, '"]'),
      variable   = paste0('("', label, '")'),
      `function` = paste0('{{"', label, '"}}'),
      source     = paste0('[("', label, '")]'),
      paste0('["', label, '"]')
    )
    class_str <- switch(node$type,
      script     = ":::script",
      variable   = ":::variable",
      `function` = ":::fn",
      source     = ":::source",
      ""
    )

    lines <- c(lines,
      paste0("  ", mid, shape_str, class_str),
      paste0("  click ", mid, " explicarNodeClick")
    )
  }

  # Edge definitions
  valid_mids <- mids
  from_mids  <- .mermaid_id(edges$from)
  to_mids    <- .mermaid_id(edges$to)

  for (i in seq_len(nrow(edges))) {
    if (!from_mids[i] %in% valid_mids || !to_mids[i] %in% valid_mids) next

    edge_label <- switch(edges$type[i],
      produces = "produces",
      consumes = "uses",
      calls    = "calls",
      depends  = "depends on",
      reads    = "reads",
      writes   = "writes",
      feeds    = "feeds",
      edges$type[i]
    )
    arrow <- if (edges$type[i] == "calls") "-.->" else "-->"
    lines <- c(lines,
      paste0("  ", from_mids[i], " ", arrow, "|", edge_label, "| ", to_mids[i])
    )
  }

  paste(lines, collapse = "\n")
}


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Sanitise a node name to a collision-resistant Mermaid identifier
#' @noRd
.mermaid_id <- function(x) {
  # Replace non-alphanumeric chars then append a short hash to prevent
  # collisions between names that differ only in punctuation (e.g. foo-bar / foo_bar)
  base <- gsub("[^A-Za-z0-9]", "_", x)
  hash <- substr(vapply(x, rlang::hash, character(1L)), 1L, 6L)
  id   <- paste0(base, "_", hash)
  ifelse(grepl("^[A-Za-z]", id), id, paste0("n", id))
}

#' Escape characters that would break Mermaid label syntax
#' @noRd
.mermaid_escape <- function(x) {
  # Double-quotes inside quoted labels must become single quotes
  gsub('"', "'", x, fixed = TRUE)
}

#' Build the reverse mapping: Mermaid ID → original node name
#'
#' Used by the viewer JS to look up clicked nodes.
#' @noRd
.mermaid_id_map <- function(nodes) {
  mids <- .mermaid_id(nodes$name)
  # When multiple names map to the same ID, last one wins; duplicates are rare
  setNames(as.list(nodes$name), mids)
}
