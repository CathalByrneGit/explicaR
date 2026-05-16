# ── Tier-2 parser: treesitter R package ──────────────────────────────────────
# Uses the `treesitter` + `treesitter.r` packages for concrete syntax tree
# parsing.  Falls back to getParseData() on error.

#' Parse scripts using tree-sitter
#' @noRd
.parse_treesitter <- function(scripts) {
  lang   <- treesitter.r::language()
  parser <- treesitter::parser(lang)

  all_nodes      <- list()
  all_edges      <- list()
  script_outputs <- list()
  trees          <- list()

  for (script in scripts) {
    script_name <- basename(script)

    all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
      name = script_name, type = "script", file = script,
      line = NA_integer_, label = script_name, shape_info = NA_character_
    )

    tryCatch({
      src  <- paste(readLines(script, warn = FALSE), collapse = "\n")
      tree <- treesitter::parser_parse(parser, src)
      trees[[script]] <- list(tree = tree, src = src)
      root <- treesitter::tree_root_node(tree)

      # Left-hand side assignments via tree-sitter query
      lhs_q  <- treesitter::query(lang,
        "(left_assignment name: (identifier) @lhs)")
      matches <- treesitter::query_matches(lhs_q, root)
      outputs <- character(0L)

      for (m in matches) {
        node    <- m$captures[["lhs"]][[1]]
        varname <- treesitter::node_text(node, src)
        start   <- treesitter::node_start_point(node)
        line    <- start[["row"]] + 1L
        outputs <- c(outputs, varname)

        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = varname, type = "variable", file = script,
          line = line, label = varname, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = varname, type = "produces"
        )
      }
      script_outputs[[script]] <- outputs

      # Function calls
      call_q   <- treesitter::query(lang, "(call function: (identifier) @fn)")
      fn_matches <- treesitter::query_matches(call_q, root)
      fn_names <- unique(vapply(fn_matches, function(m) {
        treesitter::node_text(m$captures[["fn"]][[1]], src)
      }, character(1L)))

      for (fn in fn_names) {
        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = fn, type = "function", file = script,
          line = NA_integer_, label = fn, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = fn, type = "calls"
        )
      }
    }, error = function(e) {
      message("treesitter parse failed for: ", script_name,
              " \u2014 ", conditionMessage(e))
    })
  }

  # Cross-script consumes edges
  all_output_vars <- unique(unlist(script_outputs, use.names = FALSE))

  for (script in scripts) {
    info <- trees[[script]]
    if (is.null(info)) next
    script_name   <- basename(script)
    local_outputs <- script_outputs[[script]] %||% character(0L)
    root <- treesitter::tree_root_node(info$tree)

    id_q    <- treesitter::query(treesitter.r::language(),
      "(identifier) @sym")
    matches <- treesitter::query_matches(id_q, root)
    reads   <- unique(vapply(matches, function(m) {
      treesitter::node_text(m$captures[["sym"]][[1]], info$src)
    }, character(1L)))

    reads <- reads[reads %in% all_output_vars & !reads %in% local_outputs]
    for (v in reads) {
      all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
        from = v, to = script_name, type = "consumes"
      )
    }
  }

  list(
    nodes = dplyr::bind_rows(all_nodes) |> dplyr::distinct(name, .keep_all = TRUE),
    edges = dplyr::bind_rows(all_edges) |> dplyr::distinct(),
    verbs = .extract_verbs_all(scripts)
  )
}
