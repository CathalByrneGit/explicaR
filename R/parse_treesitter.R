# ── Tier-2 parser: treesitter R package ──────────────────────────────────────
# Uses the `treesitter` + `treesitter.r` packages for concrete syntax tree
# parsing.  Falls back to getParseData() on error.
#
# Grammar changed in treesitter.r 1.x: assignments are `binary_operator`
# (not `left_assignment`).  API: query_captures() replaces query_matches();
# node_text(node) takes no src argument.

#' Parse scripts using tree-sitter
#' @noRd
.parse_treesitter <- function(scripts) {
  lang   <- treesitter.r::language()
  parser <- treesitter::parser(lang)

  # Pre-compile queries once
  assign_q <- treesitter::query(lang,
    '(binary_operator (identifier) @lhs "<-")')
  fn_def_q <- treesitter::query(lang,
    '(binary_operator (identifier) @fn "<-" (function_definition))')
  call_q   <- treesitter::query(lang,
    '(call function: (identifier) @fn)')
  id_q     <- treesitter::query(lang,
    '(identifier) @sym')

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
      trees[[script]] <- tree
      root <- treesitter::tree_root_node(tree)

      # Function definitions — captured first to classify correctly
      fn_caps    <- treesitter::query_captures(fn_def_q, root)
      fn_def_names <- unique(vapply(fn_caps$node, treesitter::node_text,
                                    character(1L)))

      # All <- assignments
      assign_caps <- treesitter::query_captures(assign_q, root)
      outputs     <- character(0L)

      for (node in assign_caps$node) {
        varname   <- treesitter::node_text(node)
        pt        <- treesitter::node_start_point(node)
        line      <- pt[["row"]] + 1L
        outputs   <- c(outputs, varname)
        node_type <- if (varname %in% fn_def_names) "function" else "variable"

        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = varname, type = node_type, file = script,
          line = line, label = varname, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = varname, type = "produces"
        )
      }
      script_outputs[[script]] <- outputs

      # Function calls
      call_caps <- treesitter::query_captures(call_q, root)
      fn_names  <- unique(vapply(call_caps$node, treesitter::node_text,
                                  character(1L)))

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
              " — ", conditionMessage(e))
    })
  }

  # Cross-script consumes edges
  all_output_vars <- unique(unlist(script_outputs, use.names = FALSE))

  for (script in scripts) {
    tree <- trees[[script]]
    if (is.null(tree)) next
    script_name   <- basename(script)
    local_outputs <- script_outputs[[script]] %||% character(0L)
    root          <- treesitter::tree_root_node(tree)

    id_caps <- treesitter::query_captures(id_q, root)
    reads   <- unique(vapply(id_caps$node, treesitter::node_text, character(1L)))
    reads   <- reads[reads %in% all_output_vars & !reads %in% local_outputs]

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
