# ── Python file parser ────────────────────────────────────────────────────────
# Two tiers:
#   Tier 2: treesitter.python (if installed)
#   Tier 3: pure-regex fallback (always available)
#
# Python pipelines differ from R: cross-script variable tracking requires
# either execution or sophisticated import analysis, so this parser focuses
# on intra-script structure (functions, top-level assignments, imports).


# ── Tier dispatch ─────────────────────────────────────────────────────────────

.parse_treesitter_python_available <- function() {
  requireNamespace("treesitter",        quietly = TRUE) &&
  requireNamespace("treesitter.python", quietly = TRUE)
}

#' Dispatch Python scripts to the best available backend
#' @noRd
.auto_dispatch_python <- function(scripts) {
  if (.parse_treesitter_python_available()) {
    result <- tryCatch(.parse_python_treesitter(scripts), error = function(e) NULL)
    if (!is.null(result)) return(result)
  }
  .parse_python_fallback(scripts)
}


# ── Tier 2: treesitter.python ─────────────────────────────────────────────────

.parse_python_treesitter <- function(scripts) {
  lang   <- treesitter.python::language()
  parser <- treesitter::parser(lang)

  all_nodes <- list()
  all_edges <- list()

  for (script in scripts) {
    script_name <- basename(script)

    all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
      name = script_name, type = "script", file = script,
      line = NA_integer_, label = script_name, shape_info = NA_character_
    )

    tryCatch({
      src  <- paste(readLines(script, warn = FALSE), collapse = "\n")
      tree <- treesitter::parser_parse(parser, src)
      root <- treesitter::tree_root_node(tree)

      # Function definitions
      fn_q  <- treesitter::query(lang,
        "(function_definition name: (identifier) @fn_name)")
      for (m in treesitter::query_matches(fn_q, root)) {
        node <- m$captures[["fn_name"]][[1L]]
        fn   <- treesitter::node_text(node, src)
        ln   <- treesitter::node_start_point(node)[["row"]] + 1L

        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = fn, type = "function", file = script,
          line = ln, label = fn, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = fn, type = "produces"
        )
      }

      # Top-level assignments (module-level only — parent is module node)
      assign_q <- treesitter::query(lang,
        "(module (expression_statement (assignment left: (identifier) @lhs)))")
      for (m in treesitter::query_matches(assign_q, root)) {
        node <- m$captures[["lhs"]][[1L]]
        var  <- treesitter::node_text(node, src)
        ln   <- treesitter::node_start_point(node)[["row"]] + 1L

        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = var, type = "variable", file = script,
          line = ln, label = var, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = var, type = "produces"
        )
      }

      # Import statements → source nodes
      import_q <- treesitter::query(lang,
        "[(import_statement name: (dotted_name) @mod)
          (import_from_statement module_name: (dotted_name) @mod)]")
      for (m in treesitter::query_matches(import_q, root)) {
        node <- m$captures[["mod"]][[1L]]
        mod  <- treesitter::node_text(node, src)

        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = mod, type = "source", file = NA_character_,
          line = NA_integer_, label = mod, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = mod, type = "reads"
        )
      }
    }, error = function(e) {
      message("treesitter.python failed for: ", script_name, " — ", conditionMessage(e))
    })
  }

  list(
    nodes = dplyr::bind_rows(all_nodes) |> dplyr::distinct(name, .keep_all = TRUE),
    edges = dplyr::bind_rows(all_edges) |> dplyr::distinct(),
    verbs = tibble::tibble(
      file = character(), line = integer(), fn_name = character(),
      input_var = character(), output_var = character(),
      args = list(), pkg = character()
    )
  )
}


# ── Tier 3: regex fallback (no extra packages) ────────────────────────────────

.parse_python_fallback <- function(scripts) {
  all_nodes <- list()
  all_edges <- list()

  for (script in scripts) {
    script_name <- basename(script)

    all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
      name = script_name, type = "script", file = script,
      line = NA_integer_, label = script_name, shape_info = NA_character_
    )

    tryCatch({
      lines <- readLines(script, warn = FALSE)

      # Function definitions: def name(
      fn_pattern <- "^\\s*(?:async\\s+)?def\\s+(\\w+)\\s*\\("
      fn_lines   <- which(grepl(fn_pattern, lines, perl = TRUE))
      for (ln in fn_lines) {
        m  <- regmatches(lines[ln],
                         regexec("def\\s+(\\w+)", lines[ln], perl = TRUE))[[1L]]
        if (length(m) < 2L) next
        fn <- m[2L]
        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = fn, type = "function", file = script,
          line = ln, label = fn, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = fn, type = "produces"
        )
      }

      # Class definitions: class Name
      cl_pattern <- "^\\s*class\\s+(\\w+)"
      cl_lines   <- which(grepl(cl_pattern, lines, perl = TRUE))
      for (ln in cl_lines) {
        m  <- regmatches(lines[ln],
                         regexec("class\\s+(\\w+)", lines[ln], perl = TRUE))[[1L]]
        if (length(m) < 2L) next
        cls <- m[2L]
        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = cls, type = "function", file = script,
          line = ln, label = cls, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = cls, type = "produces"
        )
      }

      # Top-level assignments: name = ... (not inside indent, not ==)
      top_assign <- "^([A-Za-z_][A-Za-z0-9_]*)\\s*=(?!=)"
      ta_lines   <- which(grepl(top_assign, lines, perl = TRUE))
      for (ln in ta_lines) {
        m   <- regmatches(lines[ln],
                          regexec(top_assign, lines[ln], perl = TRUE))[[1L]]
        if (length(m) < 2L) next
        var <- m[2L]
        # Skip dunder names and ALL_CAPS constants
        if (grepl("^__", var) || var == toupper(var)) next

        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = var, type = "variable", file = script,
          line = ln, label = var, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = var, type = "produces"
        )
      }

      # Import statements
      import_lines <- lines[grepl("^\\s*(?:import|from)\\s+", lines, perl = TRUE)]
      mods <- character(0L)
      for (imp in import_lines) {
        # import foo.bar  OR  from foo.bar import ...
        m <- regmatches(imp, regexec(
          "^\\s*(?:import|from)\\s+([A-Za-z_][A-Za-z0-9_.]*)", imp, perl = TRUE
        ))[[1L]]
        if (length(m) >= 2L) mods <- c(mods, m[2L])
      }
      for (mod in unique(mods)) {
        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = mod, type = "source", file = NA_character_,
          line = NA_integer_, label = mod, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = mod, type = "reads"
        )
      }
    }, error = function(e) {
      message("Could not parse Python script: ", script_name, " — ", conditionMessage(e))
    })
  }

  list(
    nodes = dplyr::bind_rows(all_nodes) |> dplyr::distinct(name, .keep_all = TRUE),
    edges = dplyr::bind_rows(all_edges) |> dplyr::distinct(),
    verbs = tibble::tibble(
      file = character(), line = integer(), fn_name = character(),
      input_var = character(), output_var = character(),
      args = list(), pkg = character()
    )
  )
}
