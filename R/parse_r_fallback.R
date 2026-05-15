# ── Tier-3 parser: base-R getParseData() ─────────────────────────────────────
# Two-pass approach:
#   Pass 1 — collect output variables and function calls per script.
#   Pass 2 — for each script, find reads of variables produced by *other*
#             scripts and emit "consumes" edges.
# No external packages required beyond base R.

#' @noRd
.parse_r_fallback <- function(scripts) {
  all_nodes      <- list()
  all_edges      <- list()
  script_outputs <- list()
  script_pd      <- list()

  for (script in scripts) {
    script_name <- basename(script)

    all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
      name = script_name, type = "script", file = script,
      line = NA_integer_, label = script_name, shape_info = NA_character_
    )

    tryCatch({
      pd <- getParseData(parse(file = script, keep.source = TRUE))
      if (is.null(pd)) {
        script_pd[[script]] <- NULL
        next
      }
      script_pd[[script]] <- pd

      assigns <- pd[pd$token == "LEFT_ASSIGN", ]
      outputs <- character(0L)
      for (i in seq_len(nrow(assigns))) {
        parent_id <- assigns$parent[i]
        lhs <- pd[pd$parent == parent_id & pd$token == "SYMBOL", ]
        if (nrow(lhs) > 0L) {
          varname <- lhs$text[1L]
          outputs <- c(outputs, varname)
          all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
            name = varname, type = "variable", file = script,
            line = assigns$line1[i], label = varname, shape_info = NA_character_
          )
          all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
            from = script_name, to = varname, type = "produces"
          )
        }
      }
      script_outputs[[script]] <- outputs

      fn_calls <- unique(pd[pd$token == "SYMBOL_FUNCTION_CALL", "text"])
      for (fn in fn_calls) {
        all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
          name = fn, type = "function", file = script,
          line = NA_integer_, label = fn, shape_info = NA_character_
        )
        all_edges[[length(all_edges) + 1L]] <- tibble::tibble(
          from = script_name, to = fn, type = "calls"
        )
      }
    }, error = function(e) {
      message("Could not parse: ", script_name, " — ", conditionMessage(e))
    })
  }

  all_output_vars <- unique(unlist(script_outputs, use.names = FALSE))

  for (script in scripts) {
    pd <- script_pd[[script]]
    if (is.null(pd)) next
    script_name   <- basename(script)
    local_outputs <- script_outputs[[script]] %||% character(0L)

    reads <- unique(pd[pd$token == "SYMBOL", "text"])
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


# ── Verb extraction ───────────────────────────────────────────────────────────

.verb_discovery_cache <- new.env(parent = emptyenv())

.discover_verbs <- function(pkgs = c("dplyr", "tidyr")) {
  cache_key <- paste(sort(pkgs), collapse = "_")
  if (exists(cache_key, envir = .verb_discovery_cache, inherits = FALSE))
    return(get(cache_key, envir = .verb_discovery_cache))

  result <- c()
  first_arg_names <- c(".data", "x", "data", ".tbl")

  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    fns <- getNamespaceExports(pkg)
    is_verb <- vapply(fns, function(f) {
      tryCatch({
        p <- names(suppressWarnings(formals(getExportedValue(pkg, f))))
        length(p) > 0 && p[1] %in% first_arg_names
      }, error = function(e) FALSE)
    }, logical(1))
    keep   <- fns[is_verb]
    result <- c(result, setNames(rep(pkg, length(keep)), keep))
  }

  assign(cache_key, result, envir = .verb_discovery_cache)
  result
}


.extract_verbs_all <- function(scripts) {
  purrr::map_dfr(scripts, .extract_verbs_one)
}

.extract_verbs_one <- function(script) {
  tidyverse_verbs <- .discover_verbs()

  tryCatch({
    pd <- getParseData(parse(file = script, keep.source = TRUE))
    if (is.null(pd)) return(tibble::tibble())

    fn_calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
                     pd$text %in% names(tidyverse_verbs), ]

    if (nrow(fn_calls) == 0) return(tibble::tibble())

    purrr::map_dfr(seq_len(nrow(fn_calls)), function(i) {
      fn  <- fn_calls$text[i]
      ln  <- fn_calls$line1[i]
      output_var <- .find_assigned_var(pd, fn_calls$id[i])
      input_var  <- .find_first_arg_symbol(pd, fn_calls$id[i])

      tibble::tibble(
        file       = script,
        line       = ln,
        fn_name    = fn,
        input_var  = input_var,
        output_var = output_var,
        args       = list(.extract_call_args(pd, fn_calls$id[i])),
        pkg        = unname(tidyverse_verbs[fn])
      )
    })
  }, error = function(e) tibble::tibble())
}


.find_assigned_var <- function(pd, node_id, depth = 0L) {
  if (depth > 15L) return(NA_character_)
  row <- pd[pd$id == node_id, ]
  if (nrow(row) == 0) return(NA_character_)
  parent_id <- row$parent[1]
  if (parent_id <= 0) return(NA_character_)
  parent_row <- pd[pd$id == parent_id, ]
  if (nrow(parent_row) == 0) return(NA_character_)

  if (parent_row$token[1] %in% c("expr", "equal_assign")) {
    siblings    <- pd[pd$parent == parent_id, ]
    assign_rows <- siblings[siblings$token == "LEFT_ASSIGN", ]
    if (nrow(assign_rows) > 0) {
      lhs_sym <- siblings[siblings$token == "SYMBOL" &
                            siblings$col1 < assign_rows$col1[1], ]
      if (nrow(lhs_sym) > 0) return(lhs_sym$text[1])
    }
  }
  .find_assigned_var(pd, parent_id, depth + 1L)
}


.find_first_arg_symbol <- function(pd, fn_node_id) {
  parent_id <- pd[pd$id == fn_node_id, "parent"]
  if (length(parent_id) == 0 || is.na(parent_id)) return(NA_character_)
  call_parent <- pd[pd$parent == parent_id[1], ]
  syms <- call_parent[call_parent$token == "SYMBOL" &
                        call_parent$id != fn_node_id, ]
  if (nrow(syms) == 0) return(NA_character_)
  syms$text[1]
}


.extract_call_args <- function(pd, fn_node_id) {
  parent_id <- pd[pd$id == fn_node_id, "parent"]
  if (length(parent_id) == 0) return(list())
  siblings <- pd[pd$parent == parent_id[1] &
                   pd$token %in% c("SYMBOL", "STR_CONST", "NUM_CONST",
                                   "SYMBOL_FUNCTION_CALL"), ]
  as.list(siblings$text)
}
