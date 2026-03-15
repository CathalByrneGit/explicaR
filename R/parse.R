#' Parse R scripts to produce a pipeline edge list
#'
#' Uses `getParseData()` for both cross-script dependency analysis and
#' fine-grained verb extraction within scripts.
#'
#' @param project_dir Path to the project directory containing R scripts.
#' @param pattern Regex pattern to match R script files (default: `"\\.R$"`).
#' @param recursive Logical; whether to search subdirectories (default: `FALSE`).
#'
#' @return A list with:
#'   - `nodes`: tibble of nodes (name, type, file, line, label, shape_info)
#'   - `edges`: tibble of edges (from, to, type)
#'   - `verbs`: tibble of verb call records for the animation layer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- explicar_parse("path/to/my/project")
#' result$nodes
#' result$edges
#' }
explicar_parse <- function(project_dir = ".", pattern = "\\.R$", recursive = FALSE) {
  scripts <- list.files(project_dir, pattern = pattern,
                        full.names = TRUE, recursive = recursive)

  if (length(scripts) == 0) {
    message("No R scripts found in: ", project_dir)
    return(.empty_parse_result())
  }

  # --- Native parse: cross-script dependency analysis ---
  cd_result  <- .native_parse(scripts)
  cd_nodes   <- cd_result$nodes
  cd_edges   <- cd_result$edges

  # --- getParseData: fine-grained verb extraction ---
  verb_records <- purrr::map_dfr(scripts, .extract_verbs)

  # --- roxygen2 enrichment (if available) ---
  roxy_labels <- purrr::map_dfr(scripts, .extract_roxygen)

  # Merge roxygen labels into nodes
  cd_nodes <- .merge_roxygen(cd_nodes, roxy_labels)

  # Add source file nodes (CSV, xlsx referenced via read_csv / read.csv etc.)
  source_nodes <- .extract_source_files(scripts, project_dir)
  all_nodes <- dplyr::bind_rows(cd_nodes, source_nodes) |>
    dplyr::distinct(name, .keep_all = TRUE)

  list(
    nodes = all_nodes,
    edges = cd_edges,
    verbs = verb_records
  )
}


# ── Internal helpers ─────────────────────────────────────────────────────────

.empty_parse_result <- function() {
  list(
    nodes = tibble::tibble(name = character(), type = character(),
                           file = character(), line = integer(),
                           label = character(), shape_info = character()),
    edges = tibble::tibble(from = character(), to = character(), type = character()),
    verbs = tibble::tibble(file = character(), line = integer(),
                           fn_name = character(), input_var = character(),
                           output_var = character(), args = list(), pkg = character())
  )
}


#' Native dependency parser using getParseData()
#'
#' Two-pass approach:
#'   Pass 1 — collect output variables and function calls per script.
#'   Pass 2 — for each script, find reads of variables produced by *other*
#'             scripts and emit "consumes" edges.
#' @noRd
.native_parse <- function(scripts) {
  all_nodes      <- list()
  all_edges      <- list()
  script_outputs <- list()   # script path -> character vector of output names
  script_pd      <- list()   # script path -> parse-data frame (cache)

  # ── Pass 1: outputs, function calls, script/variable nodes ──────────────────
  for (script in scripts) {
    script_name <- basename(script)

    all_nodes[[length(all_nodes) + 1L]] <- tibble::tibble(
      name = script_name, type = "script", file = script,
      line = NA_integer_, label = script_name, shape_info = NA_character_
    )

    tryCatch({
      pd <- getParseData(parse(file = script, keep.source = TRUE))
      if (is.null(pd)) return(invisible(NULL))
      script_pd[[script]] <- pd

      # Output variables: SYMBOL on LHS of <-
      assigns  <- pd[pd$token == "LEFT_ASSIGN", ]
      outputs  <- character(0L)
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

      # Function call nodes + "calls" edges
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
      message("Could not parse script: ", script_name, " — ", conditionMessage(e))
    })
  }

  # ── Pass 2: cross-script "consumes" edges ───────────────────────────────────
  all_output_vars <- unique(unlist(script_outputs, use.names = FALSE))

  for (script in scripts) {
    pd <- script_pd[[script]]
    if (is.null(pd)) next
    script_name   <- basename(script)
    local_outputs <- script_outputs[[script]] %||% character(0L)

    # SYMBOL tokens that refer to a variable produced by another script
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
    edges = dplyr::bind_rows(all_edges) |> dplyr::distinct()
  )
}


# ── Verb discovery ────────────────────────────────────────────────────────────

#' Discover data-transformation verbs exported from dplyr / tidyr
#'
#' A function qualifies when its first formal parameter is one of the
#' conventional `.data` / `x` / `data` / `.tbl` names used by the tidyverse.
#' The result is a named character vector `c(verb = "pkg", ...)`, cached for
#' the session so the scan runs at most once.
#' @noRd
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


#' Extract dplyr/tidyr verb calls from a single script
#' @noRd
.extract_verbs <- function(script) {
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

      # Try to find the output variable: walk up AST to find enclosing assignment
      output_var <- .find_assigned_var(pd, fn_calls$id[i])
      # Try to find the input variable (first arg that looks like a symbol)
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
  }, error = function(e) {
    tibble::tibble()
  })
}


#' Walk up AST from a node to find the variable it is assigned to
#' @noRd
.find_assigned_var <- function(pd, node_id, depth = 0L) {
  if (depth > 15L) return(NA_character_)
  row <- pd[pd$id == node_id, ]
  if (nrow(row) == 0) return(NA_character_)
  parent_id <- row$parent[1]
  if (parent_id <= 0) return(NA_character_)
  parent_row <- pd[pd$id == parent_id, ]
  if (nrow(parent_row) == 0) return(NA_character_)

  # Check if this parent is an assignment expression
  if (parent_row$token[1] %in% c("expr", "equal_assign")) {
    # Look for LEFT_ASSIGN sibling
    siblings <- pd[pd$parent == parent_id, ]
    assign_rows <- siblings[siblings$token == "LEFT_ASSIGN", ]
    if (nrow(assign_rows) > 0) {
      lhs_sym <- siblings[siblings$token == "SYMBOL" &
                            siblings$col1 < assign_rows$col1[1], ]
      if (nrow(lhs_sym) > 0) return(lhs_sym$text[1])
    }
  }
  .find_assigned_var(pd, parent_id, depth + 1L)
}


#' Find the first symbol argument to a function call
#' @noRd
.find_first_arg_symbol <- function(pd, fn_node_id) {
  parent_id <- pd[pd$id == fn_node_id, "parent"]
  if (length(parent_id) == 0 || is.na(parent_id)) return(NA_character_)
  # The call expression parent contains the argument list
  call_parent <- pd[pd$parent == parent_id[1], ]
  # First SYMBOL in siblings that isn't the function name itself
  syms <- call_parent[call_parent$token == "SYMBOL" &
                        call_parent$id != fn_node_id, ]
  if (nrow(syms) == 0) return(NA_character_)
  syms$text[1]
}


#' Extract argument text snippets for a call
#' @noRd
.extract_call_args <- function(pd, fn_node_id) {
  parent_id <- pd[pd$id == fn_node_id, "parent"]
  if (length(parent_id) == 0) return(list())
  siblings <- pd[pd$parent == parent_id[1] &
                   pd$token %in% c("SYMBOL", "STR_CONST", "NUM_CONST",
                                   "SYMBOL_FUNCTION_CALL"), ]
  as.list(siblings$text)
}


#' Extract roxygen documentation blocks from a script
#' @noRd
.extract_roxygen <- function(script) {
  if (!requireNamespace("roxygen2", quietly = TRUE)) {
    return(tibble::tibble())
  }
  tryCatch({
    blocks <- suppressWarnings(roxygen2::parse_file(script))
    if (length(blocks) == 0) return(tibble::tibble())

    purrr::map_dfr(blocks, function(blk) {
      fn_name <- tryCatch(blk$object$alias, error = function(e) NA_character_)
      title   <- tryCatch(
        roxygen2::block_get_tag_value(blk, "title"), error = function(e) NA_character_
      )
      desc    <- tryCatch(
        roxygen2::block_get_tag_value(blk, "description"), error = function(e) NA_character_
      )
      tibble::tibble(fn_name = fn_name, title = title, description = desc)
    })
  }, error = function(e) tibble::tibble())
}


#' Merge roxygen labels into node table
#' @noRd
.merge_roxygen <- function(nodes, roxy) {
  if (nrow(roxy) == 0) return(nodes)
  roxy_clean <- roxy |>
    dplyr::filter(!is.na(fn_name), !is.na(title)) |>
    dplyr::mutate(label = dplyr::coalesce(title, fn_name)) |>
    dplyr::select(name = fn_name, label)

  nodes |>
    dplyr::left_join(roxy_clean, by = "name", suffix = c("", ".roxy")) |>
    dplyr::mutate(label = dplyr::coalesce(label.roxy, label)) |>
    dplyr::select(-dplyr::any_of("label.roxy"))
}


#' Detect raw data source file references in scripts
#' @noRd
.extract_source_files <- function(scripts, project_dir) {
  read_fns <- c("read.csv", "read_csv", "read_delim", "read_excel",
                "readRDS", "readr::read_csv", "data.table::fread",
                "read.table", "read.delim")
  all_sources <- list()

  for (script in scripts) {
    tryCatch({
      lines <- readLines(script, warn = FALSE)
      for (fn in read_fns) {
        matches <- grep(fn, lines, value = TRUE, fixed = TRUE)
        for (m in matches) {
          # Extract the first quoted string argument
          file_arg <- regmatches(m, regexpr('"[^"]+"|\'[^\']+\'', m))
          if (length(file_arg) > 0) {
            fname <- gsub('["\']', '', file_arg[1])
            all_sources[[length(all_sources) + 1]] <- tibble::tibble(
              name       = basename(fname),
              type       = "source",
              file       = fname,
              line       = NA_integer_,
              label      = basename(fname),
              shape_info = NA_character_
            )
          }
        }
      }
    }, error = function(e) invisible(NULL))
  }

  if (!length(all_sources)) {
    return(tibble::tibble(name = character(), type = character(),
                          file = character(), line = integer(),
                          label = character(), shape_info = character()))
  }
  dplyr::bind_rows(all_sources) |> dplyr::distinct(name, .keep_all = TRUE)
}
