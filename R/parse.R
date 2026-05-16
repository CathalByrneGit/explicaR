#' Parse scripts to produce a pipeline edge list
#'
#' Dispatches to the best available parse backend. R files are analysed in
#' priority order: treesitter → base-R `getParseData()`. Python files are
#' parsed when `languages` includes `"python"` (requires `treesitter.python`
#' or falls back to a pure-regex approach).
#'
#' @param project_dir Path to the project directory containing scripts.
#' @param pattern Regex pattern to match script files. When `NULL` (default)
#'   it is derived from `languages`: `"\\.R$"` for R-only, `"\\.R$|\\.py$"`
#'   for R + Python.
#' @param recursive Logical; whether to search sub-directories (default `FALSE`).
#' @param backend One of `"auto"` (default), `"treesitter"`, or `"r"`. Only
#'   affects R files; Python files always use the best available Python backend.
#' @param languages Character vector of languages to parse. Any subset of
#'   `c("r", "python")`. Default `"r"`.
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
#' # R only (default)
#' result <- explicar_parse("path/to/project")
#'
#' # Mixed R + Python project
#' result <- explicar_parse("path/to/project", languages = c("r", "python"))
#' result$nodes
#' result$edges
#' }
explicar_parse <- function(project_dir = ".",
                           pattern     = NULL,
                           recursive   = TRUE,
                           backend     = c("auto", "treesitter", "r"),
                           languages   = "r") {
  backend   <- match.arg(backend)
  languages <- tolower(languages)

  if (is.null(pattern)) {
    lang_pats <- c(r = "\\.R$", python = "\\.py$")
    matched   <- lang_pats[names(lang_pats) %in% languages]
    pattern   <- paste(matched, collapse = "|")
  }

  scripts <- list.files(project_dir, pattern = pattern,
                        full.names = TRUE, recursive = recursive)

  # Drop paths inside directories that are never source code
  skip_re <- paste0(
    "(/|\\\\)(",
    "\\.git|\\.explicar|\\.Rproj\\.user|renv|packrat|_targets|",
    "node_modules|\\.cache|\\.quarto",
    ")(/|\\\\|$)"
  )
  scripts <- scripts[!grepl(skip_re, scripts)]

  if (length(scripts) == 0L) {
    message("No scripts found in: ", project_dir)
    return(.empty_parse_result())
  }

  r_scripts  <- scripts[grepl("\\.R$",  scripts)]
  py_scripts <- scripts[grepl("\\.py$", scripts)]

  # Parse R files
  r_result <- if (length(r_scripts) > 0L) {
    switch(backend,
      auto       = .auto_dispatch(r_scripts),
      treesitter = .parse_treesitter(r_scripts),
      r          = .parse_r_fallback(r_scripts)
    )
  } else {
    .empty_parse_result()
  }

  # Parse Python files
  py_result <- if (length(py_scripts) > 0L && "python" %in% languages) {
    .auto_dispatch_python(py_scripts)
  } else {
    .empty_parse_result()
  }

  result <- .merge_parse_results(r_result, py_result)

  # Roxygen enrichment — try all R scripts; .extract_roxygen() returns an
  # empty tibble for files with no #' blocks, so non-package layouts are safe.
  roxy_labels <- purrr::map_dfr(r_scripts, .extract_roxygen)
  result$nodes <- .merge_roxygen(result$nodes, roxy_labels)

  # Raw data source file references (R files only — Python imports handled above)
  source_nodes <- .extract_source_files(r_scripts, project_dir)
  result$nodes <- dplyr::bind_rows(result$nodes, source_nodes) |>
    dplyr::distinct(name, .keep_all = TRUE)

  result
}


# ── Dispatch ──────────────────────────────────────────────────────────────────

.auto_dispatch <- function(scripts) {
  result <- tryCatch(.parse_treesitter(scripts), error = function(e) {
    message("treesitter parse failed (", conditionMessage(e), ") — falling back to R parser")
    NULL
  })
  if (!is.null(result)) return(result)
  .parse_r_fallback(scripts)
}

#' Combine two parse results (nodes de-duped, edges de-duped, verbs appended)
#' @noRd
.merge_parse_results <- function(a, b) {
  list(
    nodes = dplyr::bind_rows(a$nodes, b$nodes) |>
      dplyr::distinct(name, .keep_all = TRUE),
    edges = dplyr::bind_rows(a$edges, b$edges) |>
      dplyr::distinct(),
    verbs = dplyr::bind_rows(a$verbs, b$verbs)
  )
}


# ── Universal helpers (backend-agnostic) ──────────────────────────────────────

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


.extract_roxygen <- function(script) {
  if (!requireNamespace("roxygen2", quietly = TRUE)) return(tibble::tibble())
  suppressWarnings(tryCatch({
    blocks <- roxygen2::parse_file(script)
    if (!length(blocks)) return(tibble::tibble())

    purrr::map_dfr(blocks, function(blk) {
      fn_name <- tryCatch(blk$object$alias, error = function(e) NA_character_)
      title   <- tryCatch(
        roxygen2::block_get_tag_value(blk, "title"),
        error = function(e) NA_character_
      )
      desc <- tryCatch(
        roxygen2::block_get_tag_value(blk, "description"),
        error = function(e) NA_character_
      )
      tibble::tibble(fn_name = fn_name, title = title, description = desc)
    })
  }, error = function(e) tibble::tibble()))
}


.merge_roxygen <- function(nodes, roxy) {
  if (nrow(roxy) == 0L) return(nodes)
  roxy_clean <- roxy |>
    dplyr::filter(!is.na(fn_name), !is.na(title)) |>
    dplyr::mutate(label = dplyr::coalesce(title, fn_name)) |>
    dplyr::select(name = fn_name, label)

  nodes |>
    dplyr::left_join(roxy_clean, by = "name", suffix = c("", ".roxy")) |>
    dplyr::mutate(label = dplyr::coalesce(label.roxy, label)) |>
    dplyr::select(-dplyr::any_of("label.roxy"))
}


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
          file_arg <- regmatches(m, regexpr('"[^"]+"|\'[^\']+\'', m))
          if (length(file_arg) > 0) {
            fname <- gsub('["\']', '', file_arg[1])
            all_sources[[length(all_sources) + 1L]] <- tibble::tibble(
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
    return(tibble::tibble(
      name = character(), type = character(), file = character(),
      line = integer(), label = character(), shape_info = character()
    ))
  }
  dplyr::bind_rows(all_sources) |> dplyr::distinct(name, .keep_all = TRUE)
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
        if (is.data.frame(df)) return(paste0(nrow(df), " \u00D7 ", ncol(df)))
      }
      NA_character_
    }))

  parse_result
}
