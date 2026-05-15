#' Parse R scripts to produce a pipeline edge list
#'
#' Dispatches to the best available parse backend in priority order:
#' 1. **treesitter** (`treesitter` + `treesitter.r` packages)
#' 2. **r** — base-R `getParseData()` (always available)
#'
#' @param project_dir Path to the project directory containing R scripts.
#' @param pattern Regex pattern to match R script files (default: `"\\.R$"`).
#' @param recursive Logical; whether to search sub-directories (default: `FALSE`).
#' @param backend One of `"auto"` (default), `"treesitter"`, or `"r"`. `"auto"`
#'   tries backends from fastest/richest to most compatible.
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
explicar_parse <- function(project_dir = ".",
                           pattern     = "\\.R$",
                           recursive   = FALSE,
                           backend     = c("auto", "treesitter", "r")) {
  backend <- match.arg(backend)

  scripts <- list.files(project_dir, pattern = pattern,
                        full.names = TRUE, recursive = recursive)

  if (length(scripts) == 0L) {
    message("No R scripts found in: ", project_dir)
    return(.empty_parse_result())
  }

  result <- switch(backend,
    auto       = .auto_dispatch(scripts),
    treesitter = .parse_treesitter(scripts),
    r          = .parse_r_fallback(scripts)
  )

  # Roxygen enrichment is backend-agnostic
  roxy_labels <- purrr::map_dfr(scripts, .extract_roxygen)
  result$nodes <- .merge_roxygen(result$nodes, roxy_labels)

  # Raw data source file references
  source_nodes <- .extract_source_files(scripts, project_dir)
  result$nodes <- dplyr::bind_rows(result$nodes, source_nodes) |>
    dplyr::distinct(name, .keep_all = TRUE)

  result
}


# ── Dispatch ──────────────────────────────────────────────────────────────────

.auto_dispatch <- function(scripts) {
  if (.parse_treesitter_available()) {
    result <- tryCatch(.parse_treesitter(scripts), error = function(e) NULL)
    if (!is.null(result)) return(result)
  }
  .parse_r_fallback(scripts)
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
  tryCatch({
    blocks <- suppressWarnings(roxygen2::parse_file(script))
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
  }, error = function(e) tibble::tibble())
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
        if (is.data.frame(df)) return(paste0(nrow(df), " × ", ncol(df)))
      }
      NA_character_
    }))

  parse_result
}
