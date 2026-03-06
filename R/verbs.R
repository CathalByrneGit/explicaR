#' Per-verb animation logic
#'
#' `make_verb_descriptor()` is the generic factory for standard single-table
#' verbs. It pulls the function title from the package's own Rd documentation
#' so no description text needs to be hardcoded here.
#'
#' Special-case constructors are kept only for structurally different verbs
#' (joins, pivots) that need extra fields or special illustrative data.
#'
#' A verb descriptor is a list with:
#' - `verb`: the verb name
#' - `pkg`: package (`"dplyr"` or `"tidyr"`)
#' - `pipeline_expr`: a quoted expression for datamations
#' - `description`: human-readable one-liner (from Rd `\title`)
#' - `data_before` / `data_after`: optional snapshots (from trace or targets)


# ── Rd title cache ────────────────────────────────────────────────────────────

.rd_db_cache <- new.env(parent = emptyenv())

#' Retrieve (and cache) the Rd database for a package
#' @noRd
.get_rd_db <- function(pkg) {
  if (exists(pkg, envir = .rd_db_cache, inherits = FALSE))
    return(get(pkg, envir = .rd_db_cache))
  db <- tryCatch(tools::Rd_db(pkg), error = function(e) list())
  assign(pkg, db, envir = .rd_db_cache)
  db
}

#' Extract the \title text from a package's Rd file for one function
#' @noRd
.verb_title_from_rd <- function(pkg, fn_name) {
  tryCatch({
    db  <- .get_rd_db(pkg)
    key <- paste0(fn_name, ".Rd")
    if (!key %in% names(db)) return(fn_name)
    rd  <- db[[key]]
    for (item in rd) {
      if (identical(attr(item, "Rd_tag"), "\\title"))
        return(trimws(paste(unlist(item), collapse = "")))
    }
    fn_name
  }, error = function(e) fn_name)
}


# ── Generic descriptor factory ────────────────────────────────────────────────

#' Build an animation descriptor for any standard single-table verb
#'
#' Description is derived automatically from the function's Rd `\title`, so
#' no text needs to be hardcoded for each verb.
#'
#' @param fn_name  Function name (e.g. `"filter"`, `"rename"`).
#' @param pkg      Package name (`"dplyr"` or `"tidyr"`).
#' @param data_before  Snapshot before the verb (data.frame or NULL).
#' @param args_text    Argument text extracted from the call.
#' @param output_var   Name of the output variable in the pipeline.
#' @param data_after   Snapshot after the verb (data.frame or NULL); computed
#'   by applying the verb to `data_before` when NULL.
#' @noRd
make_verb_descriptor <- function(fn_name, pkg, data_before, args_text,
                                  output_var, data_after) {
  title <- .verb_title_from_rd(pkg, fn_name)
  list(
    verb          = fn_name,
    pkg           = pkg,
    description   = glue::glue("{title}: {args_text}"),
    data_before   = data_before,
    data_after    = data_after %||%
      .apply_safely(data_before, paste0(pkg, "::", fn_name), args_text),
    pipeline_expr = .make_pipe_expr(fn_name, args_text, data_before, pkg),
    output_var    = output_var
  )
}


# ── Special-case constructors (structurally different verbs) ──────────────────

#' Build animation descriptor for *_join()
#' @noRd
verb_left_join <- function(data_before, data_y, by_cols, output_var = NULL,
                           data_after = NULL) {
  by_str <- if (is.character(by_cols)) {
    paste(by_cols, collapse = ", ")
  } else {
    paste(names(by_cols), "=", unname(by_cols), collapse = ", ")
  }
  list(
    verb         = "left_join",
    pkg          = "dplyr",
    description  = glue::glue("Join on: {by_str}"),
    data_before  = data_before,
    data_y       = data_y,
    data_after   = data_after,
    by_cols      = by_cols,
    output_var   = output_var
  )
}

#' Build animation descriptor for pivot_longer()
#' @noRd
verb_pivot_longer <- function(data_before, cols_text, names_to = "name",
                              values_to = "value", output_var = NULL,
                              data_after = NULL) {
  list(
    verb         = "pivot_longer",
    pkg          = "tidyr",
    description  = glue::glue(
      "Pivot columns [{cols_text}] to rows \u2192 '{names_to}' / '{values_to}'"
    ),
    data_before  = data_before,
    data_after   = data_after,
    cols_text    = cols_text,
    names_to     = names_to,
    values_to    = values_to,
    pipeline_expr = .make_pivot_longer_expr(data_before, cols_text,
                                             names_to, values_to),
    output_var   = output_var
  )
}

#' Build animation descriptor for pivot_wider()
#' @noRd
verb_pivot_wider <- function(data_before, names_from, values_from,
                             output_var = NULL, data_after = NULL) {
  list(
    verb         = "pivot_wider",
    pkg          = "tidyr",
    description  = glue::glue(
      "Pivot '{names_from}' column to wide format (values from '{values_from}')"
    ),
    data_before  = data_before,
    data_after   = data_after,
    names_from   = names_from,
    values_from  = values_from,
    output_var   = output_var
  )
}


# ── Dispatch ──────────────────────────────────────────────────────────────────

#' Dispatch a verb record to the appropriate descriptor constructor
#'
#' Standard single-table verbs are handled by the generic `make_verb_descriptor()`
#' factory, which derives descriptions from Rd documentation. Only joins and
#' pivots (structurally different) have dedicated constructors.
#'
#' @param verb_record A single row from `parse_result$verbs` as a list.
#' @param snapshots Named list of dataframes (from trace or targets cache).
#'
#' @return A verb descriptor list, or `NULL` if the verb is not supported.
#' @export
verb_descriptor <- function(verb_record, snapshots = list()) {
  fn          <- verb_record$fn_name
  args        <- verb_record$args[[1]]
  data_before <- snapshots[[verb_record$input_var]]
  data_after  <- snapshots[[verb_record$output_var]]
  args_text   <- if (length(args) > 1) paste(args[-1], collapse = ", ") else ""
  pkg         <- verb_record$pkg

  # Structurally special verbs
  if (fn %in% c("left_join", "right_join", "inner_join", "full_join")) {
    return(verb_left_join(data_before, NULL, args_text,
                          verb_record$output_var, data_after))
  }
  if (fn == "pivot_longer") {
    return(verb_pivot_longer(data_before, args_text,
                              output_var = verb_record$output_var,
                              data_after = data_after))
  }
  if (fn == "pivot_wider") {
    return(verb_pivot_wider(data_before, args_text, args_text,
                             verb_record$output_var, data_after))
  }

  # Generic factory for all other verbs
  if (is.na(pkg) || !nzchar(pkg)) {
    message("Unknown package for verb: ", fn)
    return(NULL)
  }
  make_verb_descriptor(fn, pkg, data_before, args_text,
                        verb_record$output_var, data_after)
}


# ── Utilities ─────────────────────────────────────────────────────────────────

`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Try applying a verb to illustrative data
#' @param fn_qualified Fully qualified function name, e.g. `"dplyr::filter"`.
#' @noRd
.apply_safely <- function(data, fn_qualified, args_text) {
  if (is.null(data)) return(NULL)
  tryCatch(
    eval(parse(text = glue::glue("{fn_qualified}(data, {args_text})"))),
    error = function(e) NULL
  )
}

#' Build a simple pipe expression for datamations
#' @noRd
.make_pipe_expr <- function(verb, args_text, data, pkg = "dplyr") {
  if (is.null(data)) return(NULL)
  bquote(data |> .(as.name(paste0(pkg, "::", verb)))(.(args_text)))
}

#' Build pivot_longer expression for datamations
#' @noRd
.make_pivot_longer_expr <- function(data, cols_text, names_to, values_to) {
  if (is.null(data)) return(NULL)
  glue::glue(
    'data |> tidyr::pivot_longer(cols = {cols_text}, ',
    'names_to = "{names_to}", values_to = "{values_to}")'
  )
}
