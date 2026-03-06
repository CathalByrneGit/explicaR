#' Per-verb animation logic
#'
#' Each function in this file returns a list describing how to animate a
#' specific dplyr/tidyr verb transformation. These descriptors are consumed
#' by [build_animation()] in `animate.R`.
#'
#' A verb descriptor is a list with:
#' - `verb`: the verb name
#' - `pkg`: package (`"dplyr"` or `"tidyr"`)
#' - `pipeline_expr`: a quoted expression for datamations
#' - `description`: human-readable one-liner
#' - `data_before` / `data_after`: optional snapshots (from trace or targets)

# ── Verb descriptor constructors ─────────────────────────────────────────────

#' Build animation descriptor for filter()
#' @noRd
verb_filter <- function(data_before, condition_text, output_var = NULL,
                        data_after = NULL) {
  list(
    verb         = "filter",
    pkg          = "dplyr",
    description  = glue::glue("Keep rows where {condition_text}"),
    data_before  = data_before,
    data_after   = data_after %||% .apply_safely(data_before, "filter", condition_text),
    pipeline_expr = .make_pipe_expr("filter", condition_text, data_before),
    output_var   = output_var
  )
}

#' Build animation descriptor for mutate()
#' @noRd
verb_mutate <- function(data_before, mutation_text, output_var = NULL,
                        data_after = NULL) {
  list(
    verb         = "mutate",
    pkg          = "dplyr",
    description  = glue::glue("Add or modify column: {mutation_text}"),
    data_before  = data_before,
    data_after   = data_after,
    pipeline_expr = .make_pipe_expr("mutate", mutation_text, data_before),
    output_var   = output_var
  )
}

#' Build animation descriptor for select()
#' @noRd
verb_select <- function(data_before, cols_text, output_var = NULL,
                        data_after = NULL) {
  list(
    verb         = "select",
    pkg          = "dplyr",
    description  = glue::glue("Keep columns: {cols_text}"),
    data_before  = data_before,
    data_after   = data_after,
    pipeline_expr = .make_pipe_expr("select", cols_text, data_before),
    output_var   = output_var
  )
}

#' Build animation descriptor for group_by()
#' @noRd
verb_group_by <- function(data_before, group_cols, output_var = NULL,
                          data_after = NULL) {
  list(
    verb         = "group_by",
    pkg          = "dplyr",
    description  = glue::glue("Group rows by: {paste(group_cols, collapse = ', ')}"),
    data_before  = data_before,
    data_after   = data_after,
    pipeline_expr = .make_pipe_expr("group_by", paste(group_cols, collapse = ", "),
                                    data_before),
    output_var   = output_var
  )
}

#' Build animation descriptor for summarise()
#' @noRd
verb_summarise <- function(data_before, summary_text, group_cols = NULL,
                           output_var = NULL, data_after = NULL) {
  desc <- if (!is.null(group_cols)) {
    glue::glue("Summarise by {paste(group_cols, collapse=', ')}: {summary_text}")
  } else {
    glue::glue("Summarise: {summary_text}")
  }
  list(
    verb         = "summarise",
    pkg          = "dplyr",
    description  = desc,
    data_before  = data_before,
    data_after   = data_after,
    pipeline_expr = .make_pipe_expr("summarise", summary_text, data_before),
    output_var   = output_var
  )
}

#' Build animation descriptor for arrange()
#' @noRd
verb_arrange <- function(data_before, sort_cols, output_var = NULL,
                         data_after = NULL) {
  list(
    verb         = "arrange",
    pkg          = "dplyr",
    description  = glue::glue("Sort rows by: {paste(sort_cols, collapse = ', ')}"),
    data_before  = data_before,
    data_after   = data_after,
    pipeline_expr = .make_pipe_expr("arrange", paste(sort_cols, collapse = ", "),
                                    data_before),
    output_var   = output_var
  )
}

#' Build animation descriptor for left_join()
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


# ── Dispatch ─────────────────────────────────────────────────────────────────

#' Dispatch a verb record to the appropriate descriptor constructor
#'
#' @param verb_record A single row from `parse_result$verbs` as a list.
#' @param snapshots Named list of dataframes (from trace or targets cache).
#'
#' @return A verb descriptor list, or `NULL` if the verb is not supported.
#' @export
verb_descriptor <- function(verb_record, snapshots = list()) {
  fn   <- verb_record$fn_name
  args <- verb_record$args[[1]]

  data_before <- snapshots[[verb_record$input_var]]
  data_after  <- snapshots[[verb_record$output_var]]

  args_text <- if (length(args) > 1) paste(args[-1], collapse = ", ") else ""

  switch(fn,
    filter       = verb_filter(data_before,  args_text,       verb_record$output_var, data_after),
    mutate       = verb_mutate(data_before,  args_text,       verb_record$output_var, data_after),
    select       = verb_select(data_before,  args_text,       verb_record$output_var, data_after),
    group_by     = verb_group_by(data_before, strsplit(args_text, ",\\s*")[[1]],
                                 verb_record$output_var, data_after),
    summarise    = ,
    summarize    = verb_summarise(data_before, args_text, output_var = verb_record$output_var,
                                  data_after = data_after),
    arrange      = verb_arrange(data_before,  strsplit(args_text, ",\\s*")[[1]],
                                verb_record$output_var, data_after),
    pivot_longer = verb_pivot_longer(data_before, args_text, output_var = verb_record$output_var,
                                     data_after = data_after),
    pivot_wider  = verb_pivot_wider(data_before, args_text, args_text,
                                    verb_record$output_var, data_after),
    left_join    = ,
    right_join   = ,
    inner_join   = ,
    full_join    = verb_left_join(data_before, NULL, args_text,
                                  verb_record$output_var, data_after),
    {
      message("Unsupported verb: ", fn)
      NULL
    }
  )
}


# ── Utilities ─────────────────────────────────────────────────────────────────

`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Try applying a dplyr verb to illustrative data
#' @noRd
.apply_safely <- function(data, verb, args_text) {
  if (is.null(data)) return(NULL)
  tryCatch(
    eval(parse(text = glue::glue("dplyr::{verb}(data, {args_text})"))),
    error = function(e) NULL
  )
}

#' Build a simple pipe expression for datamations
#' @noRd
.make_pipe_expr <- function(verb, args_text, data) {
  if (is.null(data)) return(NULL)
  bquote(data |> .(as.name(paste0("dplyr::", verb)))(.(args_text)))
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
