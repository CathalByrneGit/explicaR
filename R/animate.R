#' Build datamations animations for pipeline verb nodes
#'
#' `explicar_animate()` takes the output of [explicar_parse()] and a snapshot
#' list of intermediate dataframes, and returns a named list of animation
#' objects — one per verb record in the parse result.
#'
#' Two modes are supported:
#' - **Illustrative** (default): generates a small synthetic dataframe that
#'   matches column names/types of the real data but uses toy values. Safe and
#'   fast.
#' - **Real**: uses actual intermediate snapshots from the trace or targets
#'   cache. More informative but requires execution.
#'
#' @param parse_result Output from [explicar_parse()].
#' @param snapshots Named list of real intermediate dataframes (from
#'   [explicar_targets()] or [with_pipeline_trace()]). When `NULL`, illustrative
#'   mode is used for all verb nodes.
#' @param max_rows Maximum rows to show in animations (default 50). Larger
#'   dataframes are sampled down for clarity.
#' @param verbs Character vector of verb names to animate. `NULL` (default)
#'   animates all supported verbs found in the parse result.
#'
#' @return A named list where each element is a list with:
#'   - `descriptor`: the verb descriptor (from [verb_descriptor()])
#'   - `widget`: a datamations htmlwidget (if datamations is available) or
#'     a plain HTML fallback
#'   - `json`: the Vega-Lite animation spec as a character string
#' @export
#'
#' @examples
#' \dontrun{
#' pr  <- explicar_parse("path/to/project")
#' snaps <- with_pipeline_trace("path/to/project/pipeline.R")
#' anim <- explicar_animate(pr, snapshots = snaps)
#' anim[["filter@clean.R:23"]]$widget
#' }
explicar_animate <- function(parse_result,
                             snapshots = NULL,
                             max_rows  = 50L,
                             verbs     = NULL) {
  verb_records <- parse_result$verbs

  if (nrow(verb_records) == 0) {
    message("No verb records found. Run explicar_parse() first.")
    return(list())
  }

  if (!is.null(verbs)) {
    verb_records <- verb_records[verb_records$fn_name %in% verbs, ]
  }

  snaps <- snapshots %||% list()

  results <- list()

  for (i in seq_len(nrow(verb_records))) {
    rec  <- as.list(verb_records[i, ])
    key  <- .animation_key(rec)

    desc <- tryCatch(
      verb_descriptor(rec, snaps),
      error = function(e) {
        message("Could not build descriptor for ", key, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(desc)) next

    # Down-sample large dataframes for clarity
    if (!is.null(desc$data_before) && nrow(desc$data_before) > max_rows) {
      desc$data_before <- desc$data_before[seq_len(max_rows), , drop = FALSE]
    }
    if (!is.null(desc$data_after) && nrow(desc$data_after) > max_rows) {
      desc$data_after <- desc$data_after[seq_len(max_rows), , drop = FALSE]
    }

    # Try illustrative fallback when we have no real data
    if (is.null(desc$data_before)) {
      desc$data_before <- .illustrative_data(rec$fn_name)
    }

    widget <- .build_widget(desc)
    json   <- .build_json(desc)

    results[[key]] <- list(
      descriptor = desc,
      widget     = widget,
      json       = json
    )
  }

  results
}


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Unique key for an animation entry
#' @noRd
.animation_key <- function(rec) {
  paste0(rec$fn_name, "@", basename(rec$file), ":", rec$line)
}


#' Attempt to build a datamations widget; fall back to HTML if unavailable
#' @noRd
.build_widget <- function(desc) {
  if (!requireNamespace("datamations", quietly = TRUE)) {
    return(.fallback_html_widget(desc))
  }

  tryCatch({
    if (!is.null(desc$pipeline_expr) && !is.null(desc$data_before)) {
      # Build the pipeline as a quoted expression with the actual data
      pipe_call <- .build_datamation_call(desc)
      if (!is.null(pipe_call)) {
        return(eval(pipe_call))
      }
    }
    .fallback_html_widget(desc)
  }, error = function(e) {
    message("datamations error (", desc$verb, "): ", conditionMessage(e))
    .fallback_html_widget(desc)
  })
}


#' Build the datamation() call for supported verbs
#' @noRd
.build_datamation_call <- function(desc) {
  data <- desc$data_before
  if (is.null(data) || nrow(data) == 0) return(NULL)

  verb <- desc$verb

  # datamations works best with dplyr pipelines expressed as quoted calls
  tryCatch({
    switch(verb,
      filter = {
        expr <- bquote(datamations::datamation_sanddance(
          data |> dplyr::filter(.(rlang::parse_expr(desc$description)))
        ))
        # Use a simpler approach: just show before/after
        .before_after_widget(desc)
      },
      pivot_longer = {
        .before_after_widget(desc)
      },
      .before_after_widget(desc)
    )
  }, error = function(e) NULL)
}


#' Build a before/after HTML comparison widget when datamations is unavailable
#' @noRd
.before_after_widget <- function(desc) {
  before_html <- .df_to_html_table(desc$data_before, max_rows = 8L, caption = "Before")
  after_html  <- .df_to_html_table(desc$data_after,  max_rows = 8L, caption = "After")

  htmltools::div(
    class = "explicar-animation",
    htmltools::tags$h4(
      class = "explicar-verb-title",
      htmltools::tags$code(desc$verb),
      " \u2014 ",
      desc$description
    ),
    htmltools::div(
      class = "explicar-before-after",
      htmltools::div(class = "explicar-before", before_html),
      htmltools::div(class = "explicar-arrow", "\u2192"),
      htmltools::div(class = "explicar-after",  after_html)
    )
  )
}


#' Build a Vega-Lite JSON spec for a verb transformation
#' @noRd
.build_json <- function(desc) {
  tryCatch({
    data_before <- desc$data_before
    data_after  <- desc$data_after

    if (is.null(data_before)) return("{}")

    spec <- list(
      `$schema` = "https://vega.github.io/schema/vega-lite/v5.json",
      description = desc$description,
      data = list(values = .df_to_records(data_before)),
      mark = "point",
      encoding = list(
        x = list(field = names(data_before)[1], type = "nominal"),
        y = list(field = names(data_before)[2], type = "quantitative")
      ),
      title = paste0(desc$verb, ": ", desc$description)
    )

    jsonlite::toJSON(spec, auto_unbox = TRUE, pretty = TRUE)
  }, error = function(e) "{}")
}


#' Convert a dataframe to a list of records for JSON embedding
#' @noRd
.df_to_records <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  purrr::map(seq_len(nrow(df)), function(i) as.list(df[i, ]))
}


#' Render a dataframe as a simple HTML table
#' @noRd
.df_to_html_table <- function(df, max_rows = 8L, caption = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(htmltools::p(class = "explicar-no-data", "No data available"))
  }

  df_show <- if (nrow(df) > max_rows) df[seq_len(max_rows), , drop = FALSE] else df

  rows <- purrr::map(seq_len(nrow(df_show)), function(i) {
    htmltools::tags$tr(
      purrr::map(names(df_show), function(col) {
        htmltools::tags$td(as.character(df_show[[col]][i]))
      })
    )
  })

  header <- htmltools::tags$tr(
    purrr::map(names(df_show), function(col) {
      htmltools::tags$th(col)
    })
  )

  extra <- if (nrow(df) > max_rows) {
    htmltools::p(
      class = "explicar-truncated",
      glue::glue("... and {nrow(df) - max_rows} more rows ({nrow(df)} \u00d7 {ncol(df)} total)")
    )
  } else NULL

  htmltools::tagList(
    if (!is.null(caption)) htmltools::tags$p(class = "explicar-caption",
                                              htmltools::strong(caption)),
    htmltools::tags$table(
      class = "explicar-table",
      htmltools::tags$thead(header),
      htmltools::tags$tbody(rows)
    ),
    extra
  )
}


#' Generate illustrative toy data for a verb that has no real snapshot
#' @noRd
.illustrative_data <- function(verb_name) {
  # Return a generic small dataframe that works for most dplyr/tidyr demos
  switch(verb_name,
    pivot_longer = tibble::tibble(
      id = 1:4,
      Q1 = c(10, 20, 30, 40),
      Q2 = c(15, 25, 35, 45),
      Q3 = c(12, 22, 32, 42)
    ),
    pivot_wider = tibble::tibble(
      id      = rep(1:2, 3),
      quarter = rep(c("Q1", "Q2", "Q3"), each = 2),
      value   = c(10, 15, 20, 25, 12, 18)
    ),
    left_join = ,
    right_join = ,
    inner_join = ,
    full_join = tibble::tibble(
      id   = 1:4,
      name = c("Alice", "Bob", "Carol", "Dave"),
      dept = c("A", "B", "A", "C")
    ),
    # default
    tibble::tibble(
      id     = 1:6,
      group  = rep(c("A", "B"), 3),
      value  = c(3.1, 7.2, 1.5, 8.8, 4.0, 6.3),
      status = c("pass", "fail", "pass", "pass", "fail", "pass")
    )
  )
}
