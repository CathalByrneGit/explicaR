#' Data shape utilities
#'
#' Functions for computing and formatting data-shape badges that appear on
#' variable nodes in the pipeline graph.
#'
#' The shape badge format is `"nrow × ncol"`, e.g. `"980 × 8"`. It gives
#' readers a quick sense of how data transforms through the pipeline without
#' needing to click into animations.

#' Compute the shape badge string for a dataframe
#'
#' @param df A dataframe or tibble.
#'
#' @return A character string like `"980 × 8"`, or `NA` if `df` is not a
#'   dataframe.
#' @export
#'
#' @examples
#' shape_badge(mtcars)  # "32 × 11"
shape_badge <- function(df) {
  if (!is.data.frame(df)) return(NA_character_)
  paste0(nrow(df), " \u00d7 ", ncol(df))
}


#' Compute shape badges for a named list of dataframes
#'
#' @param dfs Named list of dataframes.
#'
#' @return A named character vector of shape badges.
#' @export
#'
#' @examples
#' shape_badges(list(cars = cars, mtcars = mtcars))
shape_badges <- function(dfs) {
  if (!is.list(dfs)) stop("`dfs` must be a named list.")
  vapply(dfs, shape_badge, character(1))
}


#' Summarise column types as a compact string
#'
#' Returns a string like `"int(3) dbl(2) chr(1)"` summarising the column
#' types in a dataframe. Useful for richer node tooltips.
#'
#' @param df A dataframe or tibble.
#'
#' @return A character string.
#' @export
#'
#' @examples
#' col_type_summary(mtcars)
col_type_summary <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) return("")

  types <- vapply(df, function(col) {
    cls <- class(col)[1]
    switch(cls,
      integer   = "int",
      numeric   = "dbl",
      double    = "dbl",
      character = "chr",
      logical   = "lgl",
      factor    = "fct",
      Date      = "date",
      POSIXct   = "dttm",
      POSIXlt   = "dttm",
      cls
    )
  }, character(1))

  counts <- sort(table(types), decreasing = TRUE)
  paste(names(counts), "(", counts, ")", sep = "", collapse = " ")
}


#' Full shape description for a tooltip (combines badge + type summary)
#'
#' @param df A dataframe.
#'
#' @return A character string, e.g. `"980 × 8 | dbl(5) int(2) chr(1)"`.
#' @export
shape_description <- function(df) {
  if (!is.data.frame(df)) return(NA_character_)
  badge   <- shape_badge(df)
  types   <- col_type_summary(df)
  if (nchar(types) > 0) paste0(badge, " | ", types) else badge
}
