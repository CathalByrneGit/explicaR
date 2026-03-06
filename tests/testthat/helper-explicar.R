# Helper: build a minimal parse result for testing
make_parse_result <- function() {
  nodes <- tibble::tibble(
    name       = c("clean.R", "raw_df", "clean_df"),
    type       = c("script",  "variable", "variable"),
    file       = c("clean.R", "clean.R",  "clean.R"),
    line       = c(NA_integer_, NA_integer_, 5L),
    label      = c("clean.R", "raw_df", "clean_df"),
    shape_info = c(NA_character_, "100 \u00d7 5", "80 \u00d7 5")
  )
  edges <- tibble::tibble(
    from = c("clean.R",  "raw_df"),
    to   = c("clean_df", "clean.R"),
    type = c("produces", "consumes")
  )
  verbs <- tibble::tibble(
    file       = "clean.R",
    line       = 5L,
    fn_name    = "filter",
    input_var  = "raw_df",
    output_var = "clean_df",
    args       = list(c("raw_df", "!is.na(id)")),
    pkg        = "dplyr"
  )
  list(nodes = nodes, edges = edges, verbs = verbs)
}
