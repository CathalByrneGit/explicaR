test_that("verb_filter returns correct descriptor structure", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  desc <- verb_filter(df, "x > 2", output_var = "filtered_df")
  expect_equal(desc$verb, "filter")
  expect_equal(desc$pkg,  "dplyr")
  expect_true(grepl("x > 2", desc$description))
  expect_s3_class(desc$data_before, "data.frame")
})

test_that("verb_pivot_longer returns correct descriptor structure", {
  df <- data.frame(id = 1:3, Q1 = 10:12, Q2 = 20:22)
  desc <- verb_pivot_longer(df, "starts_with('Q')",
                             names_to = "quarter", values_to = "score")
  expect_equal(desc$verb,      "pivot_longer")
  expect_equal(desc$names_to,  "quarter")
  expect_equal(desc$values_to, "score")
})

test_that("verb_descriptor dispatches correctly", {
  df <- data.frame(id = 1:3, val = c(1.1, 2.2, 3.3))
  rec <- list(
    fn_name    = "filter",
    file       = "test.R",
    line       = 5L,
    input_var  = "df",
    output_var = "df2",
    args       = list(c("df", "val > 1")),
    pkg        = "dplyr"
  )
  desc <- verb_descriptor(rec, snapshots = list(df = df))
  expect_equal(desc$verb, "filter")
})

test_that("verb_descriptor returns NULL for unknown verb", {
  rec <- list(
    fn_name    = "not_a_real_verb_xyz",
    file       = "test.R",
    line       = 1L,
    input_var  = NA,
    output_var = NA,
    args       = list(),
    pkg        = NA_character_
  )
  expect_null(verb_descriptor(rec))
})
