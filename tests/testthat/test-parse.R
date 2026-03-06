test_that("explicar_parse returns empty result for empty directory", {
  tmp <- tempdir()
  result <- explicar_parse(tmp, pattern = "__no_match__\\.R$")
  expect_named(result, c("nodes", "edges", "verbs"))
  expect_equal(nrow(result$nodes), 0L)
  expect_equal(nrow(result$edges), 0L)
  expect_equal(nrow(result$verbs), 0L)
})

test_that("explicar_parse extracts verb records from a script", {
  tmp   <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  writeLines(c(
    "raw_df <- data.frame(x = 1:3, y = c('a','b','c'))",
    "clean_df <- dplyr::filter(raw_df, x > 1)"
  ), tmp)

  result <- explicar_parse(dirname(tmp), pattern = basename(tmp))
  # We expect at least the script node
  expect_true("script" %in% result$nodes$type)
})

test_that(".empty_parse_result has correct structure", {
  res <- explicaR:::.empty_parse_result()
  expect_named(res, c("nodes", "edges", "verbs"))
  expect_s3_class(res$nodes, "data.frame")
  expect_s3_class(res$edges, "data.frame")
  expect_s3_class(res$verbs, "data.frame")
})
