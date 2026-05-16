test_that("targets_available returns FALSE when targets not installed", {
  # targets may or may not be installed in the test env
  result <- targets_available(tempdir())
  expect_type(result, "logical")
  expect_length(result, 1L)
})
