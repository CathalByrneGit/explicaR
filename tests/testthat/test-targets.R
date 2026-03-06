test_that("targets_available returns FALSE when targets not installed", {
  # targets may or may not be installed in the test env
  result <- targets_available(tempdir())
  expect_type(result, "logical")
  expect_length(result, 1L)
})

test_that("explicar_mode returns 'instrumented' when no cache found", {
  mode <- explicar_mode(tempdir())
  expect_equal(mode, "instrumented")
})

test_that("explicar_targets returns empty list when no cache", {
  result <- explicar_targets(tempdir())
  expect_type(result, "list")
  expect_length(result, 0L)
})
