test_that("shape_badge produces correct format", {
  expect_equal(shape_badge(mtcars), "32 \u00d7 11")
  expect_equal(shape_badge(cars),   "50 \u00d7 2")
  expect_true(is.na(shape_badge("not a dataframe")))
})

test_that("shape_badges works on a named list", {
  badges <- shape_badges(list(mtcars = mtcars, cars = cars))
  expect_equal(badges[["mtcars"]], "32 \u00d7 11")
  expect_equal(badges[["cars"]],   "50 \u00d7 2")
})

test_that("col_type_summary returns non-empty string for a dataframe", {
  s <- col_type_summary(mtcars)
  expect_type(s, "character")
  expect_gt(nchar(s), 0L)
})

test_that("shape_description combines badge and type summary", {
  d <- shape_description(mtcars)
  expect_true(grepl("\u00d7", d))
  expect_true(grepl("\\|", d))
})
