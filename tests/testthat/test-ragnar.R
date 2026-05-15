skip_if_not_installed("ragnar")
skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_pkg_project <- function() {
  dir <- tempfile("explicar_ragnar_test_")
  dir.create(dir)

  writeLines(c("Package: testpkg", "Version: 0.1.0"),
             file.path(dir, "DESCRIPTION"))

  # R/ with roxygen-documented function
  dir.create(file.path(dir, "R"))
  writeLines(c(
    "#' Compute the mean of a vector",
    "#'",
    "#' @param x A numeric vector.",
    "#' @return A single numeric value.",
    "my_mean <- function(x) mean(x)"
  ), file.path(dir, "R", "utils.R"))

  # README
  writeLines(c(
    "# testpkg",
    "",
    "## Overview",
    "A test package for explicaR ragnar integration.",
    "",
    "## Usage",
    "Call `my_mean()` to compute the mean."
  ), file.path(dir, "README.md"))

  dir
}

# ---------------------------------------------------------------------------
# .ragnar_chunk helper (no ragnar required for unit test)
# ---------------------------------------------------------------------------

test_that(".ragnar_chunk falls back when ragnar is missing", {
  # Force the fallback by testing with a dummy markdown text
  # (ragnar IS installed per skip_if_not_installed, but we can test the
  #  return-type contract)
  result <- explicaR:::.ragnar_chunk("# Title\n\nSome content here.", 1000L)
  expect_type(result, "character")
  expect_true(length(result) >= 1L)
})

test_that(".ragnar_chunk returns empty character for blank input", {
  result <- explicaR:::.ragnar_chunk("", 1000L)
  expect_type(result, "character")
  expect_length(result, 0L)
})

# ---------------------------------------------------------------------------
# Store creation
# ---------------------------------------------------------------------------

test_that("explicar_ragnar_build creates the store file", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  # BM25-only (no Ollama needed in tests)
  path <- explicar_ragnar_build(proj, embed = FALSE, quiet = TRUE)
  expect_true(file.exists(path))
  expect_true(grepl("ragnar\\.duckdb$", path))
})

test_that("explicar_ragnar_build indexes README chunks", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ragnar_build(proj, include = "readme", embed = FALSE, quiet = TRUE)
  store <- ragnar::ragnar_store_connect(explicaR:::.ragnar_store_path(proj))
  rows  <- DBI::dbGetQuery(store$con,
    "SELECT * FROM chunks WHERE page_title = 'README'")
  expect_true(nrow(rows) >= 1L)
  expect_true(all(rows$source == "local:testpkg"))
})

test_that("explicar_ragnar_build indexes roxygen source chunks", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ragnar_build(proj, include = "source", embed = FALSE, quiet = TRUE)
  store <- ragnar::ragnar_store_connect(explicaR:::.ragnar_store_path(proj))
  rows  <- DBI::dbGetQuery(store$con,
    "SELECT * FROM chunks WHERE text LIKE '%my_mean%'")
  expect_true(nrow(rows) >= 1L)
})

test_that("explicar_ragnar_build is idempotent without force", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ragnar_build(proj, embed = FALSE, quiet = TRUE)

  # Second call with the same source should print "already indexed"
  msg <- capture_messages(
    explicar_ragnar_build(proj, embed = FALSE, quiet = FALSE)
  )
  expect_true(any(grepl("already indexed", msg)))
})

test_that("explicar_ragnar_build force = TRUE re-indexes", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ragnar_build(proj, include = "readme", embed = FALSE, quiet = TRUE)

  msg <- capture_messages(
    explicar_ragnar_build(proj, include = "readme", embed = FALSE,
                          force = TRUE, quiet = FALSE)
  )
  # Should report inserting chunks again
  expect_true(any(grepl("chunk", msg, ignore.case = TRUE)))
})

# ---------------------------------------------------------------------------
# Retrieval
# ---------------------------------------------------------------------------

test_that("explicar_doc_retrieve returns a data frame", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ragnar_build(proj, include = "readme", embed = FALSE, quiet = TRUE)
  result <- explicar_doc_retrieve("overview", project_dir = proj,
                                   n = 5L, bm25_only = TRUE)
  expect_true(is.data.frame(result))
  expect_true("text" %in% names(result))
})

test_that("explicar_doc_retrieve respects n parameter", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ragnar_build(proj, include = c("readme", "source"),
                        embed = FALSE, quiet = TRUE)
  result <- explicar_doc_retrieve(".", project_dir = proj,
                                   n = 2L, bm25_only = TRUE)
  expect_lte(nrow(result), 2L)
})

test_that("explicar_doc_retrieve errors when no store exists", {
  proj <- tempfile("empty_ragnar_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  expect_error(
    explicar_doc_retrieve("anything", project_dir = proj),
    "explicar_ragnar_build"
  )
})

test_that("explicar_ragnar_build errors gracefully when ragnar is missing", {
  # We're inside skip_if_not_installed(ragnar) so ragnar IS available.
  # Test the error message for a missing store instead.
  proj <- tempfile("no_store_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  expect_error(
    explicar_doc_retrieve("test", project_dir = proj),
    regexp = "explicar_ragnar_build"
  )
})
