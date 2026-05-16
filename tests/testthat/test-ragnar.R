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

  dir.create(file.path(dir, "R"))
  writeLines(c(
    "#' Compute the mean of a vector",
    "#'",
    "#' @param x A numeric vector.",
    "#' @return A single numeric value.",
    "my_mean <- function(x) mean(x)"
  ), file.path(dir, "R", "utils.R"))

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
# .ragnar_chunk helper
# ---------------------------------------------------------------------------

test_that(".ragnar_chunk returns a character vector for markdown input", {
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
# explicar_ingest — store creation
# ---------------------------------------------------------------------------

test_that("explicar_ingest creates the unified explicar.duckdb store file", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ingest(proj, include = "readme", embed = FALSE, quiet = TRUE)

  db_path <- file.path(proj, ".explicar", "explicar.duckdb")
  expect_true(file.exists(db_path))
})

test_that("explicar_ingest indexes README chunks into the store", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ingest(proj, include = "readme", embed = FALSE, quiet = TRUE)

  db_path <- file.path(proj, ".explicar", "explicar.duckdb")
  store   <- ragnar::ragnar_store_connect(db_path, read_only = TRUE)
  rows    <- DBI::dbGetQuery(store@con,
    "SELECT * FROM chunks WHERE page_title = 'README'")
  expect_true(nrow(rows) >= 1L)
})

test_that("explicar_ingest indexes roxygen source chunks", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ingest(proj, include = "source", embed = FALSE, quiet = TRUE)

  db_path <- file.path(proj, ".explicar", "explicar.duckdb")
  store   <- ragnar::ragnar_store_connect(db_path, read_only = TRUE)
  rows    <- DBI::dbGetQuery(store@con,
    "SELECT * FROM chunks WHERE text LIKE '%my_mean%'")
  expect_true(nrow(rows) >= 1L)
})

test_that("explicar_ingest is idempotent without force — second call inserts 0 new chunks", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  n1 <- explicar_ingest(proj, include = "readme", embed = FALSE, quiet = TRUE)
  n2 <- explicar_ingest(proj, include = "readme", embed = FALSE, quiet = TRUE)

  expect_equal(n2, 0L)
})

test_that("explicar_ingest force = TRUE re-indexes chunks", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ingest(proj, include = "readme", embed = FALSE, quiet = TRUE)

  msgs <- capture_messages(
    explicar_ingest(proj, include = "readme", embed = FALSE,
                    force = TRUE, quiet = FALSE)
  )
  expect_true(any(grepl("chunk|Ingested", msgs, ignore.case = TRUE)))
})

# ---------------------------------------------------------------------------
# explicar_semantic_retrieve — retrieval
# ---------------------------------------------------------------------------

test_that("explicar_semantic_retrieve returns a data frame after ingest", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ingest(proj, include = "readme", embed = FALSE, quiet = TRUE)
  result <- explicar_semantic_retrieve("overview", project_dir = proj,
                                       top_k = 5L, bm25_only = TRUE)
  expect_true(is.data.frame(result))
  expect_true("text" %in% names(result))
})

test_that("explicar_semantic_retrieve respects top_k parameter", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_ingest(proj, include = c("readme", "source"),
                  embed = FALSE, quiet = TRUE)
  result <- explicar_semantic_retrieve(".", project_dir = proj,
                                       top_k = 2L, bm25_only = TRUE)
  expect_lte(nrow(result), 2L)
})

test_that("explicar_semantic_retrieve errors with clear message when no store exists", {
  proj <- tempfile("empty_ragnar_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  expect_error(
    explicar_semantic_retrieve("anything", project_dir = proj),
    "No store found"
  )
})

# ---------------------------------------------------------------------------
# Deprecated wrappers — still callable but emit a warning
# ---------------------------------------------------------------------------

test_that("explicar_ragnar_build emits a deprecation warning", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  expect_warning(
    explicar_ragnar_build(proj, include = "readme", embed = FALSE, quiet = TRUE),
    "deprecated"
  )
})

test_that("explicar_ragnar_build returns path to explicar.duckdb", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  path <- suppressWarnings(
    explicar_ragnar_build(proj, include = "readme", embed = FALSE, quiet = TRUE)
  )
  expect_true(grepl("explicar\\.duckdb$", path))
  expect_true(file.exists(path))
})

test_that("explicar_doc_retrieve emits a deprecation warning", {
  proj <- make_pkg_project()
  on.exit(unlink(proj, recursive = TRUE))

  suppressWarnings(
    explicar_ingest(proj, include = "readme", embed = FALSE, quiet = TRUE)
  )

  expect_warning(
    explicar_doc_retrieve("overview", project_dir = proj,
                          n = 3L, bm25_only = TRUE),
    "deprecated"
  )
})
