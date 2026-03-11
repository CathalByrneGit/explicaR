skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_project <- function() {
  dir <- tempfile("explicar_test_project_")
  dir.create(dir)
  # Write a minimal R script so explicar_parse() has something to process
  writeLines(
    c(
      "library(dplyr)",
      "raw_df   <- read.csv('data.csv')",
      "clean_df <- raw_df |> filter(!is.na(id))"
    ),
    file.path(dir, "clean.R")
  )
  dir
}

# ---------------------------------------------------------------------------
# Schema / build tests
# ---------------------------------------------------------------------------

test_that("explicar_index_build creates index file and tables", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  path <- explicar_index_build(proj, quiet = TRUE)

  expect_true(file.exists(path))
  expect_true(grepl("\\.duckdb$", path))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true(all(c("nodes", "edges", "verbs", "file_mtimes") %in% tables))
})

test_that("explicar_index_build returns invisibly the index path", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  result <- withVisible(explicar_index_build(proj, quiet = TRUE))
  expect_false(result$visible)
  expect_true(file.exists(result$value))
})

test_that("explicar_index_build is idempotent (second call is a no-op)", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_index_build(proj, quiet = TRUE)
  mtime1 <- file.info(explicaR:::.index_path(proj))$mtime

  Sys.sleep(0.05)
  explicar_index_build(proj, quiet = TRUE)
  mtime2 <- file.info(explicaR:::.index_path(proj))$mtime

  # File should not be rewritten if nothing changed
  expect_equal(mtime1, mtime2)
})

test_that("explicar_index_build force=TRUE re-indexes unchanged files", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_index_build(proj, quiet = TRUE)

  msg <- capture_messages(explicar_index_build(proj, force = TRUE, quiet = FALSE))
  expect_true(any(grepl("Indexing", msg)))
})

test_that("nodes table has expected columns after build", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  path <- explicar_index_build(proj, quiet = TRUE)
  con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  nodes <- DBI::dbGetQuery(con, "SELECT * FROM nodes LIMIT 1")
  expect_true(all(c("name", "type", "file", "line", "label") %in% names(nodes)))
})

# ---------------------------------------------------------------------------
# Retrieve tests
# ---------------------------------------------------------------------------

test_that("explicar_index_retrieve returns a tibble", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_index_build(proj, quiet = TRUE)
  result <- explicar_index_retrieve("clean", project_dir = proj)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("name", "type", "file", "label") %in% names(result)))
})

test_that("explicar_index_retrieve respects top_k", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_index_build(proj, quiet = TRUE)
  result <- explicar_index_retrieve(".", project_dir = proj, top_k = 2L)

  expect_lte(nrow(result), 2L)
})

test_that("explicar_index_retrieve type filter works", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_index_build(proj, quiet = TRUE)
  result <- explicar_index_retrieve(".", project_dir = proj, type = "script")

  expect_true(all(result$type == "script") || nrow(result) == 0L)
})

test_that("explicar_index_retrieve errors when no index exists", {
  proj <- tempfile("empty_proj_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  expect_error(
    explicar_index_retrieve("anything", project_dir = proj),
    "Run explicar_index_build"
  )
})

# ---------------------------------------------------------------------------
# Connect tests
# ---------------------------------------------------------------------------

test_that("explicar_index_connect returns a DBI connection", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_index_build(proj, quiet = TRUE)
  con <- explicar_index_connect(proj)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_true(inherits(con, "DBIConnection"))
  expect_true("nodes" %in% DBI::dbListTables(con))
})

test_that("explicar_index_connect errors when no index exists", {
  proj <- tempfile("empty_proj2_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  expect_error(
    explicar_index_connect(proj),
    "Run explicar_index_build"
  )
})
