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

# ---------------------------------------------------------------------------
# Docs schema tests (offline — no network calls)
# ---------------------------------------------------------------------------

test_that("docs and doc_embeddings tables exist after build", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  path <- explicar_index_build(proj, quiet = TRUE)
  con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true("docs"           %in% tables)
  expect_true("doc_embeddings" %in% tables)
})

test_that(".chunk_markdown splits by heading", {
  md <- paste(
    "# Overview",
    "This is the overview section with some text.",
    "",
    "## Details",
    "More detail here.",
    "",
    "## Another",
    "And another section.",
    sep = "\n"
  )
  chunks <- explicaR:::.chunk_markdown(md, "Test Page", max_chars = 50L)
  expect_true(length(chunks) >= 2L)
  expect_true(all(vapply(chunks, function(c) is.character(c$content), logical(1))))
  expect_true(all(vapply(chunks, function(c) is.character(c$context), logical(1))))
})

test_that(".chunk_markdown handles empty input", {
  chunks <- explicaR:::.chunk_markdown("", "Empty", max_chars = 500L)
  expect_equal(length(chunks), 0L)
})

test_that(".chunk_markdown splits on ## headings", {
  md <- "# Title\n\nIntro text.\n\n## Section A\n\nContent A.\n\n## Section B\n\nContent B."
  chunks <- explicaR:::.chunk_markdown(md, "Title", max_chars = 2000L)
  expect_gte(length(chunks), 2L)
  contexts <- vapply(chunks, `[[`, "", "context")
  expect_true(any(grepl("Section A", contexts)))
  expect_true(any(grepl("Section B", contexts)))
})

test_that(".chunk_markdown splits long sections at paragraph boundaries", {
  long_para <- paste(rep("word", 400), collapse = " ")
  md <- paste0("## Big Section\n\n", long_para, "\n\n", long_para)
  chunks <- explicaR:::.chunk_markdown(md, "Page", max_chars = 800L)
  expect_gte(length(chunks), 2L)
})

test_that("explicar_index_build_docs is idempotent without force", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  # Add a DESCRIPTION so .read_pkg_name() works
  writeLines(c("Package: testpkg", "Version: 0.1.0"),
             file.path(proj, "DESCRIPTION"))

  # Insert a fake doc row to simulate a previous build
  path <- explicar_index_build(proj, quiet = TRUE)
  con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "
    INSERT INTO docs VALUES (
      'local:testpkg/README/1', 'local:testpkg',
      'file:///some/README.md', 'README', 1,
      'README', 'some content', 0.0
    )
  ")
  DBI::dbDisconnect(con, shutdown = TRUE)

  msg <- capture_messages(
    explicar_index_build_docs(proj, quiet = FALSE)
  )
  expect_true(any(grepl("already indexed", msg)))
})

test_that("explicar_index_build_docs indexes README", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  writeLines(c("Package: testpkg", "Version: 0.1.0"),
             file.path(proj, "DESCRIPTION"))
  writeLines(c("# testpkg", "", "## Overview", "A test package.",
               "", "## Usage", "Call `foo()` to do things."),
             file.path(proj, "README.md"))

  path <- explicar_index_build(proj, quiet = TRUE)
  explicar_index_build_docs(proj, include = "readme", quiet = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  docs <- DBI::dbGetQuery(con, "SELECT * FROM docs WHERE page_title = 'README'")
  expect_true(nrow(docs) >= 1L)
  expect_true(all(docs$source == "local:testpkg"))
  expect_true(all(grepl("README", docs$url)))
})

test_that("explicar_index_build_docs indexes roxygen from R source", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  writeLines(c("Package: testpkg", "Version: 0.1.0"),
             file.path(proj, "DESCRIPTION"))
  dir.create(file.path(proj, "R"), showWarnings = FALSE)
  writeLines(c(
    "#' Compute the mean of a vector",
    "#'",
    "#' @param x A numeric vector.",
    "#' @return A single numeric value.",
    "my_mean <- function(x) mean(x)"
  ), file.path(proj, "R", "utils.R"))

  path <- explicar_index_build(proj, quiet = TRUE)
  explicar_index_build_docs(proj, include = "source", quiet = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  docs <- DBI::dbGetQuery(con, "SELECT * FROM docs WHERE page_title LIKE '%mean%'")
  expect_true(nrow(docs) >= 1L)
  expect_true(grepl("my_mean", paste(docs$content, collapse = " ")))
})
test_that("explicar_index_generate_wiki stops gracefully when Ollama is down", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  writeLines(c("Package: testpkg", "Version: 0.1.0"),
             file.path(proj, "DESCRIPTION"))

  explicar_index_build(proj, quiet = TRUE)

  # Ollama is not running in tests, so we expect a clear error message
  expect_error(
    explicar_index_generate_wiki(proj,
                                 model      = "llama3.2",
                                 ollama_url = "http://127.0.0.1:19999",
                                 quiet      = TRUE),
    regexp = "Cannot reach Ollama"
  )
})

test_that("explicar_index_generate_wiki is idempotent without force", {
  proj <- make_project()
  on.exit(unlink(proj, recursive = TRUE))

  writeLines(c("Package: testpkg", "Version: 0.1.0"),
             file.path(proj, "DESCRIPTION"))

  path <- explicar_index_build(proj, quiet = TRUE)

  # Pre-seed the wiki table
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "
    INSERT INTO docs VALUES (
      'wiki:testpkg:llama3.2/R/foo.R/1', 'wiki:testpkg:llama3.2',
      'file:///R/foo.R', 'Foo Module', 1,
      'Foo Module', 'content about foo', 0.0
    )
  ")
  DBI::dbDisconnect(con, shutdown = TRUE)

  msg <- capture_messages(
    explicar_index_generate_wiki(proj, model = "llama3.2",
                                 ollama_url = "http://127.0.0.1:19999",
                                 quiet = FALSE)
  )
  expect_true(any(grepl("already generated", msg)))
})
