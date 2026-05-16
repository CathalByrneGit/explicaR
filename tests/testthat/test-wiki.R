skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_wiki_project <- function() {
  dir <- tempfile("wiki_test_")
  dir.create(dir)
  writeLines(c(
    "#' Add two numbers",
    "#' @param x First number",
    "#' @param y Second number",
    "#' @return Sum",
    "add <- function(x, y) x + y"
  ), file.path(dir, "math.R"))
  dir
}

# ---------------------------------------------------------------------------
# explicar_wiki_build — basic build
# ---------------------------------------------------------------------------

test_that("explicar_wiki_build creates .explicar dir, explicar.duckdb, and wiki table", {
  proj <- make_wiki_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_wiki_build(proj, llm_chat = FALSE, quiet = TRUE)

  explicar_dir <- file.path(proj, ".explicar")
  db_path      <- file.path(explicar_dir, "explicar.duckdb")

  expect_true(dir.exists(explicar_dir))
  expect_true(file.exists(db_path))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true("wiki" %in% tables)
})

test_that("explicar_wiki_build inserts rows for each .R file found", {
  proj <- make_wiki_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_wiki_build(proj, llm_chat = FALSE, quiet = TRUE)

  db_path <- file.path(proj, ".explicar", "explicar.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  rows <- DBI::dbGetQuery(con, "SELECT * FROM wiki")
  expect_true(nrow(rows) > 0L)
})

test_that("explicar_wiki_build wiki content is non-empty character", {
  proj <- make_wiki_project()
  on.exit(unlink(proj, recursive = TRUE))

  explicar_wiki_build(proj, llm_chat = FALSE, quiet = TRUE)

  db_path <- file.path(proj, ".explicar", "explicar.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  rows <- DBI::dbGetQuery(con, "SELECT content FROM wiki")
  expect_true(all(nzchar(rows$content)))
  expect_true(is.character(rows$content))
})

# ---------------------------------------------------------------------------
# explicar_wiki_build — change detection
# ---------------------------------------------------------------------------

test_that("explicar_wiki_build with force=FALSE skips unchanged files on second call", {
  proj <- make_wiki_project()
  on.exit(unlink(proj, recursive = TRUE))

  # First build
  explicar_wiki_build(proj, llm_chat = FALSE, quiet = TRUE)

  # Second build — messages should contain "skip (unchanged)"
  msgs <- capture_messages(
    explicar_wiki_build(proj, llm_chat = FALSE, force = FALSE, quiet = FALSE)
  )
  expect_true(any(grepl("skip (unchanged)", msgs, fixed = TRUE)))
})

test_that("explicar_wiki_build with force=TRUE regenerates all pages", {
  proj <- make_wiki_project()
  on.exit(unlink(proj, recursive = TRUE))

  # First build
  explicar_wiki_build(proj, llm_chat = FALSE, quiet = TRUE)

  # Force rebuild — should NOT emit "skip (unchanged)"
  msgs <- capture_messages(
    explicar_wiki_build(proj, llm_chat = FALSE, force = TRUE, quiet = FALSE)
  )
  expect_false(any(grepl("skip (unchanged)", msgs, fixed = TRUE)))
  expect_true(any(grepl("generating", msgs)))
})

# ---------------------------------------------------------------------------
# .wiki_local_mermaid
# ---------------------------------------------------------------------------

test_that(".wiki_local_mermaid returns non-empty string starting with 'flowchart' when edges exist", {
  # Build a parse_result with outgoing edges from "math.R"
  parse_result <- list(
    nodes = data.frame(
      name = c("math.R", "utils.R"),
      type = c("script", "script"),
      file = c("math.R", "utils.R"),
      line = c(NA_integer_, NA_integer_),
      label = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = "math.R",
      to   = "utils.R",
      type = "sources",
      stringsAsFactors = FALSE
    )
  )

  result <- explicaR:::.wiki_local_mermaid("math.R", parse_result)

  expect_true(nzchar(result))
  expect_true(grepl("^flowchart", result))
})

test_that(".wiki_local_mermaid returns empty string when no edges involve the script", {
  parse_result <- list(
    nodes = data.frame(
      name = "math.R", type = "script", file = "math.R",
      line = NA_integer_, label = NA_character_,
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = character(0), to = character(0), type = character(0),
      stringsAsFactors = FALSE
    )
  )

  result <- explicaR:::.wiki_local_mermaid("math.R", parse_result)
  expect_equal(result, "")
})

# ---------------------------------------------------------------------------
# .wiki_is_current
# ---------------------------------------------------------------------------

test_that(".wiki_is_current returns FALSE when wiki table is empty", {
  db_path <- tempfile("wiki_test_empty_", fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  explicaR:::.ensure_wiki_table(con)

  result <- explicaR:::.wiki_is_current(con, "/some/file.R", 1234567890.0)
  expect_false(result)
})

test_that(".wiki_is_current returns TRUE when mtime matches stored value", {
  db_path <- tempfile("wiki_test_match_", fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  explicaR:::.ensure_wiki_table(con)

  mtime <- 1700000000.0
  DBI::dbWriteTable(con, "wiki",
    data.frame(file = "/some/file.R", model = "fallback",
               generated_at = mtime, last_modified = mtime,
               content = "hello", stringsAsFactors = FALSE),
    append = TRUE
  )

  result <- explicaR:::.wiki_is_current(con, "/some/file.R", mtime)
  expect_true(result)
})

test_that(".wiki_is_current returns FALSE when mtime differs from stored value", {
  db_path <- tempfile("wiki_test_diff_", fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  explicaR:::.ensure_wiki_table(con)

  mtime_stored <- 1700000000.0
  mtime_now    <- 1700001000.0
  DBI::dbWriteTable(con, "wiki",
    data.frame(file = "/some/file.R", model = "fallback",
               generated_at = mtime_stored, last_modified = mtime_stored,
               content = "hello", stringsAsFactors = FALSE),
    append = TRUE
  )

  result <- explicaR:::.wiki_is_current(con, "/some/file.R", mtime_now)
  expect_false(result)
})

# ---------------------------------------------------------------------------
# deep_research — signature / error tests (no live LLM)
# ---------------------------------------------------------------------------

test_that("deep_research errors with clear message when a non-chat object is passed", {
  skip_if_not_installed("ellmer")

  # Passing a plain list instead of a Chat object should cause a clear error
  # when it tries to call $chat()
  expect_error(
    deep_research(list(), "What does this code do?", max_iterations = 1L)
  )
})

test_that("deep_research has the expected formal arguments", {
  args <- formals(deep_research)
  expect_true("chat"           %in% names(args))
  expect_true("question"       %in% names(args))
  expect_true("max_iterations" %in% names(args))
})
