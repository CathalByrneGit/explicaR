# ---------------------------------------------------------------------------
# .is_remote_url
# ---------------------------------------------------------------------------

test_that(".is_remote_url returns TRUE for https GitHub URL", {
  expect_true(explicaR:::.is_remote_url("https://github.com/user/repo"))
})

test_that(".is_remote_url returns TRUE for URL without protocol (github.com/...)", {
  expect_true(explicaR:::.is_remote_url("github.com/user/repo"))
})

test_that(".is_remote_url returns TRUE for https GitLab URL", {
  expect_true(explicaR:::.is_remote_url("https://gitlab.com/user/repo"))
})

test_that(".is_remote_url returns FALSE for absolute local path", {
  expect_false(explicaR:::.is_remote_url("/path/to/project"))
})

test_that(".is_remote_url returns FALSE for '.' (current directory)", {
  expect_false(explicaR:::.is_remote_url("."))
})

test_that(".is_remote_url returns FALSE for relative local path", {
  expect_false(explicaR:::.is_remote_url("some/local/path"))
})

# ---------------------------------------------------------------------------
# .parse_repo_url
# ---------------------------------------------------------------------------

test_that(".parse_repo_url parses standard https GitHub URL correctly", {
  result <- explicaR:::.parse_repo_url("https://github.com/tidyverse/dplyr")

  expect_equal(result$host,  "github.com")
  expect_equal(result$owner, "tidyverse")
  expect_equal(result$repo,  "dplyr")
})

test_that(".parse_repo_url normalizes github.com/... shorthand to correct list", {
  result <- explicaR:::.parse_repo_url("github.com/tidyverse/dplyr")

  expect_equal(result$host,  "github.com")
  expect_equal(result$owner, "tidyverse")
  expect_equal(result$repo,  "dplyr")
})

test_that(".parse_repo_url strips .git suffix from URL", {
  result <- explicaR:::.parse_repo_url("https://github.com/tidyverse/dplyr.git")

  expect_equal(result$repo, "dplyr")
  expect_false(grepl("\\.git$", result$url))
})

test_that(".parse_repo_url returns correct url field for normalized URL", {
  result <- explicaR:::.parse_repo_url("https://github.com/tidyverse/dplyr")
  expect_match(result$url, "^https://github\\.com/tidyverse/dplyr$")
})

# ---------------------------------------------------------------------------
# .clone_cache_path
# ---------------------------------------------------------------------------

test_that(".clone_cache_path returns path under ~/.explicar/repos/<host>/<owner>/<repo>", {
  parsed <- list(host = "github.com", owner = "tidyverse", repo = "dplyr")
  result <- explicaR:::.clone_cache_path(parsed)

  expected_suffix <- file.path(".explicar", "repos", "github.com", "tidyverse", "dplyr")
  expect_match(result, gsub("\\.", "\\\\.", expected_suffix), fixed = FALSE)
  expect_true(startsWith(result, path.expand("~")))
})

# ---------------------------------------------------------------------------
# resolve_project — local paths
# ---------------------------------------------------------------------------

test_that("resolve_project returns normalizePath of an existing local directory", {
  proj <- tempfile("resolve_test_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  result <- resolve_project(proj)
  expect_equal(result, normalizePath(proj, mustWork = TRUE))
})

test_that("resolve_project errors for a non-existent local path", {
  fake_path <- tempfile("resolve_nonexistent_")
  # Do NOT create the directory — it should error
  expect_error(resolve_project(fake_path))
})
