skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")
skip_if_not_installed("jsonlite")

# ---------------------------------------------------------------------------
# .mcp_tool_definitions
# ---------------------------------------------------------------------------

test_that(".mcp_tool_definitions returns a list of exactly 4 tools", {
  tools <- explicaR:::.mcp_tool_definitions()
  expect_true(is.list(tools))
  expect_equal(length(tools), 4L)
})

test_that("each tool definition has name, description, and inputSchema fields", {
  tools <- explicaR:::.mcp_tool_definitions()
  for (tool in tools) {
    expect_true("name"        %in% names(tool))
    expect_true("description" %in% names(tool))
    expect_true("inputSchema" %in% names(tool))
  }
})

test_that("tool names are exactly 'search_code', 'query_graph', 'get_wiki', 'list_files'", {
  tools <- explicaR:::.mcp_tool_definitions()
  names_found <- vapply(tools, `[[`, character(1L), "name")
  expected    <- c("search_code", "query_graph", "get_wiki", "list_files")
  expect_equal(sort(names_found), sort(expected))
})

# ---------------------------------------------------------------------------
# .mcp_text
# ---------------------------------------------------------------------------

test_that(".mcp_text wraps a string in MCP content format", {
  result <- explicaR:::.mcp_text("hello world")

  expect_true(is.list(result))
  expect_true("content" %in% names(result))
  expect_true(is.list(result$content))
  expect_equal(length(result$content), 1L)
  expect_equal(result$content[[1L]]$type, "text")
  expect_equal(result$content[[1L]]$text, "hello world")
})

test_that(".mcp_text returns the correct nested list structure", {
  result <- explicaR:::.mcp_text("test message")
  expect_equal(result, list(content = list(list(type = "text", text = "test message"))))
})

# ---------------------------------------------------------------------------
# .mcp_query_graph — validation errors
# ---------------------------------------------------------------------------

test_that(".mcp_query_graph errors with 'Only SELECT' for non-SELECT SQL", {
  # The SELECT check fires before the NULL-connection check.
  # With a real connection, a non-SELECT statement is rejected with "Only SELECT".
  skip_if_not_installed("duckdb")
  db <- tempfile("mcp_select_test_", fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_error(
    explicaR:::.mcp_query_graph(list(sql = "DROP TABLE nodes"), con),
    "Only SELECT"
  )
})

test_that(".mcp_query_graph errors with 'Only SELECT' when INSERT is passed and con is not NULL", {
  # We need a real DBI connection to get past the NULL check
  skip_if_not_installed("duckdb")
  db <- tempfile("mcp_test_", fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_error(
    explicaR:::.mcp_query_graph(list(sql = "INSERT INTO foo VALUES (1)"), con),
    "Only SELECT"
  )
})

test_that(".mcp_query_graph errors with 'required' when sql is empty string", {
  expect_error(
    explicaR:::.mcp_query_graph(list(sql = ""), NULL),
    "required"
  )
})

test_that(".mcp_query_graph errors with 'required' when sql is missing from args", {
  expect_error(
    explicaR:::.mcp_query_graph(list(), NULL),
    "required"
  )
})

test_that(".mcp_query_graph errors with 'Only SELECT' when con is NULL and SQL is non-SELECT", {
  # The SELECT-only guard fires before the NULL connection check.
  expect_error(
    explicaR:::.mcp_query_graph(list(sql = "DROP TABLE nodes"), NULL),
    "Only SELECT"
  )
})

test_that(".mcp_query_graph with NULL con and SELECT sql errors with 'No database'", {
  expect_error(
    explicaR:::.mcp_query_graph(list(sql = "SELECT 1"), NULL),
    "No database"
  )
})
