skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")

# ---------------------------------------------------------------------------
# .node_to_text_embed
# ---------------------------------------------------------------------------

test_that(".node_to_text_embed returns string with 'function:', backtick name, file, and line", {
  nd <- data.frame(
    name = "clean_data", type = "function",
    file = "R/clean.R", line = 42L, label = "Removes NA rows",
    stringsAsFactors = FALSE
  )

  result <- explicaR:::.node_to_text_embed(nd)

  expect_true(is.character(result))
  expect_match(result, "function:")
  expect_match(result, "`clean_data`")
  expect_match(result, "R/clean.R")
  expect_match(result, "42")
})

test_that(".node_to_text_embed for variable node with no file returns 'variable:' and name, no file part", {
  nd <- data.frame(
    name = "my_var", type = "variable",
    file = NA_character_, line = NA_integer_, label = NA_character_,
    stringsAsFactors = FALSE
  )

  result <- explicaR:::.node_to_text_embed(nd)

  expect_match(result, "variable:")
  expect_match(result, "`my_var`")
  expect_false(grepl("\\[", result))  # no [file:line] part
})

test_that(".node_to_text_embed for script node with label includes em-dash separator and label text", {
  nd <- data.frame(
    name = "process.R", type = "script",
    file = "process.R", line = 1L, label = "Main processing script",
    stringsAsFactors = FALSE
  )

  result <- explicaR:::.node_to_text_embed(nd)

  expect_match(result, "—")  # em dash —
  expect_match(result, "Main processing script")
})

test_that(".node_to_text_embed for function node where label equals name does not include em-dash", {
  nd <- data.frame(
    name = "add", type = "function",
    file = "R/math.R", line = 5L, label = "add",
    stringsAsFactors = FALSE
  )

  result <- explicaR:::.node_to_text_embed(nd)

  expect_false(grepl("—", result))
})

# ---------------------------------------------------------------------------
# .parse_node_text — round-trip
# ---------------------------------------------------------------------------

test_that(".parse_node_text round-trips: parses text back to matching name/type/file/line/label", {
  nd <- data.frame(
    name = "clean_data", type = "function",
    file = "R/clean.R", line = 42L, label = "Removes NA rows",
    stringsAsFactors = FALSE
  )

  txt    <- explicaR:::.node_to_text_embed(nd)
  parsed <- explicaR:::.parse_node_text(txt)

  expect_equal(parsed$name[[1L]],  "clean_data")
  expect_equal(parsed$type[[1L]],  "function")
  expect_equal(parsed$file[[1L]],  "R/clean.R")
  expect_equal(parsed$line[[1L]],  42L)
  expect_equal(parsed$label[[1L]], "Removes NA rows")
})

test_that(".parse_node_text round-trips for variable node with no file", {
  nd <- data.frame(
    name = "my_var", type = "variable",
    file = NA_character_, line = NA_integer_, label = NA_character_,
    stringsAsFactors = FALSE
  )

  txt    <- explicaR:::.node_to_text_embed(nd)
  parsed <- explicaR:::.parse_node_text(txt)

  expect_equal(parsed$name[[1L]], "my_var")
  expect_equal(parsed$type[[1L]], "variable")
})

test_that(".parse_node_text handles unmatched text gracefully by returning NA name", {
  result <- explicaR:::.parse_node_text("this does not match the expected pattern!!")
  expect_true(is.na(result$name[[1L]]))
})

test_that(".parse_node_text handles empty string gracefully", {
  result <- explicaR:::.parse_node_text("")
  expect_true(is.na(result$name[[1L]]))
})

# ---------------------------------------------------------------------------
# explicar_semantic_retrieve — errors when no store exists
# ---------------------------------------------------------------------------

test_that("explicar_semantic_retrieve errors with clear message when no store exists", {
  skip_if_not_installed("ragnar")

  proj <- tempfile("embed_test_empty_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  expect_error(
    explicar_semantic_retrieve("anything", project_dir = proj),
    regexp = "No store found"
  )
})

# ---------------------------------------------------------------------------
# explicar_embed — graceful failure when Ollama is not reachable
# ---------------------------------------------------------------------------

test_that("explicar_embed returns invisible 0L when Ollama is not reachable", {
  skip_if_not_installed("ragnar")

  proj <- tempfile("embed_test_ollama_")
  dir.create(proj)
  on.exit(unlink(proj, recursive = TRUE))

  writeLines("x <- 1", file.path(proj, "foo.R"))
  explicar_index_build(proj, quiet = TRUE)

  # Ollama is not running in tests; explicar_embed should return 0L invisibly
  result <- withVisible(
    explicar_embed(proj,
                   ollama_url = "http://127.0.0.1:19999",
                   quiet      = TRUE)
  )
  expect_false(result$visible)
  expect_equal(result$value, 0L)
})
