test_that("explicar_graph returns a character string", {
  pr <- list(
    nodes = tibble::tibble(
      name = c("load.R", "raw_df", "filter"),
      type = c("script", "variable", "function"),
      file = c("load.R", "load.R", "load.R"),
      line = c(NA_integer_, 3L, 5L),
      label = c("load.R", "raw_df", "filter"),
      shape_info = NA_character_
    ),
    edges = tibble::tibble(
      from = c("load.R", "load.R"),
      to   = c("raw_df", "filter"),
      type = c("produces", "calls")
    ),
    verbs = tibble::tibble()
  )

  g <- explicar_graph(pr)
  expect_type(g, "character")
  expect_length(g, 1L)
  expect_true(grepl("flowchart", g))
})

test_that("explicar_graph includes all node names", {
  pr <- list(
    nodes = tibble::tibble(
      name = c("a.R", "my_df"),
      type = c("script", "variable"),
      file = c("a.R", "a.R"),
      line = c(NA_integer_, 1L),
      label = c("a.R", "my_df"),
      shape_info = NA_character_
    ),
    edges = tibble::tibble(
      from = "a.R", to = "my_df", type = "produces"
    ),
    verbs = tibble::tibble()
  )

  g <- explicar_graph(pr, level = "all")
  expect_true(grepl("a_R", g))
  expect_true(grepl("my_df", g))
})

test_that("explicar_graph respects direction argument", {
  pr <- list(
    nodes = tibble::tibble(name = "x", type = "script",
                           file = NA_character_, line = NA_integer_,
                           label = "x", shape_info = NA_character_),
    edges = tibble::tibble(from = character(), to = character(), type = character()),
    verbs = tibble::tibble()
  )
  expect_true(grepl("flowchart LR", explicar_graph(pr, direction = "LR")))
  expect_true(grepl("flowchart TD", explicar_graph(pr, direction = "TD")))
})

test_that("explicar_graph handles empty parse result gracefully", {
  pr <- list(
    nodes = tibble::tibble(name = character(), type = character(),
                           file = character(), line = integer(),
                           label = character(), shape_info = character()),
    edges = tibble::tibble(from = character(), to = character(), type = character()),
    verbs = tibble::tibble()
  )
  g <- explicar_graph(pr)
  expect_type(g, "character")
  expect_true(grepl("No nodes", g))
})

test_that("explicar_graph uses different shapes per node type", {
  pr <- list(
    nodes = tibble::tibble(
      name = c("s.R", "v", "fn", "src"),
      type = c("script", "variable", "function", "source"),
      file = NA_character_,
      line = NA_integer_,
      label = c("s.R", "v", "fn", "src"),
      shape_info = NA_character_
    ),
    edges = tibble::tibble(from = character(), to = character(), type = character()),
    verbs = tibble::tibble()
  )
  g <- explicar_graph(pr, level = "all")
  # script → ["…"]
  expect_true(grepl('\\["', g))
  # variable → ("…")
  expect_true(grepl('\\("', g))
  # function → {{"…"}}
  expect_true(grepl('\\{\\{', g))
  # source → [("…")]
  expect_true(grepl('\\[\\("', g))
})

test_that(".mermaid_id sanitises special characters", {
  expect_equal(explicaR:::.mermaid_id("my.df"),  "my_df")
  expect_equal(explicaR:::.mermaid_id("a-b c"),  "a_b_c")
  expect_equal(explicaR:::.mermaid_id("clean.R"), "clean_R")
})

test_that(".mermaid_escape replaces double-quotes", {
  expect_equal(explicaR:::.mermaid_escape('say "hi"'), "say 'hi'")
  expect_equal(explicaR:::.mermaid_escape("no quotes"), "no quotes")
})

test_that("explicar_graph calls are dashed arrows", {
  pr <- list(
    nodes = tibble::tibble(
      name = c("a.R", "fn"),
      type = c("script", "function"),
      file = NA_character_, line = NA_integer_,
      label = c("a.R", "fn"), shape_info = NA_character_
    ),
    edges = tibble::tibble(from = "a.R", to = "fn", type = "calls"),
    verbs = tibble::tibble()
  )
  g <- explicar_graph(pr, level = "all")
  # Calls edges use dashed arrows: -.->
  expect_true(grepl("-\\.->", g))
})

test_that("explicar_graph skips edges whose nodes are missing", {
  pr <- list(
    nodes = tibble::tibble(
      name = "a.R", type = "script",
      file = NA_character_, line = NA_integer_,
      label = "a.R", shape_info = NA_character_
    ),
    edges = tibble::tibble(
      from = c("a.R", "ghost"),
      to   = c("missing_node", "a.R"),
      type = "produces"
    ),
    verbs = tibble::tibble()
  )
  # Should not throw — invalid edges are silently skipped
  expect_no_error(explicar_graph(pr))
})
