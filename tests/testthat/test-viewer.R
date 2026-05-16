# ── Helpers ───────────────────────────────────────────────────────────────────

minimal_parse_result <- function() {
  list(
    nodes = tibble::tibble(
      name       = c("load.R", "raw_df", "filter"),
      type       = c("script", "variable", "function"),
      file       = c("load.R", "load.R", "load.R"),
      line       = c(NA_integer_, 3L, 5L),
      label      = c("load.R", "raw_df", "filter"),
      shape_info = NA_character_
    ),
    edges = tibble::tibble(
      from = c("load.R", "load.R"),
      to   = c("raw_df", "filter"),
      type = c("produces", "calls")
    ),
    verbs = tibble::tibble(
      file = character(), line = integer(), fn_name = character(),
      input_var = character(), output_var = character(),
      args = list(), pkg = character()
    )
  )
}

# ── generate_viewer ────────────────────────────────────────────────────────────

test_that("generate_viewer creates an HTML file", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  result <- generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  expect_true(file.exists(out))
  expect_true(file.info(out)$size > 0L)
})

test_that("generate_viewer returns invisibly", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  ret <- withVisible(generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE))
  expect_false(ret$visible)
  expect_equal(ret$value, out)
})

test_that("generate_viewer embeds the title", {
  pr    <- minimal_parse_result()
  out   <- tempfile(fileext = ".html")
  title <- "My Test Pipeline"
  on.exit(unlink(out))

  generate_viewer(pr, title = title, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl(title, html, fixed = TRUE))
})

test_that("generate_viewer contains Mermaid syntax", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("mermaid", html, ignore.case = TRUE))
  expect_true(grepl("flowchart", html))
})

test_that("generate_viewer embeds the Mermaid graph data", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Node names should appear in the embedded Mermaid block
  expect_true(grepl("load_R", html))   # sanitised Mermaid ID
  expect_true(grepl("raw_df", html))
})

test_that("generate_viewer contains node-data JSON", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Embedded JSON must include the node names
  expect_true(grepl("ex-node-data", html))
  expect_true(grepl("raw_df", html))
})

test_that("generate_viewer embeds stats (nodes · edges · verb calls)", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("nodes", html))
  expect_true(grepl("edges", html))
})

test_that("generate_viewer respects direction argument", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_viewer(pr, direction = "LR", output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("flowchart LR", html))
})

test_that("generate_viewer works with an empty parse result", {
  pr  <- explicaR:::.empty_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  expect_no_error(
    generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)
  )
  expect_true(file.exists(out))
})

test_that("generate_viewer HTML contains the generated-at timestamp", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  before <- format(Sys.time(), "%Y-%m-%d")
  generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl(before, html))
})

test_that("generate_viewer replaces all template placeholders", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # No un-substituted {{...}} placeholders should remain
  expect_false(grepl("\\{\\{[A-Z_]+\\}\\}", html))
})

# ── generate_wasm_viewer ──────────────────────────────────────────────────────

test_that("generate_wasm_viewer creates an HTML file", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  result <- generate_wasm_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  expect_true(file.exists(out))
  expect_true(file.info(out)$size > 0L)
})

test_that("generate_wasm_viewer returns invisibly", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  ret <- withVisible(generate_wasm_viewer(pr, output_file = out, open = FALSE, quiet = TRUE))
  expect_false(ret$visible)
  expect_equal(ret$value, out)
})

test_that("generate_wasm_viewer replaces all template placeholders", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_wasm_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("\\{\\{[A-Z_]+\\}\\}", html))
})

test_that("generate_wasm_viewer embeds node and edge JSON", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_wasm_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("ex-node-data", html))
  expect_true(grepl("ex-edge-data", html))
  expect_true(grepl("raw_df", html))
})

test_that("generate_wasm_viewer contains DuckDB-WASM reference", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_wasm_viewer(pr, output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("duckdb-wasm", html, ignore.case = TRUE))
})

test_that("generate_wasm_viewer respects direction argument", {
  pr  <- minimal_parse_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))

  generate_wasm_viewer(pr, direction = "LR", output_file = out, open = FALSE, quiet = TRUE)

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("flowchart LR", html))
})

# ── explicar_config ────────────────────────────────────────────────────────────

test_that("explicar_config returns a list with expected keys", {
  cfg <- explicar_config()
  expect_type(cfg, "list")
  expect_true("exclude_dirs"       %in% names(cfg))
  expect_true("exclude_extensions" %in% names(cfg))
  expect_true("max_file_size_kb"   %in% names(cfg))
  expect_true("default_languages"  %in% names(cfg))
})

test_that("explicar_config exclude_dirs includes .git", {
  cfg <- explicar_config()
  expect_true(".git" %in% cfg$exclude_dirs)
})

test_that("explicar_config project override is merged", {
  tmp <- tempfile()
  dir.create(file.path(tmp, ".explicar"), recursive = TRUE)
  writeLines('exclude_dirs:\n  - my_custom_dir', file.path(tmp, ".explicar", "config.yml"))
  on.exit(unlink(tmp, recursive = TRUE))

  skip_if_not_installed("yaml")
  cfg <- explicar_config(project_dir = tmp)
  expect_true("my_custom_dir" %in% cfg$exclude_dirs)
})

# ── Python parse support ───────────────────────────────────────────────────────

test_that("explicar_parse with languages='python' parses .py files", {
  tmp_dir <- tempfile("py_proj_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "import pandas as pd",
    "",
    "raw_df = pd.read_csv('data.csv')",
    "",
    "def clean(df):",
    "    return df.dropna()"
  ), file.path(tmp_dir, "pipeline.py"))

  result <- explicar_parse(tmp_dir, languages = "python")

  expect_named(result, c("nodes", "edges", "verbs"))
  expect_true("script" %in% result$nodes$type)
  # Should find pipeline.py as a script node
  expect_true(any(grepl("pipeline\\.py", result$nodes$name)))
})

test_that("explicar_parse python finds function definitions", {
  tmp_dir <- tempfile("py_fn_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "def transform(df):",
    "    return df",
    "",
    "def aggregate(df):",
    "    return df.sum()"
  ), file.path(tmp_dir, "funcs.py"))

  result <- explicar_parse(tmp_dir, languages = "python")
  fn_names <- result$nodes$name[result$nodes$type == "function"]
  expect_true("transform" %in% fn_names)
  expect_true("aggregate" %in% fn_names)
})

test_that("explicar_parse python finds top-level assignments", {
  tmp_dir <- tempfile("py_var_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "raw_df = load_data()",
    "clean_df = raw_df.dropna()"
  ), file.path(tmp_dir, "work.py"))

  result <- explicar_parse(tmp_dir, languages = "python")
  var_names <- result$nodes$name[result$nodes$type == "variable"]
  expect_true("raw_df" %in% var_names)
  expect_true("clean_df" %in% var_names)
})

test_that("explicar_parse python finds import statements as source nodes", {
  tmp_dir <- tempfile("py_imp_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(c(
    "import pandas",
    "from sklearn.preprocessing import StandardScaler"
  ), file.path(tmp_dir, "model.py"))

  result <- explicar_parse(tmp_dir, languages = "python")
  src_names <- result$nodes$name[result$nodes$type == "source"]
  expect_true("pandas" %in% src_names)
})

test_that("explicar_parse languages=c('r','python') parses both file types", {
  tmp_dir <- tempfile("mixed_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(
    c("raw_df <- data.frame(x = 1:3)"),
    file.path(tmp_dir, "load.R")
  )
  writeLines(
    c("import pandas", "df = load_data()"),
    file.path(tmp_dir, "process.py")
  )

  result <- explicar_parse(tmp_dir, languages = c("r", "python"))

  script_names <- result$nodes$name[result$nodes$type == "script"]
  expect_true(any(grepl("load\\.R",   script_names)))
  expect_true(any(grepl("process\\.py", script_names)))
})
