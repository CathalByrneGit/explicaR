#' Build a local documentation index from package sources
#'
#' Extracts documentation from the R package's own source files — roxygen2
#' comments in `R/*.R`, `man/*.Rd` (if generated), `README.md`, and
#' `vignettes/` — and stores the result as searchable chunks in the
#' `.explicar/index.duckdb` `docs` table.  Everything stays on disk: no
#' network calls, no external services, safe for private repositories.
#'
#' Sources are processed in priority order:
#' 1. `man/*.Rd` files (most structured; run `devtools::document()` to
#'    generate them from roxygen2 comments).
#' 2. Roxygen2 `#'` comment blocks extracted directly from `R/*.R` source
#'    (fallback when `man/` is empty or for unexported internals).
#' 3. `README.md` / `README.Rmd`.
#' 4. Any `*.Rmd` / `*.qmd` / `*.md` files under `vignettes/`.
#'
#' @param project_dir Path to the R project directory.  Default `"."`.
#' @param include Character vector of source types to include.  Any subset of
#'   `c("man", "source", "readme", "vignettes")`.
#' @param embed Embed each chunk via Ollama for vector retrieval.
#' @param embed_model Ollama embedding model.  Default `"nomic-embed-text"`.
#' @param ollama_url Ollama API base URL.
#' @param force Re-build even if docs are already stored.
#' @param quiet Suppress progress messages.
#'
#' @return Invisibly, the number of chunks stored.
#' @export
#'
#' @examples
#' \dontrun{
#' # Full local doc index (no network, works for private repos)
#' explicar_index_build_docs()
#'
#' # Only README + vignettes
#' explicar_index_build_docs(include = c("readme", "vignettes"))
#'
#' # Build + embed for semantic search
#' explicar_index_build_docs(embed = TRUE, embed_model = "nomic-embed-text")
#'
#' # After building, retrieve relevant docs
#' explicar_index_retrieve("how does verb animation work")
#' }
explicar_index_build_docs <- function(project_dir = ".",
                                      include     = c("man", "source",
                                                       "readme", "vignettes"),
                                      embed       = FALSE,
                                      embed_model = "nomic-embed-text",
                                      ollama_url  = "http://localhost:11434",
                                      force       = FALSE,
                                      quiet       = FALSE) {
  .require_duckdb()

  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  idx_path    <- .index_path(project_dir)

  if (!file.exists(idx_path)) {
    if (!quiet) message("No index found — building code index first...")
    explicar_index_build(project_dir, quiet = quiet)
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = idx_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .ensure_schema(con)

  pkg_name  <- .read_pkg_name(project_dir)
  source_id <- paste0("local:", pkg_name)

  if (!force) {
    n_existing <- DBI::dbGetQuery(
      con,
      sprintf("SELECT COUNT(*) AS n FROM docs WHERE source = '%s'",
              gsub("'", "''", source_id))
    )$n
    if (n_existing > 0L) {
      if (!quiet) {
        message("Docs already indexed (", n_existing, " chunks). ",
                "Use force = TRUE to rebuild.")
      }
      return(invisible(n_existing))
    }
  }

  all_rows <- list()

  # 1. man/*.Rd files (most authoritative when present)
  if ("man" %in% include) {
    rd_rows <- .index_rd_files(project_dir, source_id, pkg_name, quiet)
    all_rows <- c(all_rows, rd_rows)
    if (!quiet && length(rd_rows)) {
      message("  man/: ", length(rd_rows), " chunk(s) from ",
              length(unique(vapply(rd_rows, `[[`, "", "page_title"))), " Rd file(s)")
    }
  }

  # Track which function names are already covered by Rd docs
  rd_covered <- if ("man" %in% include) {
    unique(vapply(all_rows, function(r) r$page_title, ""))
  } else {
    character(0L)
  }

  # 2. Roxygen2 comments from R/*.R source (covers internals + undocumented)
  if ("source" %in% include) {
    src_rows <- .index_source_roxygen(project_dir, source_id, rd_covered, quiet)
    all_rows <- c(all_rows, src_rows)
    if (!quiet && length(src_rows)) {
      n_fns <- length(unique(vapply(src_rows, `[[`, "", "page_title")))
      message("  R/: ", length(src_rows), " chunk(s) from ", n_fns, " function(s)")
    }
  }

  # 3. README
  if ("readme" %in% include) {
    readme_rows <- .index_readme_doc(project_dir, source_id)
    all_rows <- c(all_rows, readme_rows)
    if (!quiet && length(readme_rows)) {
      message("  README: ", length(readme_rows), " chunk(s)")
    }
  }

  # 4. Vignettes
  if ("vignettes" %in% include) {
    vig_rows <- .index_vignettes_doc(project_dir, source_id, quiet)
    all_rows <- c(all_rows, vig_rows)
    if (!quiet && length(vig_rows)) {
      message("  vignettes/: ", length(vig_rows), " chunk(s)")
    }
  }

  if (length(all_rows) == 0L) {
    if (!quiet) message("No documentation found in ", project_dir)
    return(invisible(0L))
  }

  df <- do.call(rbind, lapply(all_rows, as.data.frame, stringsAsFactors = FALSE))

  DBI::dbExecute(
    con,
    sprintf("DELETE FROM docs WHERE source = '%s'", gsub("'", "''", source_id))
  )
  DBI::dbWriteTable(con, "docs", df, append = TRUE)

  if (!quiet) message("Stored ", nrow(df), " doc chunk(s) in index.")

  if (embed) {
    if (!requireNamespace("httr2", quietly = TRUE)) {
      if (!quiet) message("Skipping embeddings: 'httr2' not installed.")
    } else if (!ollama_available(embed_model, ollama_url)) {
      if (!quiet) message("Skipping embeddings: Ollama unavailable.")
    } else {
      if (!quiet) message("Embedding doc chunks...")
      .embed_doc_chunks(con, embed_model, ollama_url, quiet = quiet)
    }
  }

  invisible(nrow(df))
}

# ---------------------------------------------------------------------------
# 1. man/*.Rd parsing
# ---------------------------------------------------------------------------

.index_rd_files <- function(project_dir, source_id, pkg_name, quiet) {
  man_dir  <- file.path(project_dir, "man")
  rd_files <- if (dir.exists(man_dir)) {
    list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  } else {
    character(0L)
  }
  if (!length(rd_files)) return(list())

  fetched_at <- as.numeric(Sys.time())
  rows       <- list()

  for (rd_file in rd_files) {
    md <- .rd_to_markdown(rd_file, pkg_name)
    if (is.null(md)) next

    url    <- paste0("file://", rd_file)
    chunks <- .chunk_markdown(md$text, md$title)

    for (i in seq_along(chunks)) {
      rows[[length(rows) + 1L]] <- list(
        doc_id     = paste0(source_id, "/man/", basename(rd_file), "/", i),
        source     = source_id,
        url        = url,
        page_title = md$title,
        chunk_idx  = i,
        context    = chunks[[i]]$context,
        content    = chunks[[i]]$content,
        fetched_at = fetched_at
      )
    }
  }
  rows
}

# Convert a single .Rd file to a list(title, text).
.rd_to_markdown <- function(rd_file, pkg_name) {
  rd <- tryCatch(tools::parse_Rd(rd_file), error = function(e) NULL)
  if (is.null(rd)) return(NULL)

  # Extract title
  title <- .rd_tag_text(rd, "\\title")
  name  <- .rd_tag_text(rd, "\\name")
  if (!nchar(trimws(title))) title <- if (nchar(trimws(name))) name else
    tools::file_path_sans_ext(basename(rd_file))

  # Render to plain text and add lightweight markdown headings
  txt <- tryCatch(
    paste(capture.output(tools::Rd2txt(rd, package = pkg_name)), collapse = "\n"),
    error = function(e) NULL
  )
  if (is.null(txt)) return(NULL)

  # Promote "Section:\n" lines to "## Section" so the chunker splits on them
  txt <- gsub("(?m)^([A-Z][A-Za-z ]{2,30}):\\s*$", "## \\1", txt, perl = TRUE)
  txt <- paste0("# ", trimws(title), "\n\n", txt)

  list(title = trimws(title), text = txt)
}

# Extract text from a named Rd tag (e.g. "\\title", "\\name").
.rd_tag_text <- function(rd, tag) {
  for (el in rd) {
    if (identical(attr(el, "Rd_tag"), tag)) {
      return(trimws(.rd_node_text(el)))
    }
  }
  ""
}

.rd_node_text <- function(node) {
  if (is.character(node)) return(node)
  if (!is.list(node)) return("")
  paste(vapply(node, .rd_node_text, character(1L)), collapse = "")
}

# ---------------------------------------------------------------------------
# 2. Roxygen2 comment blocks from R/*.R source
# ---------------------------------------------------------------------------

.index_source_roxygen <- function(project_dir, source_id, already_covered, quiet) {
  r_files <- list.files(file.path(project_dir, "R"), pattern = "\\.R$",
                        full.names = TRUE)
  if (!length(r_files)) return(list())

  fetched_at <- as.numeric(Sys.time())
  rows       <- list()

  for (r_file in r_files) {
    docs <- .parse_roxygen_blocks(r_file)

    for (doc in docs) {
      # Skip if already covered by an Rd file with the same function name
      if (doc$name %in% already_covered) next

      md  <- .format_roxydoc(doc)
      url <- paste0("file://", r_file, "#L", doc$def_line)

      chunks <- .chunk_markdown(md$text, md$title)
      for (i in seq_along(chunks)) {
        rows[[length(rows) + 1L]] <- list(
          doc_id     = paste0(source_id, "/source/", basename(r_file),
                              "/", doc$name, "/", i),
          source     = source_id,
          url        = url,
          page_title = md$title,
          chunk_idx  = i,
          context    = chunks[[i]]$context,
          content    = chunks[[i]]$content,
          fetched_at = fetched_at
        )
      }
    }
  }
  rows
}

# Parse all roxygen2 blocks in an R source file.
# Returns a list of list(name, title, params, returns, description, def_line).
.parse_roxygen_blocks <- function(file_path) {
  lines <- tryCatch(readLines(file_path, warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) return(list())

  n       <- length(lines)
  results <- list()
  i       <- 1L

  while (i <= n) {
    if (!grepl("^#'", lines[i])) { i <- i + 1L; next }

    # Collect the contiguous roxygen block
    block_start <- i
    while (i <= n && grepl("^#'", lines[i])) i <- i + 1L
    block <- sub("^#'\\s?", "", lines[block_start:(i - 1L)])

    # Find the next non-blank line (should be the function/assignment definition)
    def_line <- i
    while (def_line <= n && !nchar(trimws(lines[def_line]))) def_line <- def_line + 1L

    fn_name <- NA_character_
    if (def_line <= n) {
      m <- regmatches(lines[def_line],
                      regexec("^`?([\\w.]+)`?\\s*(<-|=)\\s*function", lines[def_line],
                              perl = TRUE))[[1L]]
      if (length(m) >= 2L) fn_name <- m[2L]
    }

    if (!is.na(fn_name)) {
      results[[length(results) + 1L]] <- .parse_block(block, fn_name, def_line)
    }
  }
  results
}

.parse_block <- function(block, fn_name, def_line) {
  title       <- ""
  description <- character(0L)
  params      <- list()
  returns     <- character(0L)
  section     <- "description"
  cur_param   <- NULL

  flush_param <- function() {
    if (!is.null(cur_param)) params[[length(params) + 1L]] <<- cur_param
    cur_param <<- NULL
  }

  for (line in block) {
    if (grepl("^@title\\b", line)) {
      title <- trimws(sub("^@title\\s*", "", line))
    } else if (grepl("^@description\\b", line)) {
      section <- "description"
    } else if (grepl("^@param\\s", line)) {
      flush_param()
      m <- regmatches(line, regexec("^@param\\s+(\\S+)\\s*(.*)", line, perl = TRUE))[[1L]]
      cur_param <- list(name = m[2L], desc = m[3L])
      section   <- "param"
    } else if (grepl("^@return\\b", line)) {
      flush_param()
      returns <- c(returns, trimws(sub("^@return\\s*", "", line)))
      section <- "return"
    } else if (grepl("^@", line)) {
      flush_param()
      section <- "other"
    } else if (section == "description") {
      description <- c(description, line)
    } else if (section == "param" && !is.null(cur_param)) {
      cur_param$desc <- paste(cur_param$desc, trimws(line))
    } else if (section == "return") {
      returns <- c(returns, line)
    }
  }
  flush_param()

  list(name = fn_name, title = title, description = description,
       params = params, returns = returns, def_line = def_line)
}

# Format a parsed roxydoc record as markdown.
.format_roxydoc <- function(doc) {
  # Infer title: @title > first non-empty description line > function name
  title <- doc$title
  desc_clean <- trimws(paste(doc$description, collapse = "\n"))

  if (!nchar(title)) {
    first_nonempty <- Filter(function(l) nchar(trimws(l)) > 0L, doc$description)
    title <- if (length(first_nonempty)) trimws(first_nonempty[[1L]]) else doc$name
  }

  md_lines <- c(
    paste0("# ", title),
    "",
    paste0("**`", doc$name, "()`**"),
    ""
  )

  if (nchar(desc_clean)) md_lines <- c(md_lines, desc_clean, "")

  if (length(doc$params)) {
    md_lines <- c(md_lines, "## Parameters", "")
    for (p in doc$params) {
      md_lines <- c(md_lines, paste0("- `", p$name, "`: ", trimws(p$desc)))
    }
    md_lines <- c(md_lines, "")
  }

  returns_text <- trimws(paste(doc$returns, collapse = " "))
  if (nchar(returns_text)) {
    md_lines <- c(md_lines, "## Returns", "", returns_text, "")
  }

  list(title = title, text = paste(md_lines, collapse = "\n"))
}

# ---------------------------------------------------------------------------
# 3. README
# ---------------------------------------------------------------------------

.index_readme_doc <- function(project_dir, source_id) {
  candidates <- c("README.md", "README.Rmd", "README.rst")
  readme     <- Filter(file.exists,
                       file.path(project_dir, candidates))[[1L]]
  if (!length(readme)) return(list())
  readme <- readme[[1L]]

  text   <- paste(readLines(readme, warn = FALSE), collapse = "\n")
  chunks <- .chunk_markdown(text, "README")
  if (!length(chunks)) return(list())

  fetched_at <- as.numeric(Sys.time())
  url        <- paste0("file://", readme)

  lapply(seq_along(chunks), function(i) list(
    doc_id     = paste0(source_id, "/README/", i),
    source     = source_id,
    url        = url,
    page_title = "README",
    chunk_idx  = i,
    context    = chunks[[i]]$context,
    content    = chunks[[i]]$content,
    fetched_at = fetched_at
  ))
}

# ---------------------------------------------------------------------------
# 4. Vignettes
# ---------------------------------------------------------------------------

.index_vignettes_doc <- function(project_dir, source_id, quiet) {
  vig_dir <- file.path(project_dir, "vignettes")
  if (!dir.exists(vig_dir)) return(list())

  vig_files <- list.files(vig_dir, pattern = "\\.(Rmd|qmd|md)$",
                           full.names = TRUE, recursive = TRUE)
  if (!length(vig_files)) return(list())

  fetched_at <- as.numeric(Sys.time())
  rows       <- list()

  for (vf in vig_files) {
    lines <- tryCatch(readLines(vf, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) next

    title  <- .yaml_title(lines) %||% tools::file_path_sans_ext(basename(vf))
    text   <- paste(lines, collapse = "\n")
    chunks <- .chunk_markdown(text, title)
    url    <- paste0("file://", vf)

    for (i in seq_along(chunks)) {
      rows[[length(rows) + 1L]] <- list(
        doc_id     = paste0(source_id, "/vignettes/", basename(vf), "/", i),
        source     = source_id,
        url        = url,
        page_title = title,
        chunk_idx  = i,
        context    = chunks[[i]]$context,
        content    = chunks[[i]]$content,
        fetched_at = fetched_at
      )
    }
  }
  rows
}

# ---------------------------------------------------------------------------
# Shared utilities
# ---------------------------------------------------------------------------

# Read the Package: field from DESCRIPTION.
.read_pkg_name <- function(project_dir) {
  desc <- tryCatch(
    read.dcf(file.path(project_dir, "DESCRIPTION"), fields = "Package"),
    error = function(e) NULL
  )
  if (is.null(desc) || is.na(desc[1L, 1L])) return("")
  as.character(desc[1L, 1L])
}

# Extract the 'title:' field from YAML front matter.
.yaml_title <- function(lines) {
  if (!length(lines) || trimws(lines[1L]) != "---") return(NULL)
  end <- which(trimws(lines) == "---")
  if (length(end) < 2L) return(NULL)
  yaml <- lines[2L:(end[2L] - 1L)]
  hit  <- grep("^title:", yaml, value = TRUE, ignore.case = TRUE)
  if (!length(hit)) return(NULL)
  trimws(gsub('^title:\\s*["\']?|["\']?\\s*$', "", hit[1L]))
}
