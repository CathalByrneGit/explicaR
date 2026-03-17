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

# ===========================================================================
# LLM-generated wiki  (explicar_index_generate_wiki)
# ===========================================================================

#' Generate a wiki from your package using a local LLM
#'
#' Sends each R source file (and optionally a cross-file architecture prompt)
#' to a locally-running [Ollama](https://ollama.com) model and asks it to
#' write a narrative wiki page in markdown.  The pages are stored in the
#' `.explicar/index.duckdb` `docs` table alongside any content added by
#' [explicar_index_build_docs()], so [explicar_index_retrieve()] can search
#' all of them together.
#'
#' Nothing leaves your machine.  All inference is done by the Ollama server
#' you already run locally (same server used for embeddings).
#'
#' @param project_dir Path to the R project.  Default `"."`.
#' @param model Ollama model name to use for generation, e.g.
#'   `"llama3.2"`, `"mistral"`, `"gemma3"`, `"phi4"`.
#' @param ollama_url Ollama API base URL.  Default `"http://localhost:11434"`.
#' @param include Character vector.  `"files"` generates one wiki page per
#'   `R/*.R` source file.  `"architecture"` generates a single cross-file
#'   architecture overview.  Default is both.
#' @param max_file_chars Maximum characters of source code sent per file
#'   (prevents very large files from overflowing the model's context).
#'   Default 6000.
#' @param force Overwrite existing wiki pages for this model/project.
#' @param quiet Suppress progress messages.
#'
#' @return Invisibly, the number of wiki pages stored.
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate wiki with any locally-available Ollama model
#' explicar_index_generate_wiki(model = "llama3.2")
#'
#' # Only the architecture overview page
#' explicar_index_generate_wiki(model = "mistral", include = "architecture")
#'
#' # Then retrieve across code + wiki together
#' explicar_index_retrieve("how does the animation pipeline work")
#' }
explicar_index_generate_wiki <- function(project_dir    = ".",
                                         model          = "llama3.2",
                                         ollama_url     = "http://localhost:11434",
                                         include        = c("files", "architecture"),
                                         max_file_chars = 6000L,
                                         force          = FALSE,
                                         quiet          = FALSE) {
  .require_duckdb()
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("Package 'httr2' is required for LLM generation.\n",
         "Install with: install.packages('httr2')", call. = FALSE)

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
  source_id <- paste0("wiki:", pkg_name, ":", model)

  if (!force) {
    n_existing <- DBI::dbGetQuery(
      con,
      sprintf("SELECT COUNT(*) AS n FROM docs WHERE source = '%s'",
              gsub("'", "''", source_id))
    )$n
    if (n_existing > 0L) {
      if (!quiet) {
        message("Wiki already generated (", n_existing, " pages). ",
                "Use force = TRUE to regenerate.")
      }
      return(invisible(n_existing))
    }
  }

  # Quick connectivity check
  if (!.ollama_ping(ollama_url)) {
    stop("Cannot reach Ollama at ", ollama_url,
         ".\nMake sure Ollama is running: https://ollama.com", call. = FALSE)
  }
  if (!quiet) message("Using model '", model, "' via ", ollama_url)

  r_files     <- list.files(file.path(project_dir, "R"), pattern = "\\.R$",
                             full.names = TRUE)
  all_rows    <- list()
  fetched_at  <- as.numeric(Sys.time())
  pages_done  <- 0L

  # ------------------------------------------------------------------
  # 1. One wiki page per source file
  # ------------------------------------------------------------------
  if ("files" %in% include) {
    for (r_file in r_files) {
      rel_name <- basename(r_file)
      if (!quiet) message("  Generating wiki page for ", rel_name, " ...")

      code <- .read_truncated(r_file, max_file_chars)
      prompt <- .wiki_file_prompt(rel_name, pkg_name, code)
      wiki_md <- .ollama_generate(prompt, model, ollama_url)

      if (is.null(wiki_md)) {
        if (!quiet) message("    [skipped — model returned empty response]")
        next
      }

      # Use the first # heading as page title; fall back to file name
      title  <- .first_heading(wiki_md) %||% tools::file_path_sans_ext(rel_name)
      chunks <- .chunk_markdown(wiki_md, title)
      url    <- paste0("file://", r_file)

      for (i in seq_along(chunks)) {
        all_rows[[length(all_rows) + 1L]] <- list(
          doc_id     = paste0(source_id, "/", rel_name, "/", i),
          source     = source_id,
          url        = url,
          page_title = title,
          chunk_idx  = i,
          context    = chunks[[i]]$context,
          content    = chunks[[i]]$content,
          fetched_at = fetched_at
        )
      }
      pages_done <- pages_done + 1L
    }
  }

  # ------------------------------------------------------------------
  # 2. Cross-file architecture overview
  # ------------------------------------------------------------------
  if ("architecture" %in% include && length(r_files)) {
    if (!quiet) message("  Generating architecture overview ...")

    # Summarise the package: file names + exported function names + first
    # roxygen description line from each file
    summaries <- vapply(r_files, function(f) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0L))
      fns   <- grep("^[a-zA-Z._][a-zA-Z0-9._]*\\s*(<-|=)\\s*function", lines,
                    value = TRUE, perl = FALSE)
      fn_names <- sub("\\s*(<-|=).*", "", trimws(fns))
      paste0("## ", basename(f), "\n",
             "Functions: ", paste(fn_names, collapse = ", "))
    }, character(1L), USE.NAMES = FALSE)

    arch_prompt <- .wiki_arch_prompt(pkg_name, summaries)
    arch_md     <- .ollama_generate(arch_prompt, model, ollama_url)

    if (!is.null(arch_md)) {
      title  <- .first_heading(arch_md) %||% paste0(pkg_name, " — Architecture")
      chunks <- .chunk_markdown(arch_md, title)

      for (i in seq_along(chunks)) {
        all_rows[[length(all_rows) + 1L]] <- list(
          doc_id     = paste0(source_id, "/architecture/", i),
          source     = source_id,
          url        = paste0("file://", project_dir),
          page_title = title,
          chunk_idx  = i,
          context    = chunks[[i]]$context,
          content    = chunks[[i]]$content,
          fetched_at = fetched_at
        )
      }
      pages_done <- pages_done + 1L
    }
  }

  if (length(all_rows) == 0L) {
    if (!quiet) message("No wiki pages generated.")
    return(invisible(0L))
  }

  df <- do.call(rbind, lapply(all_rows, as.data.frame, stringsAsFactors = FALSE))

  DBI::dbExecute(
    con,
    sprintf("DELETE FROM docs WHERE source = '%s'", gsub("'", "''", source_id))
  )
  DBI::dbWriteTable(con, "docs", df, append = TRUE)

  if (!quiet) {
    message("Stored ", nrow(df), " chunk(s) across ",
            pages_done, " generated wiki page(s).")
  }

  invisible(pages_done)
}

# ---------------------------------------------------------------------------
# Ollama generation helpers
# ---------------------------------------------------------------------------

# POST to /api/generate (non-streaming).  Returns the response text or NULL.
.ollama_generate <- function(prompt, model, ollama_url, timeout = 120L) {
  tryCatch({
    resp <- httr2::request(paste0(ollama_url, "/api/generate")) |>
      httr2::req_body_json(list(model  = model,
                                prompt = prompt,
                                stream = FALSE)) |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    trimws(as.character(body$response))
  }, error = function(e) NULL)
}

# Quick reachability check — returns TRUE if Ollama responds.
.ollama_ping <- function(ollama_url) {
  tryCatch({
    resp <- httr2::request(paste0(ollama_url, "/api/tags")) |>
      httr2::req_timeout(5L) |>
      httr2::req_perform()
    httr2::resp_status(resp) < 400L
  }, error = function(e) FALSE)
}

# Read a file and truncate to max_chars to avoid overflow.
.read_truncated <- function(file_path, max_chars) {
  raw <- paste(tryCatch(readLines(file_path, warn = FALSE),
                        error = function(e) character(0L)),
               collapse = "\n")
  if (nchar(raw) > max_chars) {
    raw <- paste0(substr(raw, 1L, max_chars),
                  "\n\n... [source truncated for brevity] ...")
  }
  raw
}

# Return the text of the first # or ## heading in a markdown string.
.first_heading <- function(md) {
  m <- regmatches(md, regexpr("(?m)^#{1,2}\\s+(.+)$", md, perl = TRUE))
  if (!length(m)) return(NULL)
  trimws(sub("^#{1,2}\\s+", "", m[[1L]]))
}

# ---------------------------------------------------------------------------
# Prompt templates
# ---------------------------------------------------------------------------

.wiki_file_prompt <- function(filename, pkg_name, code) {
  paste0(
    "You are a technical documentation writer for the R package '", pkg_name, "'.\n",
    "Write a clear, well-structured wiki page in markdown that explains the\n",
    "file '", filename, "' to a developer who is new to this codebase.\n\n",
    "Your page must include:\n",
    "1. A concise # heading that names the module (not the filename).\n",
    "2. An 'Overview' section: what this file does and why it exists.\n",
    "3. A 'Key Functions' section: for each exported or important function,\n",
    "   one short paragraph explaining its purpose, key parameters, and\n",
    "   what it returns.\n",
    "4. A 'How It Works' section: explain the internal flow or algorithm\n",
    "   at a high level (no need to repeat line-by-line code).\n",
    "5. Where relevant, a 'Usage Example' section with a short R snippet.\n\n",
    "Write in prose (not bullet lists). Use markdown ## for each section.\n",
    "Do not include the raw source code in your output.\n\n",
    "Source code of '", filename, "':\n\n```r\n", code, "\n```"
  )
}

.wiki_arch_prompt <- function(pkg_name, file_summaries) {
  files_block <- paste(file_summaries, collapse = "\n\n")
  paste0(
    "You are a software architect documenting the R package '", pkg_name, "'.\n",
    "Write a high-level architecture overview wiki page in markdown.\n\n",
    "Your page must include:\n",
    "1. A # heading: '", pkg_name, " — Architecture'.\n",
    "2. An 'Overview' section: what the package does and its main goals.\n",
    "3. A 'Module Map' section: describe each source file and its role,\n",
    "   and how the files depend on or call each other.\n",
    "4. A 'Data Flow' section: trace how data moves through the package\n",
    "   from user input to final output.\n",
    "5. A 'Key Design Decisions' section: note any patterns or abstractions\n",
    "   that a new contributor should understand.\n\n",
    "Write in clear prose with markdown ## headings for each section.\n\n",
    "Package structure:\n\n", files_block
  )
}
