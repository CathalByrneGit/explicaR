# ── Static documentation site builder ─────────────────────────────────────────

#' Build a static documentation site from explicaR wiki pages
#'
#' Reads the markdown wiki pages written by [explicar_wiki_build()], renders
#' them to self-contained HTML, and produces a searchable static site with
#' per-script pages, upstream/downstream navigation, and source provenance
#' links back to the original file (on GitHub, GitLab, Gitea, or locally).
#'
#' The `.md` files in `wiki_dir` are the editable source of truth: edit them
#' freely then re-run `explicar_site_build()` to regenerate the HTML without
#' re-calling the LLM.
#'
#' @param parse_result Output from [explicar_parse()], used for graph links.
#' @param project_dir  Project root directory. Default `"."`.
#' @param wiki_dir     Directory containing `.md` wiki files.
#'   Default `"<project_dir>/explicar/wiki"`.
#' @param output_dir   Output directory for the generated site.
#'   Default `"<project_dir>/explicar/docs"`.
#' @param title        Site title. Default: basename of `project_dir`.
#' @param branch       Git branch for provenance links. Default `"main"`.
#' @param remote_url   Remote URL override.  `NULL` = auto-detect (tries
#'   git2r then git CLI); `FALSE` = local `file://` links only; a string
#'   overrides detection (supports GitHub, GitLab, Gitea/Forgejo).
#' @param open         Open the index page in the browser when done.
#'   Default `TRUE`.
#' @param quiet        Suppress progress messages. Default `FALSE`.
#'
#' @return Invisibly, the path to the generated `index.html`.
#' @export
#'
#' @examples
#' \dontrun{
#' pr <- explicar_parse(".")
#' explicar_wiki_build(".", llm_chat = ellmer::chat_anthropic())
#' explicar_site_build(pr)
#'
#' # Rebuild HTML only after editing .md files — no LLM needed
#' explicar_site_build(pr)
#'
#' # Explicit remote (e.g. Gitea self-hosted)
#' explicar_site_build(pr, remote_url = "https://git.myorg.com/team/repo")
#' }
explicar_site_build <- function(parse_result,
                                project_dir = ".",
                                wiki_dir    = NULL,
                                output_dir  = NULL,
                                title       = NULL,
                                branch      = "main",
                                remote_url  = NULL,
                                open        = TRUE,
                                quiet       = FALSE) {
  project_dir <- normalizePath(project_dir, mustWork = TRUE)
  wiki_dir    <- wiki_dir   %||% file.path(project_dir, "explicar", "wiki")
  output_dir  <- output_dir %||% file.path(project_dir, "explicar", "docs")
  title       <- title      %||% basename(project_dir)

  if (!dir.exists(wiki_dir))
    stop("Wiki directory not found: ", wiki_dir,
         "\nRun explicar_wiki_build() first.", call. = FALSE)
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  md_files <- list.files(wiki_dir, pattern = "\\.md$", full.names = TRUE)
  if (length(md_files) == 0L) {
    if (!quiet) message("No .md files found in: ", wiki_dir)
    return(invisible(file.path(output_dir, "index.html")))
  }

  # Resolve provenance base URL (NULL = auto-detect, FALSE = local only)
  effective_remote <- if (isFALSE(remote_url)) {
    FALSE
  } else if (!is.null(remote_url)) {
    .normalise_remote(remote_url)
  } else {
    .detect_remote(project_dir) # may return NULL → falls back to file://
  }

  if (!quiet) {
    if (isFALSE(effective_remote)) {
      message("Provenance: local file:// links")
    } else if (!is.null(effective_remote)) {
      message("Provenance: ", effective_remote, "  branch: ", branch)
    } else {
      message("Provenance: no git remote found, using file:// links")
    }
  }

  page_tmpl  <- .read_site_tmpl("site_page.html")
  index_tmpl <- .read_site_tmpl("site_index.html")

  # Build page data for all scripts
  pages <- lapply(md_files, function(md_path) {
    .build_page_data(md_path, parse_result, project_dir,
                     effective_remote, branch)
  })
  names(pages) <- vapply(pages, `[[`, character(1L), "html_name")

  # Search index embedded in every page
  search_list <- lapply(pages, function(p) {
    list(title = p$file_name, url = p$html_name,
         excerpt = p$excerpt,  text = p$plain_text)
  })
  search_json <- jsonlite::toJSON(search_list, auto_unbox = TRUE)

  # Per-script pages
  for (p in pages) {
    if (!quiet) message("  ", p$html_name)
    html <- page_tmpl
    html <- gsub("{{SITE_TITLE}}",       .html_esc(title),         html, fixed = TRUE)
    html <- gsub("{{PAGE_TITLE}}",       .html_esc(p$file_name),   html, fixed = TRUE)
    html <- gsub("{{PROVENANCE_HTML}}",  p$provenance_html,        html, fixed = TRUE)
    html <- gsub("{{GRAPH_LINKS_HTML}}", p$graph_links_html,       html, fixed = TRUE)
    html <- gsub("{{CONTENT_HTML}}",     p$content_html,           html, fixed = TRUE)
    html <- gsub("{{SIDEBAR_HTML}}",     .build_sidebar(pages, p$html_name), html, fixed = TRUE)
    html <- gsub("{{SEARCH_DATA_JSON}}", search_json,              html, fixed = TRUE)
    writeLines(html, file.path(output_dir, p$html_name))
  }

  # Index page
  cards_html <- .build_cards(pages)
  idx <- index_tmpl
  idx <- gsub("{{SITE_TITLE}}",       .html_esc(title),  idx, fixed = TRUE)
  idx <- gsub("{{SCRIPT_COUNT}}",     length(pages),     idx, fixed = TRUE)
  idx <- gsub("{{CARDS_HTML}}",       cards_html,        idx, fixed = TRUE)
  idx <- gsub("{{SEARCH_DATA_JSON}}", search_json,       idx, fixed = TRUE)
  writeLines(idx, file.path(output_dir, "index.html"))

  if (!quiet) message("Site built: ", file.path(output_dir, "index.html"),
                      " (", length(pages), " pages)")
  if (open && interactive()) utils::browseURL(file.path(output_dir, "index.html"))
  invisible(file.path(output_dir, "index.html"))
}


# ── Per-page data assembly ─────────────────────────────────────────────────────

.build_page_data <- function(md_path, parse_result, project_dir,
                              remote_url, branch) {
  md_base     <- tools::file_path_sans_ext(basename(md_path))  # e.g. "clean"
  file_name_r <- paste0(md_base, ".R")
  file_name_py <- paste0(md_base, ".py")

  # Locate the actual source file in the project
  src_r  <- file.path(project_dir, file_name_r)
  src_py <- file.path(project_dir, file_name_py)
  source_file <- if (file.exists(src_r))  src_r  else
                 if (file.exists(src_py)) src_py else NULL
  file_name   <- if (!is.null(source_file)) basename(source_file) else file_name_r

  # Provenance HTML
  prov_url  <- .resolve_provenance_url(
    if (!is.null(source_file)) source_file else file.path(project_dir, file_name),
    project_dir, branch, remote_url
  )
  rel_path  <- if (!is.null(source_file)) {
    proj_abs <- normalizePath(project_dir, mustWork = FALSE)
    sub(paste0("^", proj_abs, "/?"), "", normalizePath(source_file, mustWork = FALSE))
  } else file_name
  prov_html <- .provenance_html(rel_path, prov_url, remote_url)

  # Upstream / downstream script links from the parse graph
  graph_links_html <- if (!is.null(parse_result))
    .graph_links_html(file_name, parse_result)
  else ""

  # Render markdown
  md_content   <- paste(readLines(md_path, warn = FALSE), collapse = "\n")
  content_html <- .md_to_html(md_content)

  # Plain text for search (strip HTML tags)
  plain <- gsub("<[^>]+>", " ", content_html)
  plain <- trimws(gsub("\\s+", " ", plain))

  list(
    file_name        = file_name,
    html_name        = paste0(md_base, ".html"),
    provenance_html  = prov_html,
    graph_links_html = graph_links_html,
    content_html     = content_html,
    plain_text       = plain,
    excerpt          = substr(plain, 1L, 220L)
  )
}


# ── HTML fragment builders ─────────────────────────────────────────────────────

.provenance_html <- function(rel_path, url, remote_url) {
  path_html <- paste0('<span class="prov-icon">&#x1F4C1;</span> ',
                      .html_esc(rel_path))
  link_html <- if (!isFALSE(remote_url) && !grepl("^file://", url)) {
    label <- if (grepl("github\\.com", url))     "GitHub &#x2197;"
             else if (grepl("gitlab", url))      "GitLab &#x2197;"
             else                                "Source &#x2197;"
    paste0('<span class="prov-sep">·</span>',
           '<a href="', url, '" target="_blank" rel="noopener">', label, '</a>')
  } else {
    paste0('<span class="prov-sep">·</span>',
           '<a href="', url, '">', .html_esc(rel_path), '</a>')
  }
  paste0(path_html, " ", link_html)
}

.graph_links_html <- function(file_name, parse_result) {
  edges <- parse_result$edges

  # Scripts that PRODUCE things this script CONSUMES (upstream)
  consumed  <- edges[edges$type == "consumes" & edges$to   == file_name, "from", drop = TRUE]
  upstream  <- unique(edges[edges$type == "produces" &
                              edges$from != file_name &
                              edges$to %in% consumed, "from", drop = TRUE])
  upstream  <- setdiff(upstream, file_name)

  # Scripts that CONSUME things this script PRODUCES (downstream)
  produced   <- edges[edges$type == "produces" & edges$from == file_name, "to", drop = TRUE]
  downstream <- unique(edges[edges$type == "consumes" &
                               edges$from != file_name &
                               edges$to %in% produced, "to", drop = TRUE])
  downstream <- setdiff(downstream, file_name)

  parts <- character(0L)

  # Only link to scripts (nodes in the graph that look like .R / .py files)
  is_script <- function(x) grepl("\\.(R|py)$", x, ignore.case = TRUE)

  up_scripts   <- upstream[is_script(upstream)]
  down_scripts <- downstream[is_script(downstream)]

  if (length(up_scripts) > 0L) {
    up_links <- vapply(head(up_scripts, 5L), function(s) {
      slug <- paste0(tools::file_path_sans_ext(s), ".html")
      paste0('<a class="graph-link gl-up" href="', slug, '">',
             .html_esc(s), '<span class="gl-label">upstream</span></a>')
    }, character(1L))
    parts <- c(parts, up_links)
  }

  if (length(down_scripts) > 0L) {
    down_links <- vapply(head(down_scripts, 5L), function(s) {
      slug <- paste0(tools::file_path_sans_ext(s), ".html")
      paste0('<a class="graph-link gl-down" href="', slug, '">',
             .html_esc(s), '<span class="gl-label">downstream</span></a>')
    }, character(1L))
    parts <- c(parts, down_links)
  }

  paste(parts, collapse = "\n")
}

.build_sidebar <- function(pages, current_html_name = NULL) {
  links <- vapply(pages, function(p) {
    active <- identical(p$html_name, current_html_name)
    cls    <- if (active) 'sb-link active' else 'sb-link'
    paste0('<a class="', cls, '" href="', p$html_name, '">', .html_esc(p$file_name), '</a>')
  }, character(1L))
  paste(links, collapse = "\n")
}

.build_cards <- function(pages) {
  cards <- vapply(pages, function(p) {
    search_attr <- paste0(' data-search="',
                          gsub('"', '&quot;', paste(p$file_name, p$plain_text)),
                          '"')
    paste0(
      '<a class="card" href="', p$html_name, '"', search_attr, '>',
      '<div class="card-title">', .html_esc(p$file_name), '</div>',
      '<div class="card-excerpt">', .html_esc(p$excerpt), '</div>',
      '</a>'
    )
  }, character(1L))
  paste(cards, collapse = "\n")
}


# ── Markdown → HTML ────────────────────────────────────────────────────────────

.md_to_html <- function(md) {
  # Pre-pass: replace ```mermaid...``` with <div class="mermaid">...</div>
  # so downstream rendering leaves them as raw text for Mermaid.js
  md <- gsub("(?s)```mermaid\n(.*?)```",
             '\n<div class="mermaid">\\1</div>\n',
             md, perl = TRUE)

  if (requireNamespace("commonmark", quietly = TRUE)) {
    commonmark::markdown_html(md, extensions = TRUE, smart = FALSE)
  } else {
    .md_simple(md)
  }
}

# Minimal regex fallback when commonmark is not installed
.md_simple <- function(md) {
  lines  <- strsplit(md, "\n", fixed = TRUE)[[1L]]
  out    <- character(0L)
  i      <- 1L
  in_pre <- FALSE
  pre_buf <- character(0L)
  pre_lang <- ""

  flush_pre <- function() {
    body <- paste(pre_buf, collapse = "\n")
    cls  <- if (nzchar(pre_lang)) paste0(' class="language-', .html_esc(pre_lang), '"') else ""
    out <<- c(out, paste0("<pre><code", cls, ">", .html_esc(body), "</code></pre>"))
    pre_buf  <<- character(0L)
    pre_lang <<- ""
    in_pre   <<- FALSE
  }

  while (i <= length(lines)) {
    ln <- lines[[i]]

    if (in_pre) {
      if (grepl("^```\\s*$", ln)) { flush_pre() } else { pre_buf <- c(pre_buf, ln) }
      i <- i + 1L; next
    }

    # Mermaid / HTML passthrough
    if (grepl("^<div", ln) || grepl("^</div>", ln)) {
      out <- c(out, ln); i <- i + 1L; next
    }

    # Fenced code block
    if (grepl("^```", ln) && !grepl("^```\\s*$", ln)) {
      in_pre   <- TRUE
      pre_lang <- trimws(sub("^```", "", ln))
      i <- i + 1L; next
    }

    # Headings
    if      (grepl("^### ", ln)) { out <- c(out, paste0("<h3>", .md_inline(sub("^### ", "", ln)), "</h3>")); i <- i + 1L; next }
    else if (grepl("^## ",  ln)) { out <- c(out, paste0("<h2>", .md_inline(sub("^## ",  "", ln)), "</h2>")); i <- i + 1L; next }
    else if (grepl("^# ",   ln)) { out <- c(out, paste0("<h1>", .md_inline(sub("^# ",   "", ln)), "</h1>")); i <- i + 1L; next }

    # Unordered list
    if (grepl("^[-*]\\s", ln)) {
      items <- character(0L)
      while (i <= length(lines) && grepl("^[-*]\\s", lines[[i]])) {
        items <- c(items, paste0("<li>", .md_inline(sub("^[-*]\\s+", "", lines[[i]])), "</li>"))
        i <- i + 1L
      }
      out <- c(out, paste0("<ul>", paste(items, collapse = ""), "</ul>"))
      next
    }

    # Blank line
    if (!nzchar(trimws(ln))) { i <- i + 1L; next }

    # Paragraph: collect consecutive non-special lines
    para <- character(0L)
    while (i <= length(lines)) {
      l2 <- lines[[i]]
      if (!nzchar(trimws(l2)) || grepl("^#|^[-*]\\s|^```|^<div", l2)) break
      para <- c(para, l2)
      i <- i + 1L
    }
    if (length(para) > 0L)
      out <- c(out, paste0("<p>", .md_inline(paste(para, collapse = " ")), "</p>"))
  }
  paste(out, collapse = "\n")
}

.md_inline <- function(text) {
  text <- .html_esc(text)
  text <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", text)
  text <- gsub("\\*(.+?)\\*",       "<em>\\1</em>",         text)
  text <- gsub("`([^`]+)`",         "<code>\\1</code>",     text)
  text <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", '<a href="\\2">\\1</a>', text)
  text
}


# ── Utilities ──────────────────────────────────────────────────────────────────

.read_site_tmpl <- function(name) {
  path <- system.file("templates", name, package = "explicaR")
  if (!nzchar(path) || !file.exists(path))
    stop("Site template not found: ", name, call. = FALSE)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
