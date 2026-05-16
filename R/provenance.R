# ── Git provenance helpers ─────────────────────────────────────────────────────
#
# Resolves a source file URL for provenance linking in the doc site.
# Auto-detects GitHub, GitLab, Gitea/Forgejo, or any forge with the same
# blob-URL convention. Falls back to a file:// URI when no remote is found.

.resolve_provenance_url <- function(file_path,
                                    project_dir = ".",
                                    branch      = "main",
                                    remote_url  = NULL,
                                    line        = NULL) {
  abs_path <- normalizePath(file_path, mustWork = FALSE)
  proj_abs <- normalizePath(project_dir, mustWork = FALSE)

  rel_path <- if (startsWith(abs_path, proj_abs)) {
    substring(abs_path, nchar(proj_abs) + 2L)
  } else {
    basename(abs_path)
  }
  rel_path <- gsub("\\\\", "/", rel_path)

  if (isFALSE(remote_url)) return(.local_file_url(abs_path, line))

  base_url <- if (!is.null(remote_url)) {
    .normalise_remote(remote_url)
  } else {
    .detect_remote(project_dir)
  }

  if (is.null(base_url)) return(.local_file_url(abs_path, line))

  host <- .detect_host(base_url)
  .forge_url(base_url, host, branch, rel_path, line)
}

.detect_remote <- function(project_dir) {
  # Try git2r first (in Suggests)
  if (requireNamespace("git2r", quietly = TRUE)) {
    repo <- tryCatch(git2r::repository(project_dir, discover = TRUE),
                     error = function(e) NULL)
    if (!is.null(repo)) {
      remotes <- tryCatch(git2r::remotes(repo), error = function(e) character(0))
      if (length(remotes) > 0L) {
        url <- tryCatch(git2r::remote_url(repo, remotes[[1L]]),
                        error = function(e) NULL)
        if (!is.null(url)) return(.normalise_remote(url))
      }
    }
  }
  # Fall back to git CLI
  raw <- tryCatch(
    system2("git", c("-C", shQuote(project_dir), "remote", "get-url", "origin"),
            stdout = TRUE, stderr = FALSE),
    error   = function(e) character(0),
    warning = function(e) character(0)
  )
  if (length(raw) > 0L && nzchar(raw[[1L]])) .normalise_remote(raw[[1L]]) else NULL
}

.normalise_remote <- function(url) {
  url <- trimws(url)
  # SSH → HTTPS: git@github.com:user/repo.git → https://github.com/user/repo
  if (grepl("^git@", url))
    url <- sub("^git@([^:]+):(.+)$", "https://\\1/\\2", url)
  sub("\\.git$", "", url)
}

.detect_host <- function(base_url) {
  if (grepl("github\\.com",  base_url, ignore.case = TRUE)) "github"
  else if (grepl("gitlab",   base_url, ignore.case = TRUE)) "gitlab"
  else                                                       "gitea"
}

.forge_url <- function(base_url, host, branch, rel_path, line = NULL) {
  url <- switch(host,
    github = paste0(base_url, "/blob/",       branch, "/", rel_path),
    gitlab = paste0(base_url, "/-/blob/",     branch, "/", rel_path),
           # gitea / forgejo / generic forge
           paste0(base_url, "/src/branch/",   branch, "/", rel_path)
  )
  if (!is.null(line) && !is.na(line) && as.integer(line) > 0L)
    url <- paste0(url, "#L", as.integer(line))
  url
}

.local_file_url <- function(abs_path, line = NULL) {
  url <- paste0("file://", abs_path)
  if (!is.null(line) && !is.na(line) && as.integer(line) > 0L)
    url <- paste0(url, "#L", as.integer(line))
  url
}
