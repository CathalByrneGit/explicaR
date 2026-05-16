# ── Remote project resolution ──────────────────────────────────────────────────
#
# Allows explicar() to accept a GitHub/GitLab/Bitbucket URL in addition to a
# local path.  Remote repos are cloned to ~/.explicar/repos/<host>/<owner>/<repo>/
# and pulled on subsequent calls.

#' Resolve a project path or URL to a local directory
#'
#' When `project_dir` is a remote URL (GitHub, GitLab, Bitbucket), the
#' repository is cloned to `~/.explicar/repos/<host>/<owner>/<repo>/`.
#' On subsequent calls the local clone is updated with `git pull`.
#'
#' Local paths are returned unchanged (after `normalizePath()`).
#'
#' @param project_dir Local path or remote URL (e.g.
#'   `"https://github.com/tidyverse/dplyr"` or
#'   `"github.com/tidyverse/dplyr"`).
#' @param git_pat Personal access token for private repositories.  When `NULL`
#'   (default), reads `GITHUB_PAT` / `GITLAB_TOKEN` / `BITBUCKET_TOKEN` from
#'   the environment automatically.
#' @param update Logical; run `git pull` when a cached clone already exists.
#'   Set `FALSE` to use the cached version without fetching.  Default `TRUE`.
#'
#' @return Absolute path to the local project directory.
#' @export
#'
#' @examples
#' \dontrun{
#' # Remote — cloned/pulled automatically
#' path <- resolve_project("https://github.com/tidyverse/dplyr")
#'
#' # Skip the pull if you know the cache is fresh
#' path <- resolve_project("https://github.com/tidyverse/dplyr", update = FALSE)
#'
#' # Private repo with explicit PAT
#' path <- resolve_project("https://github.com/org/private-repo",
#'                         git_pat = Sys.getenv("GITHUB_PAT"))
#'
#' # Local — returned as-is
#' path <- resolve_project("path/to/project")
#' }
resolve_project <- function(project_dir, git_pat = NULL, update = TRUE) {
  if (!.is_remote_url(project_dir)) {
    return(normalizePath(project_dir, mustWork = TRUE))
  }

  parsed     <- .parse_repo_url(project_dir)
  cache_path <- .clone_cache_path(parsed)

  if (dir.exists(cache_path)) {
    if (isTRUE(update)) {
      message("explicaR: pulling latest changes for ", parsed$repo, "…")
      .git_pull(cache_path, git_pat = git_pat, host = parsed$host)
    }
  } else {
    message("explicaR: cloning ", parsed$url, "…")
    .git_clone(parsed$url, cache_path, parsed$host, git_pat = git_pat)
  }

  normalizePath(cache_path, mustWork = TRUE)
}


# ── Internals ──────────────────────────────────────────────────────────────────

.is_remote_url <- function(path) {
  grepl(
    "^https?://|^git@|^github\\.com/|^gitlab\\.com/|^bitbucket\\.org/",
    path, perl = TRUE
  )
}

.parse_repo_url <- function(url) {
  # Normalize shorthand: "github.com/user/repo" → "https://github.com/user/repo"
  if (!grepl("^https?://", url)) url <- paste0("https://", url)

  # Strip .git suffix
  url <- sub("\\.git$", "", url)

  m <- regmatches(url, regexec(
    "^https?://([^/]+)/([^/]+)/([^/]+)$", url
  ))[[1L]]

  if (length(m) < 4L) stop("Cannot parse repo URL: ", url, call. = FALSE)

  list(url = url, host = m[[2L]], owner = m[[3L]], repo = m[[4L]])
}

.clone_cache_path <- function(parsed) {
  file.path(path.expand("~"), ".explicar", "repos",
            parsed$host, parsed$owner, parsed$repo)
}

.git_clone <- function(url, dest_path, host, git_pat = NULL) {
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  if (requireNamespace("git2r", quietly = TRUE)) {
    cred <- .git_cred(host, git_pat)
    tryCatch(
      git2r::clone(url, dest_path, credentials = cred),
      error = function(e) {
        message("  git2r clone failed: ", conditionMessage(e), " — trying system git")
        .git_system_clone(url, dest_path)
      }
    )
  } else {
    .git_system_clone(url, dest_path)
  }
}

.git_pull <- function(repo_path, git_pat = NULL, host = NULL) {
  if (requireNamespace("git2r", quietly = TRUE)) {
    tryCatch({
      repo <- git2r::repository(repo_path)
      cred <- if (!is.null(host)) .git_cred(host, git_pat) else NULL
      git2r::pull(repo, credentials = cred)
    }, error = function(e) {
      message("  git2r pull failed: ", conditionMessage(e), " — trying system git")
      .git_system_pull(repo_path)
    })
  } else {
    .git_system_pull(repo_path)
  }
}

.git_cred <- function(host, explicit_pat = NULL) {
  token <- if (!is.null(explicit_pat) && nzchar(explicit_pat)) {
    explicit_pat
  } else {
    switch(
      host,
      "github.com"    = Sys.getenv("GITHUB_PAT",    unset = ""),
      "gitlab.com"    = Sys.getenv("GITLAB_TOKEN",   unset = ""),
      "bitbucket.org" = Sys.getenv("BITBUCKET_TOKEN",unset = ""),
      ""
    )
  }
  if (!nzchar(token)) return(NULL)
  tryCatch(git2r::cred_token(), error = function(e) NULL)
}

.git_system_clone <- function(url, dest_path) {
  status <- system2("git", c("clone", "--depth=1",
                              shQuote(url), shQuote(dest_path)))
  if (status != 0L) stop("git clone failed for: ", url, call. = FALSE)
  invisible()
}

.git_system_pull <- function(repo_path) {
  system2("git", c("-C", shQuote(repo_path), "pull", "--ff-only"),
          stdout = FALSE, stderr = FALSE)
  invisible()
}
