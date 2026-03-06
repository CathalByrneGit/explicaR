#' Read intermediate objects from a targets cache
#'
#' Soft-depends on the `{targets}` package. When a project uses targets,
#' this module reads cached intermediate objects and returns them as a named
#' list that can be passed to [attach_shapes()] or the animation layer.
#'
#' @param project_dir Path to the project directory (must contain a
#'   `_targets/` subdirectory or a `_targets.yaml` config file).
#' @param names Optional character vector of target names to read. If `NULL`
#'   (default) all available targets are read.
#' @param store Path to the targets store. Defaults to
#'   `file.path(project_dir, "_targets")`.
#'
#' @return A named list of cached R objects (one element per target name).
#'   Returns an empty list if targets is unavailable or the cache does not
#'   exist.
#' @export
#'
#' @examples
#' \dontrun{
#' cache <- explicar_targets("path/to/project")
#' names(cache)
#' }
explicar_targets <- function(project_dir = ".",
                             names = NULL,
                             store = file.path(project_dir, "_targets")) {
  if (!targets_available(project_dir, store)) {
    message("targets cache not found or {targets} not installed. Returning empty list.")
    return(list())
  }

  manifest <- targets::tar_manifest(store = store)

  target_names <- if (!is.null(names)) {
    intersect(names, manifest$name)
  } else {
    manifest$name
  }

  if (length(target_names) == 0) {
    message("No matching targets found in cache.")
    return(list())
  }

  results <- list()
  for (nm in target_names) {
    tryCatch({
      results[[nm]] <- targets::tar_read_raw(nm, store = store)
    }, error = function(e) {
      message("Could not read target '", nm, "': ", conditionMessage(e))
    })
  }

  results
}


#' List available target names and their metadata
#'
#' @param project_dir Path to the project directory.
#' @param store Path to the targets store.
#'
#' @return A tibble with columns `name`, `command`, `pattern`, `cue_mode`,
#'   and `description` (if targets >= 1.3.0).
#' @export
targets_manifest <- function(project_dir = ".",
                             store = file.path(project_dir, "_targets")) {
  if (!targets_available(project_dir, store)) {
    message("targets cache not available.")
    return(tibble::tibble())
  }
  targets::tar_manifest(store = store)
}


#' Check whether a targets cache exists and targets is installed
#'
#' @param project_dir Path to the project directory.
#' @param store Path to the targets store (default `_targets/` inside project).
#'
#' @return `TRUE` if targets is available and a cache is present.
#' @export
targets_available <- function(project_dir = ".",
                              store = file.path(project_dir, "_targets")) {
  if (!requireNamespace("targets", quietly = TRUE)) return(FALSE)
  targets::tar_exist_meta(store = store)
}


#' Detect explicaR execution mode
#'
#' Determines whether to use the targets cache or instrumented execution for
#' retrieving intermediate dataframes.
#'
#' @param project_dir Path to the project directory.
#' @param store Path to the targets store.
#'
#' @return One of `"targets"` or `"instrumented"`.
#' @export
explicar_mode <- function(project_dir = ".",
                          store = file.path(project_dir, "_targets")) {
  if (targets_available(project_dir, store)) "targets" else "instrumented"
}


#' Build a shapes list from a targets cache
#'
#' Reads all data-frame-typed targets and returns a named list suitable for
#' [attach_shapes()].
#'
#' @param project_dir Path to the project directory.
#' @param store Path to the targets store.
#'
#' @return A named list of dataframes (targets that are not dataframes are
#'   silently omitted).
#' @export
shapes_from_targets <- function(project_dir = ".",
                                store = file.path(project_dir, "_targets")) {
  cache <- explicar_targets(project_dir = project_dir, store = store)
  Filter(is.data.frame, cache)
}
