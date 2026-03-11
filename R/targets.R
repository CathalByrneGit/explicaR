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


#' Build a parse_result-compatible graph from a targets pipeline
#'
#' Uses `tar_network()` for the exact dependency graph and merges target
#' descriptions from `tar_manifest()` as node labels — both are free when
#' targets is available, replacing the slower CodeDepends-based parsing.
#'
#' Node labels come from the `description` field set in `tar_target(...,
#' description = "...")` (targets >= 1.3.0). When no description is set the
#' target name is used as a fallback.
#'
#' @param project_dir Path to the project directory.
#' @param store Path to the targets store.
#' @param script Path to `_targets.R`. Defaults to `_targets.R` inside
#'   `project_dir`.
#' @param outdated Logical; whether to compute outdated status for each target
#'   (shown in node tooltips). Adds a small amount of computation. Default
#'   `FALSE`.
#'
#' @return A list with:
#'   - `nodes`: tibble in `explicar_parse()` node format, plus `status` and
#'     `seconds` columns populated when available
#'   - `edges`: tibble with `from`, `to`, `type`
#'   - `manifest`: raw `tar_manifest()` output
#' @export
targets_network <- function(project_dir = ".",
                             store   = file.path(project_dir, "_targets"),
                             script  = file.path(project_dir, "_targets.R"),
                             outdated = FALSE) {
  if (!targets_available(project_dir, store)) {
    message("targets cache not available; returning empty network.")
    return(.empty_targets_network())
  }

  # Descriptions from manifest — reads stored metadata only, no pipeline needed
  manifest <- tryCatch(
    targets::tar_manifest(store = store),
    error = function(e) tibble::tibble(name = character(), description = character())
  )

  if (!file.exists(script)) {
    message("_targets.R not found at: ", script,
            ". Returning manifest-only network (no edges).")
    return(.targets_network_from_manifest(manifest))
  }

  # Full network topology from _targets.R (runs in-process, no subprocess)
  net <- tryCatch(
    targets::tar_network(
      script         = script,
      store          = store,
      outdated       = outdated,
      callr_function = NULL,
      reporter       = "silent"
    ),
    error = function(e) {
      message("tar_network() failed: ", conditionMessage(e),
              "\nFalling back to manifest-only network.")
      NULL
    }
  )

  if (is.null(net)) return(.targets_network_from_manifest(manifest))

  vertices <- net$vertices
  edges    <- net$edges

  # Merge manifest descriptions (higher priority than anything in vertices)
  has_desc <- "description" %in% names(manifest) && nrow(manifest) > 0
  if (has_desc) {
    vertices <- dplyr::left_join(
      vertices,
      dplyr::select(manifest, name, description),
      by = "name"
    )
  } else {
    vertices$description <- NA_character_
  }

  nodes <- .targets_vertices_to_nodes(vertices)

  edge_tbl <- if (!is.null(edges) && nrow(edges) > 0) {
    tibble::tibble(from = edges$from, to = edges$to, type = "depends")
  } else {
    tibble::tibble(from = character(), to = character(), type = character())
  }

  list(nodes = nodes, edges = edge_tbl, manifest = manifest)
}


# ── targets_network() internals ───────────────────────────────────────────────

.targets_type_map <- c(
  stem     = "variable",
  pattern  = "variable",
  `function` = "function",
  object   = "variable"
)

#' Convert tar_network() vertices to explicaR node schema
#' @noRd
.targets_vertices_to_nodes <- function(vertices) {
  tibble::tibble(
    name       = vertices$name,
    type       = dplyr::recode(vertices$type, !!!.targets_type_map,
                               .default = "variable"),
    file       = NA_character_,
    line       = NA_integer_,
    label      = dplyr::coalesce(vertices$description, vertices$name),
    shape_info = NA_character_,
    status     = if ("status"  %in% names(vertices)) vertices$status  else NA_character_,
    seconds    = if ("seconds" %in% names(vertices)) as.numeric(vertices$seconds) else NA_real_
  )
}

#' Manifest-only fallback when _targets.R is unavailable
#' @noRd
.targets_network_from_manifest <- function(manifest) {
  has_desc <- "description" %in% names(manifest)
  nodes <- tibble::tibble(
    name       = manifest$name,
    type       = "variable",
    file       = NA_character_,
    line       = NA_integer_,
    label      = if (has_desc) dplyr::coalesce(manifest$description, manifest$name)
                 else manifest$name,
    shape_info = NA_character_,
    status     = NA_character_,
    seconds    = NA_real_
  )
  list(
    nodes    = nodes,
    edges    = tibble::tibble(from = character(), to = character(), type = character()),
    manifest = manifest
  )
}

#' Empty network for when targets is unavailable
#' @noRd
.empty_targets_network <- function() {
  list(
    nodes    = tibble::tibble(name = character(), type = character(),
                              file = character(), line = integer(),
                              label = character(), shape_info = character(),
                              status = character(), seconds = numeric()),
    edges    = tibble::tibble(from = character(), to = character(), type = character()),
    manifest = tibble::tibble()
  )
}
