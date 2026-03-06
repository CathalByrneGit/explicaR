#' Instrumented pipeline execution
#'
#' `with_pipeline_trace()` wraps `source()` to intercept every dplyr/tidyr
#' verb call, capturing before/after dataframe snapshots at each step.
#' This provides real intermediate data without requiring a targets cache.
#'
#' @section How it works:
#' The tracer temporarily overrides common dplyr/tidyr functions in a fresh
#' environment, then evaluates the script in that environment. Each intercepted
#' call records the input and output dataframe.
#'
#' @param script Path to the R script to run with tracing enabled.
#' @param envir The environment in which to evaluate the script. Defaults to a
#'   new child of the caller's environment.
#' @param max_snapshot_rows Maximum rows to keep per snapshot (default 500).
#'
#' @return A list with:
#'   - `snapshots`: named list of captured dataframes (input and output per call)
#'   - `trace_log`: tibble with one row per intercepted call (fn, input_var,
#'     output_var, line, elapsed_ms)
#'   - `envir`: the environment after script execution (access final variables)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- with_pipeline_trace("clean.R")
#' result$snapshots[["clean_df"]]  # captured intermediate dataframe
#' result$trace_log
#' }
with_pipeline_trace <- function(script,
                                envir = new.env(parent = parent.frame()),
                                max_snapshot_rows = 500L) {
  if (!file.exists(script)) stop("Script not found: ", script)

  # Shared state accumulated during execution
  trace_store <- new.env(parent = emptyenv())
  trace_store$log       <- list()
  trace_store$snapshots <- list()

  # Build a tracing environment that wraps the target verbs
  trace_env <- .build_trace_env(envir, trace_store, max_snapshot_rows)

  # Evaluate the script inside the tracing environment
  tryCatch(
    source(script, local = trace_env),
    error = function(e) {
      message("Error during traced execution of ", basename(script),
              ": ", conditionMessage(e))
    }
  )

  # Collect all dataframe-valued variables as final snapshots
  final_vars <- ls(envir)
  for (v in final_vars) {
    obj <- tryCatch(get(v, envir = envir), error = function(e) NULL)
    if (is.data.frame(obj)) {
      key <- v
      trace_store$snapshots[[key]] <- .trim_snapshot(obj, max_snapshot_rows)
    }
  }

  trace_log <- if (length(trace_store$log) > 0) {
    dplyr::bind_rows(trace_store$log)
  } else {
    tibble::tibble(fn = character(), input_var = character(),
                   output_var = character(), line = integer(),
                   elapsed_ms = numeric())
  }

  list(
    snapshots = trace_store$snapshots,
    trace_log = trace_log,
    envir     = envir
  )
}


# ── Internal helpers ─────────────────────────────────────────────────────────

# Verbs to intercept and their packages
.TRACE_VERBS <- c(
  # dplyr
  "filter", "mutate", "select", "rename", "arrange",
  "group_by", "ungroup", "summarise", "summarize",
  "left_join", "right_join", "inner_join", "full_join",
  "anti_join", "semi_join",
  "slice", "distinct", "count",
  # tidyr
  "pivot_longer", "pivot_wider", "separate", "unite",
  "drop_na", "fill", "replace_na"
)


#' Build an environment where each target verb is replaced by a tracer wrapper
#' @noRd
.build_trace_env <- function(base_env, store, max_rows) {
  env <- new.env(parent = base_env)

  for (verb in .TRACE_VERBS) {
    # Capture verb name in a local scope to avoid the loop variable aliasing issue
    local({
      v <- verb
      pkg <- if (v %in% c("pivot_longer", "pivot_wider", "separate", "unite",
                           "drop_na", "fill", "replace_na")) "tidyr" else "dplyr"

      real_fn <- tryCatch(
        getExportedValue(pkg, v),
        error = function(e) NULL
      )

      if (!is.null(real_fn)) {
        wrapper <- .make_wrapper(v, real_fn, store, max_rows)
        assign(v, wrapper, envir = env)
      }
    })
  }

  env
}


#' Create a tracing wrapper for a single verb
#' @noRd
.make_wrapper <- function(verb_name, real_fn, store, max_rows) {
  force(verb_name)
  force(real_fn)
  force(store)
  force(max_rows)

  function(.data, ...) {
    t0 <- proc.time()[["elapsed"]]

    # Snapshot before
    input_key <- paste0(verb_name, "_input_", length(store$log) + 1L)
    store$snapshots[[input_key]] <- .trim_snapshot(.data, max_rows)

    # Run the real function
    result <- real_fn(.data, ...)

    elapsed <- (proc.time()[["elapsed"]] - t0) * 1000

    # Snapshot after
    output_key <- paste0(verb_name, "_output_", length(store$log) + 1L)
    if (is.data.frame(result)) {
      store$snapshots[[output_key]] <- .trim_snapshot(result, max_rows)
    }

    # Log the call
    store$log[[length(store$log) + 1L]] <- tibble::tibble(
      fn         = verb_name,
      input_var  = input_key,
      output_var = output_key,
      line       = NA_integer_,
      elapsed_ms = elapsed
    )

    result
  }
}


#' Trim a snapshot to max_rows for storage efficiency
#' @noRd
.trim_snapshot <- function(df, max_rows) {
  if (!is.data.frame(df)) return(df)
  if (nrow(df) > max_rows) df[seq_len(max_rows), , drop = FALSE] else df
}


#' Run multiple scripts with tracing, returning a merged snapshot list
#'
#' @param scripts Character vector of script paths to run in order.
#' @param envir Environment to run scripts in (shared across all scripts).
#' @param max_snapshot_rows Max rows per snapshot.
#'
#' @return Same structure as [with_pipeline_trace()] but accumulating across
#'   all scripts.
#' @export
trace_pipeline <- function(scripts,
                           envir             = new.env(parent = parent.frame()),
                           max_snapshot_rows = 500L) {
  all_snapshots <- list()
  all_logs      <- list()

  for (script in scripts) {
    res <- with_pipeline_trace(script, envir = envir,
                               max_snapshot_rows = max_snapshot_rows)
    all_snapshots <- c(all_snapshots, res$snapshots)
    all_logs[[basename(script)]] <- res$trace_log
  }

  list(
    snapshots = all_snapshots,
    trace_log = dplyr::bind_rows(all_logs, .id = "script"),
    envir     = envir
  )
}
