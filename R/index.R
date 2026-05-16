#' Persistent DuckDB-backed index for explicaR (deprecated)
#'
#' @description
#' These functions are deprecated. The separate `index.duckdb` store has been
#' superseded by `explicar.duckdb`, the unified ragnar store that holds both
#' the code graph and the RAG retrieval index.
#'
#' Use [explicar_ingest()] to build the retrieval index and
#' [explicar_semantic_retrieve()] to query it.
#'
#' @name explicar_index
NULL

#' Build the explicaR code graph index (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated. The `index.duckdb` store is superseded by `explicar.duckdb`.
#' Use [explicar_ingest()] to build the searchable index.
#'
#' @param project_dir Path to the project.
#' @param pattern Ignored.
#' @param recursive Ignored.
#' @param force Ignored.
#' @param quiet Suppress messages.
#'
#' @return Invisibly, `NULL`.
#' @export
explicar_index_build <- function(project_dir = ".",
                                 pattern     = "*.R",
                                 recursive   = TRUE,
                                 force       = FALSE,
                                 quiet       = FALSE) {
  .Deprecated(
    "explicar_ingest",
    msg = paste0(
      "'explicar_index_build()' is deprecated.\n",
      "The separate index.duckdb store has been superseded by the unified ",
      "explicar.duckdb ragnar store.\n",
      "Use 'explicar_ingest()' to build the retrieval index."
    )
  )
  invisible(NULL)
}


#' Open a DBI connection to the explicaR index (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated. Connect directly to `.explicar/explicar.duckdb` via DBI:
#' ```r
#' con <- DBI::dbConnect(duckdb::duckdb(),
#'          dbdir = file.path(project_dir, ".explicar", "explicar.duckdb"),
#'          read_only = TRUE)
#' ```
#'
#' @param project_dir Path to the project.
#' @param read_only Ignored.
#'
#' @return Invisibly, `NULL`.
#' @export
explicar_index_connect <- function(project_dir = ".", read_only = TRUE) {
  .Deprecated(
    msg = paste0(
      "'explicar_index_connect()' is deprecated.\n",
      "Connect directly to .explicar/explicar.duckdb via DBI:\n",
      "  con <- DBI::dbConnect(duckdb::duckdb(),\n",
      "           dbdir = file.path(project_dir, '.explicar', 'explicar.duckdb'),\n",
      "           read_only = TRUE)"
    )
  )
  invisible(NULL)
}


#' Retrieve nodes from the explicaR index (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated. Use [explicar_semantic_retrieve()] for semantic search over the
#' unified `explicar.duckdb` ragnar store.
#'
#' @param query Search query.
#' @param project_dir Path to the project.
#' @param top_k Maximum results.
#' @param type Ignored.
#'
#' @return A zero-row tibble with a deprecation warning.
#' @export
explicar_index_retrieve <- function(query,
                                    project_dir = ".",
                                    top_k       = 10L,
                                    type        = NULL) {
  .Deprecated(
    "explicar_semantic_retrieve",
    msg = paste0(
      "'explicar_index_retrieve()' is deprecated.\n",
      "Use 'explicar_semantic_retrieve()' for BM25 + VSS search over ",
      "the unified explicar.duckdb store."
    )
  )
  tibble::tibble(name = character(), type = character(), file = character(),
                 line = integer(), label = character())
}


# ── Internal helpers kept for backward compatibility ─────────────────────────

.index_dir  <- function(project_dir) file.path(project_dir, ".explicar")
.index_path <- function(project_dir) file.path(.index_dir(project_dir), "index.duckdb")

.require_duckdb <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    stop("Packages 'duckdb' and 'DBI' are required.\n",
         "Install with: install.packages(c('duckdb', 'DBI'))", call. = FALSE)
  }
}
