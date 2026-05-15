# ── Tier-1 parser: DuckDB sitting_duck extension ──────────────────────────────
# Uses the sitting_duck DuckDB extension (https://github.com/Query-farm/sitting-duck)
# for tree-sitter-powered parsing entirely inside DuckDB.
# Not yet widely installed; always falls through in the current release.

.parse_sitting_duck_available <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) return(FALSE)
  tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    DBI::dbExecute(con, "INSTALL sitting_duck FROM community; LOAD sitting_duck;")
    TRUE
  }, error = function(e) FALSE)
}

#' @noRd
.parse_sitting_duck <- function(scripts) {
  stop("sitting_duck backend is not available on this system.",
       call. = FALSE)
}
