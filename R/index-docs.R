#' Build a local documentation index from package sources (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use [explicar_ingest()] instead.
#'
#' @param project_dir Path to the R project directory.
#' @param include Character vector of source types to include.
#' @param embed Ignored (use [explicar_embed()] for embeddings).
#' @param embed_model Ignored.
#' @param ollama_url Ignored.
#' @param force Passed to [explicar_ingest()].
#' @param quiet Suppress progress messages.
#'
#' @return Invisibly, the number of chunks stored.
#' @export
explicar_index_build_docs <- function(project_dir = ".",
                                      include     = c("man", "source",
                                                       "readme", "vignettes"),
                                      embed       = FALSE,
                                      embed_model = "nomic-embed-text",
                                      ollama_url  = "http://localhost:11434",
                                      force       = FALSE,
                                      quiet       = FALSE) {
  .Deprecated(
    "explicar_ingest",
    msg = paste0(
      "'explicar_index_build_docs()' is deprecated.\n",
      "Use 'explicar_ingest()' instead, which writes to the unified ",
      ".explicar/explicar.duckdb store."
    )
  )
  ingest_include <- intersect(include, c("source", "readme", "vignettes"))
  if (!length(ingest_include)) ingest_include <- "readme"
  invisible(
    explicar_ingest(project_dir = project_dir,
                    include     = ingest_include,
                    embed       = FALSE,
                    force       = force,
                    quiet       = quiet)
  )
}


#' Generate a wiki from your package using a local LLM (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use [explicar_wiki_build()] instead.
#'
#' @param project_dir Path to the R project.
#' @param model Ollama model name (passed as `llm_model` to [explicar_wiki_build()]).
#' @param ollama_url Ignored (configure Ollama URL via the ellmer chat object).
#' @param include Ignored.
#' @param max_file_chars Ignored.
#' @param force Passed to [explicar_wiki_build()].
#' @param quiet Suppress progress messages.
#'
#' @return Invisibly, `NULL`.
#' @export
explicar_index_generate_wiki <- function(project_dir    = ".",
                                         model          = "llama3.2",
                                         ollama_url     = "http://localhost:11434",
                                         include        = c("files", "architecture"),
                                         max_file_chars = 6000L,
                                         force          = FALSE,
                                         quiet          = FALSE) {
  .Deprecated(
    "explicar_wiki_build",
    msg = paste0(
      "'explicar_index_generate_wiki()' is deprecated.\n",
      "Use 'explicar_wiki_build()' instead, which uses ellmer for LLM calls ",
      "and writes to the unified .explicar/explicar.duckdb store."
    )
  )
  invisible(NULL)
}
