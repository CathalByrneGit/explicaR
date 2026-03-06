#' Optional LLM label enrichment via Ollama
#'
#' Calls a locally running [Ollama](https://ollama.com) instance to generate
#' plain-English descriptions for undocumented function nodes.
#'
#' This is a **soft dependency** — the package works without it. LLM enrichment
#' is used only as a **last resort**, after checking for roxygen documentation
#' and inline comments. See the priority rule in the package documentation.
#'
#' @section Priority rule:
#' 1. Use roxygen `@title`/`@description` verbatim if present — skip LLM.
#' 2. Use inline comments above the function as context.
#' 3. Only call the LLM for completely undocumented functions.
#'
#' @param fn_name_or_body Either a function name (character) to look up in the
#'   current session, or the full body text of the function as a string.
#' @param model Ollama model name. Default `"qwen2.5-coder:3b"`.
#' @param ollama_url Base URL for the Ollama API. Default
#'   `"http://localhost:11434"`.
#' @param max_words Maximum words in the returned description. Default `10L`.
#' @param timeout_s Request timeout in seconds. Default `15L`.
#'
#' @return A character string with the LLM-generated description, or `NA` if
#'   enrichment fails or Ollama is unavailable.
#' @export
#'
#' @examples
#' \dontrun{
#' enrich_node_label("clean_survey")
#' enrich_node_label("function(df) df |> filter(!is.na(id)) |> distinct()")
#' }
enrich_node_label <- function(fn_name_or_body,
                              model      = "qwen2.5-coder:3b",
                              ollama_url = "http://localhost:11434",
                              max_words  = 10L,
                              timeout_s  = 15L) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    message("httr2 is required for LLM enrichment. Install it with: install.packages('httr2')")
    return(NA_character_)
  }

  # Resolve function body if a name was provided
  body_text <- .resolve_fn_body(fn_name_or_body)

  if (is.na(body_text) || nchar(trimws(body_text)) == 0) {
    return(NA_character_)
  }

  prompt <- glue::glue(
    "Summarise what this R function does in under {max_words} words. ",
    "Respond with only the summary, no explanation:\n\n{body_text}"
  )

  tryCatch({
    resp <- httr2::request(paste0(ollama_url, "/api/generate")) |>
      httr2::req_body_json(list(
        model  = model,
        prompt = prompt,
        stream = FALSE
      )) |>
      httr2::req_timeout(timeout_s) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200L) {
      message("Ollama returned HTTP ", httr2::resp_status(resp),
              " — check that Ollama is running and model '", model, "' is pulled.")
      return(NA_character_)
    }

    body <- httr2::resp_body_json(resp)
    raw  <- purrr::pluck(body, "response", .default = NA_character_)

    if (is.na(raw) || nchar(trimws(raw)) == 0) return(NA_character_)

    # Clean up: strip leading/trailing whitespace and quotes
    cleaned <- trimws(gsub('^["\']+|["\']+$', '', trimws(raw)))
    cleaned
  }, error = function(e) {
    message("LLM enrichment failed: ", conditionMessage(e))
    NA_character_
  })
}


#' Check whether Ollama is running and a model is available
#'
#' @param model Model name to check.
#' @param ollama_url Ollama API base URL.
#'
#' @return `TRUE` if Ollama responds and the model is listed.
#' @export
ollama_available <- function(model      = "qwen2.5-coder:3b",
                             ollama_url = "http://localhost:11434") {
  if (!requireNamespace("httr2", quietly = TRUE)) return(FALSE)

  tryCatch({
    resp <- httr2::request(paste0(ollama_url, "/api/tags")) |>
      httr2::req_timeout(3L) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200L) return(FALSE)

    tags  <- httr2::resp_body_json(resp)
    names <- purrr::map_chr(tags$models %||% list(), "name")
    any(startsWith(names, model))
  }, error = function(e) FALSE)
}


#' List models available in the local Ollama instance
#'
#' @param ollama_url Ollama API base URL.
#'
#' @return Character vector of model names, or `character(0)` if Ollama is
#'   unavailable.
#' @export
ollama_models <- function(ollama_url = "http://localhost:11434") {
  if (!requireNamespace("httr2", quietly = TRUE)) return(character(0))

  tryCatch({
    resp <- httr2::request(paste0(ollama_url, "/api/tags")) |>
      httr2::req_timeout(3L) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200L) return(character(0))

    tags <- httr2::resp_body_json(resp)
    purrr::map_chr(tags$models %||% list(), "name")
  }, error = function(e) character(0))
}


#' Enrich all undocumented function nodes in a parse result
#'
#' Iterates over function-type nodes that still have a plain symbol as their
#' label (i.e., no roxygen enrichment applied) and calls [enrich_node_label()]
#' for each.
#'
#' @param parse_result Output from [explicar_parse()].
#' @param model Ollama model name.
#' @param ollama_url Ollama API base URL.
#' @param quiet Suppress per-node messages when `TRUE`.
#'
#' @return The modified `parse_result` with enriched labels.
#' @export
enrich_parse_result <- function(parse_result,
                                model      = "qwen2.5-coder:3b",
                                ollama_url = "http://localhost:11434",
                                quiet      = FALSE) {
  if (!ollama_available(model, ollama_url)) {
    if (!quiet) message(
      "Ollama not available (model: ", model, "). ",
      "Start Ollama and run: ollama pull ", model
    )
    return(parse_result)
  }

  fn_mask <- parse_result$nodes$type == "function" &
    parse_result$nodes$label == parse_result$nodes$name

  n_to_enrich <- sum(fn_mask)
  if (n_to_enrich == 0L) {
    if (!quiet) message("All function nodes already have labels — nothing to enrich.")
    return(parse_result)
  }

  if (!quiet) message("Enriching ", n_to_enrich, " undocumented function node(s) via LLM...")

  nodes <- parse_result$nodes
  indices <- which(fn_mask)

  for (idx in indices) {
    nm     <- nodes$name[idx]
    script <- nodes$file[idx]

    fn_body <- if (!is.na(script) && file.exists(script)) {
      .extract_fn_body_from_file(nm, script)
    } else {
      nm
    }

    label <- enrich_node_label(fn_body, model = model, ollama_url = ollama_url)

    if (!is.na(label) && nchar(label) > 0) {
      nodes$label[idx] <- label
      if (!quiet) message("  ", nm, " \u2192 ", label)
    }
  }

  parse_result$nodes <- nodes
  parse_result
}


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Resolve a function name to its body text
#' @noRd
.resolve_fn_body <- function(fn_name_or_body) {
  # If it looks like a function body already (contains spaces / newlines)
  if (grepl("\\s", fn_name_or_body) || grepl("\n", fn_name_or_body)) {
    return(fn_name_or_body)
  }

  # Try to get function from current session
  fn_obj <- tryCatch(
    get(fn_name_or_body, envir = parent.frame(2), inherits = TRUE),
    error = function(e) NULL
  )

  if (is.function(fn_obj)) {
    return(paste(deparse(fn_obj), collapse = "\n"))
  }

  # Just use the name as a prompt seed
  fn_name_or_body
}


#' Extract a named function's body text from a source file
#' @noRd
.extract_fn_body_from_file <- function(fn_name, script) {
  tryCatch({
    lines <- readLines(script, warn = FALSE)

    # Find line where function is defined
    pattern <- paste0("^\\s*", fn_name, "\\s*(<-|=)\\s*function")
    start_idx <- grep(pattern, lines)
    if (length(start_idx) == 0) return(fn_name)

    start_idx <- start_idx[1]
    # Collect lines until braces balance or EOF
    depth   <- 0L
    end_idx <- start_idx
    found   <- FALSE
    for (i in seq(start_idx, length(lines))) {
      depth <- depth +
        lengths(regmatches(lines[i], gregexpr("\\{", lines[i]))) -
        lengths(regmatches(lines[i], gregexpr("\\}", lines[i])))
      if (i > start_idx && depth <= 0L) {
        end_idx <- i
        found   <- TRUE
        break
      }
    }
    if (!found) end_idx <- min(start_idx + 30L, length(lines))

    paste(lines[start_idx:end_idx], collapse = "\n")
  }, error = function(e) fn_name)
}
