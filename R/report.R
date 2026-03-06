#' Generate the explicaR HTML report
#'
#' Produces a single self-contained HTML file containing:
#' 1. An interactive macro pipeline graph (visNetwork)
#' 2. An animation panel that updates when the user clicks a verb node
#'
#' The output works offline and can be shared by email or hosted as a static page.
#'
#' @param project_dir Path to the R project directory to analyse.
#' @param output_file Path for the output HTML file. Defaults to
#'   `"explicar_report.html"` in the project directory.
#' @param title Report title shown at the top of the HTML page.
#' @param snapshots Optional named list of intermediate dataframes (from
#'   [with_pipeline_trace()] or [explicar_targets()]). When `NULL`, the
#'   report uses illustrative data for animations.
#' @param enrich Logical; whether to enrich node labels via LLM (requires
#'   Ollama running locally). Default `FALSE`.
#' @param llm_model Ollama model name for enrichment (used when
#'   `enrich = TRUE`).
#' @param open Logical; whether to open the report in the browser after
#'   generation. Default `TRUE`.
#'
#' @return Invisibly, the path to the generated HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' explicar("path/to/project")
#' }
explicar <- function(project_dir  = ".",
                     output_file  = file.path(project_dir, "explicar_report.html"),
                     title        = paste0("explicaR — ", basename(normalizePath(project_dir))),
                     snapshots    = NULL,
                     enrich       = FALSE,
                     llm_model    = "qwen2.5-coder:3b",
                     open         = TRUE) {

  message("explicaR: parsing project at ", normalizePath(project_dir))
  parse_result <- explicar_parse(project_dir)

  # Auto-detect execution mode and load snapshots if not supplied
  if (is.null(snapshots)) {
    mode <- explicar_mode(project_dir)
    message("explicaR: mode = ", mode)
    if (mode == "targets") {
      snapshots <- shapes_from_targets(project_dir)
    }
  }

  # Attach data-shape badges to variable nodes
  if (!is.null(snapshots) && length(snapshots) > 0) {
    parse_result <- attach_shapes(parse_result, snapshots)
  }

  # Optional LLM enrichment
  if (enrich) {
    parse_result <- .enrich_parse_result(parse_result, model = llm_model)
  }

  message("explicaR: building macro graph")
  graph_widget <- explicar_graph(parse_result)

  message("explicaR: building animations")
  animations <- explicar_animate(parse_result, snapshots = snapshots)

  message("explicaR: rendering HTML report to ", output_file)
  html <- explicar_report(
    parse_result = parse_result,
    graph_widget = graph_widget,
    animations   = animations,
    title        = title
  )

  htmltools::save_html(html, file = output_file)
  message("explicaR: done \u2714  ", output_file)

  if (open && interactive()) utils::browseURL(output_file)

  invisible(output_file)
}


#' Build the composite HTML report document
#'
#' Low-level function called by [explicar()]. Returns an `htmltools` tag object
#' that can be saved with `htmltools::save_html()` or embedded in Rmarkdown.
#'
#' @param parse_result Output from [explicar_parse()].
#' @param graph_widget visNetwork widget from [explicar_graph()].
#' @param animations Named list from [explicar_animate()].
#' @param title Page title string.
#'
#' @return An `htmltools::tagList`.
#' @export
explicar_report <- function(parse_result,
                            graph_widget,
                            animations   = list(),
                            title        = "explicaR Pipeline Report") {

  # Pre-serialise animation HTML fragments to JSON for JS lookup
  anim_json <- .serialise_animations(animations)

  htmltools::tagList(
    htmltools::tags$html(
      lang = "en",
      htmltools::tags$head(
        htmltools::tags$meta(charset = "utf-8"),
        htmltools::tags$meta(name = "viewport",
                             content = "width=device-width, initial-scale=1"),
        htmltools::tags$title(title),
        htmltools::tags$style(.report_css())
      ),
      htmltools::tags$body(
        # Header
        htmltools::tags$header(
          class = "explicar-header",
          htmltools::tags$h1(title),
          htmltools::tags$p(
            class = "explicar-subtitle",
            glue::glue(
              "{nrow(parse_result$nodes)} nodes \u00b7 ",
              "{nrow(parse_result$edges)} edges \u00b7 ",
              "{nrow(parse_result$verbs)} verb calls"
            )
          )
        ),

        # Main two-panel layout
        htmltools::tags$main(
          class = "explicar-main",

          # Top panel: macro graph
          htmltools::tags$section(
            class = "explicar-graph-panel",
            htmltools::tags$h2("Pipeline Graph"),
            htmltools::tags$p(
              class = "explicar-hint",
              "\u2190 Click a verb node to see its transformation animation below"
            ),
            htmltools::div(id = "explicar-graph", graph_widget)
          ),

          # Bottom panel: animation
          htmltools::tags$section(
            class = "explicar-anim-panel",
            htmltools::tags$h2(
              id = "explicar-anim-title",
              "Animation Panel"
            ),
            htmltools::tags$p(
              class = "explicar-hint",
              id = "explicar-anim-hint",
              "Select a verb node in the graph above to see its animation here."
            ),
            htmltools::div(id = "explicar-anim-content")
          )
        ),

        # Footer
        htmltools::tags$footer(
          class = "explicar-footer",
          htmltools::tags$p(
            "Generated by ",
            htmltools::tags$a(href = "https://github.com/CathalByrneGit/explicaR",
                              "explicaR"),
            " \u00b7 ",
            format(Sys.time(), "%Y-%m-%d %H:%M")
          )
        ),

        # Embedded animation data + interaction JS
        htmltools::tags$script(
          type = "application/json",
          id   = "explicar-animations-data",
          anim_json
        ),
        htmltools::tags$script(.report_js())
      )
    )
  )
}


# ── Internal helpers ─────────────────────────────────────────────────────────

#' Serialise animation HTML fragments to a JSON map keyed by node id
#' @noRd
.serialise_animations <- function(animations) {
  if (length(animations) == 0) return("{}")

  fragments <- purrr::imap(animations, function(anim, key) {
    html_str <- tryCatch(
      as.character(anim$widget),
      error = function(e) paste0("<p>Animation unavailable for ", key, "</p>")
    )
    list(
      key         = key,
      html        = html_str,
      verb        = anim$descriptor$verb %||% key,
      description = anim$descriptor$description %||% ""
    )
  })

  jsonlite::toJSON(fragments, auto_unbox = TRUE)
}


#' Inline CSS for the report
#' @noRd
.report_css <- function() {
  '
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 14px;
    color: #1a1a2e;
    background: #f8f9fa;
  }
  .explicar-header {
    background: #1a1a2e;
    color: #fff;
    padding: 1rem 1.5rem;
    border-bottom: 3px solid #4A90D9;
  }
  .explicar-header h1 { font-size: 1.4rem; font-weight: 600; }
  .explicar-subtitle  { font-size: 0.85rem; color: #aab; margin-top: 0.25rem; }
  .explicar-main {
    display: flex;
    flex-direction: column;
    gap: 0;
    height: calc(100vh - 110px);
  }
  .explicar-graph-panel {
    flex: 1 1 55%;
    padding: 0.75rem 1rem;
    border-bottom: 2px solid #dee2e6;
    background: #fff;
    overflow: hidden;
  }
  .explicar-anim-panel {
    flex: 1 1 45%;
    padding: 0.75rem 1rem;
    background: #f8f9fa;
    overflow-y: auto;
  }
  .explicar-graph-panel h2,
  .explicar-anim-panel h2 {
    font-size: 1rem;
    font-weight: 600;
    color: #1a1a2e;
    margin-bottom: 0.25rem;
  }
  .explicar-hint {
    font-size: 0.8rem;
    color: #6c757d;
    margin-bottom: 0.5rem;
    font-style: italic;
  }
  #explicar-graph { width: 100%; height: calc(100% - 3rem); }
  .explicar-animation { padding: 0.5rem; }
  .explicar-verb-title {
    font-size: 1rem;
    font-weight: 600;
    margin-bottom: 0.75rem;
    color: #1a1a2e;
  }
  .explicar-before-after {
    display: flex;
    align-items: flex-start;
    gap: 1rem;
    flex-wrap: wrap;
  }
  .explicar-before, .explicar-after { flex: 1 1 300px; }
  .explicar-arrow {
    font-size: 2rem;
    color: #4A90D9;
    align-self: center;
    flex: 0 0 auto;
  }
  .explicar-caption { font-weight: 600; font-size: 0.85rem; margin-bottom: 0.3rem; }
  .explicar-table {
    border-collapse: collapse;
    font-size: 0.8rem;
    width: 100%;
  }
  .explicar-table th, .explicar-table td {
    border: 1px solid #dee2e6;
    padding: 0.25rem 0.5rem;
    text-align: left;
    white-space: nowrap;
    max-width: 150px;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .explicar-table thead { background: #e9ecef; font-weight: 600; }
  .explicar-truncated { font-size: 0.75rem; color: #6c757d; margin-top: 0.25rem; }
  .explicar-no-data   { font-size: 0.8rem; color: #aaa; font-style: italic; }
  .explicar-footer {
    padding: 0.5rem 1rem;
    font-size: 0.75rem;
    color: #6c757d;
    border-top: 1px solid #dee2e6;
    background: #fff;
    text-align: center;
  }
  .explicar-footer a { color: #4A90D9; text-decoration: none; }
  '
}


#' Inline JavaScript for graph → animation panel interaction
#' @noRd
.report_js <- function() {
  '
  (function() {
    // Wait for visNetwork to be ready, then attach click handler
    function attachClickHandler() {
      var container = document.getElementById("explicar-graph");
      if (!container) return;
      // visNetwork stores its network object on the widget's HTMLwidget instance
      var widget = HTMLWidgets.find("#explicar-graph");
      if (!widget || !widget.network) {
        setTimeout(attachClickHandler, 200);
        return;
      }
      widget.network.on("click", function(params) {
        if (params.nodes.length === 0) return;
        var nodeId = params.nodes[0];
        showAnimation(nodeId);
      });
    }

    function showAnimation(nodeId) {
      var dataEl = document.getElementById("explicar-animations-data");
      if (!dataEl) return;
      var animations;
      try { animations = JSON.parse(dataEl.textContent); } catch(e) { return; }

      // Look for an animation whose key contains the nodeId
      var match = null;
      for (var i = 0; i < animations.length; i++) {
        if (animations[i].key && animations[i].key.indexOf(nodeId) !== -1) {
          match = animations[i];
          break;
        }
      }

      var titleEl   = document.getElementById("explicar-anim-title");
      var hintEl    = document.getElementById("explicar-anim-hint");
      var contentEl = document.getElementById("explicar-anim-content");

      if (!match) {
        titleEl.textContent   = nodeId;
        hintEl.textContent    = "No animation available for this node.";
        contentEl.innerHTML   = "";
        return;
      }

      titleEl.textContent   = match.verb + " \u2014 " + nodeId;
      hintEl.textContent    = match.description || "";
      contentEl.innerHTML   = match.html || "<p>No animation data.</p>";

      // Re-execute any scripts embedded in the injected HTML (for widgets)
      Array.from(contentEl.querySelectorAll("script")).forEach(function(oldScript) {
        var newScript = document.createElement("script");
        if (oldScript.src) {
          newScript.src = oldScript.src;
        } else {
          newScript.textContent = oldScript.textContent;
        }
        oldScript.parentNode.replaceChild(newScript, oldScript);
      });

      // Scroll animation panel into view
      contentEl.scrollIntoView({ behavior: "smooth", block: "start" });
    }

    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", attachClickHandler);
    } else {
      attachClickHandler();
    }
  })();
  '
}


#' Enrich node labels using LLM (internal, called when enrich=TRUE)
#' @noRd
.enrich_parse_result <- function(parse_result, model) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    message("httr2 not installed. Skipping LLM enrichment.")
    return(parse_result)
  }

  fn_nodes <- parse_result$nodes[parse_result$nodes$type == "function" &
                                    parse_result$nodes$label == parse_result$nodes$name, ]

  if (nrow(fn_nodes) == 0) return(parse_result)

  message("explicaR: enriching ", nrow(fn_nodes), " undocumented function nodes via LLM")

  for (i in seq_len(nrow(fn_nodes))) {
    nm <- fn_nodes$name[i]
    enriched <- tryCatch(
      enrich_node_label(nm, model = model),
      error = function(e) NULL
    )
    if (!is.null(enriched) && nchar(enriched) > 0) {
      parse_result$nodes$label[parse_result$nodes$name == nm] <- enriched
    }
  }

  parse_result
}
