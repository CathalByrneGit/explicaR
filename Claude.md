# explicaR — Project Brief for AI Assistants

## What is explicaR?

**explicaR** (from the Spanish *explicar* — to explain) is an R package that makes data pipelines interpretable and visual. It targets data analysts who have complex, multi-script R workflows — often migrated from Excel — where the flow of data transformations is implicit and hard to communicate to others.

The package operates at **two levels**:

1. **Macro level** — a navigable graph showing how scripts, functions, and variables relate across an entire project
2. **Micro level** — cell-by-cell animated visualisations showing exactly what each data transformation does to a dataframe

---

## The Problem Being Solved

When an analyst builds a workflow in R (especially one evolved from Excel), the logic lives implicitly across many scripts. There is no equivalent of Excel's "trace dependents" or a flowchart showing what feeds into what. New team members, auditors, or even the original author returning after 6 months cannot easily answer:

- Where does `clean_df` come from?
- What does `pivot_longer` actually do to *this* specific dataframe?
- Which scripts depend on which intermediate objects?
- If I change `raw.csv`, what downstream outputs are affected?

explicaR answers these questions visually and interactively.

---

## Core Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                        explicaR                              │
│                                                              │
│   PARSER LAYER          GRAPH LAYER        ANIMATION LAYER  │
│   ─────────────         ───────────        ───────────────  │
│   CodeDepends      →    tidygraph      →   datamations      │
│   getParseData()        visNetwork         (per-verb)       │
│                         ggraph                              │
│                                                              │
│                    HTML REPORT OUTPUT                        │
│                    (Quarto / htmlwidgets)                    │
└──────────────────────────────────────────────────────────────┘
```

### R/ directory layout

```
R/
├── parse.R        # Static analysis: CodeDepends + getParseData → edge list
├── trace.R        # Instrumented source() — captures before/after df snapshots
├── targets.R      # targets cache reader (soft dependency on {targets})
├── graph.R        # tidygraph + visNetwork: macro pipeline graph
├── shapes.R       # Data shape badges per node (nrow × ncol, type summary)
├── animate.R      # datamations JSON builder — called per verb node
├── verbs.R        # Per-verb animation logic (pivot_longer, filter, mutate…)
└── report.R       # Quarto/htmlwidgets composite HTML report renderer
```

---

## Layer 1 — The Parser

### Primary tool: `CodeDepends`

`CodeDepends` is preferred over raw `getParseData()` for cross-script pipelines because it tracks **inputs and outputs per expression**, not just tokens.

```r
library(CodeDepends)

scripts <- list.files("R/", pattern = "\\.R$", full.names = TRUE)
parsed  <- lapply(scripts, readScript)
deps    <- lapply(parsed, getInputs)

# CodeDepends tells you:
# - script A writes → clean_df (output)
# - script B reads  → clean_df (input)
# → therefore A → B is an edge in the pipeline graph
```

### What to extract per function call

For each dplyr/tidyr verb call found in the AST, the parser should produce a record:

```r
list(
  file       = "clean.R",
  line       = 23,
  fn_name    = "pivot_longer",
  input_var  = "raw_df",
  output_var = "long_df",
  args       = list(cols = "starts_with('Q')", names_to = "quarter"),
  pkg        = "tidyr"
)
```

This record is the bridge between the macro graph (node identity) and the animation layer (what to animate and with what arguments).

### Secondary tool: `getParseData()`

For finer-grained token-level analysis within a single script — identifying pipes (`|>` or `%>%`), assignment targets, and call chains.

```r
parsed <- parse(file = "clean.R")
pd     <- getParseData(parsed)
# Returns dataframe: token, text, line, col, parent id
# Useful tokens: SYMBOL_FUNCTION_CALL, LEFT_ASSIGN, PIPE, FUNCTION
```

### Tertiary source: roxygen2 comments

If the project uses `{roxygen2}` documentation comments, these are **high-quality free context** and should be extracted and used wherever available. They represent intent the analyst already articulated — far more reliable than inferring meaning from code alone.

```r
# Example: a documented function in the project
#' Clean raw survey responses
#'
#' Removes incomplete rows, standardises column names, and pivots
#' quarterly columns into long format ready for modelling.
#'
#' @param df A raw dataframe read from \code{raw.csv}
#' @return A long-format dataframe with columns: id, quarter, value
clean_survey <- function(df) { ... }
```

The parser should extract roxygen blocks using the `{roxygen2}` package or simple regex on `#'` prefixed lines, and attach them to the corresponding node in the graph:

```r
# Extract roxygen title and description for a function node
parse_roxygen <- function(file) {
  # roxygen2::parse_file() returns structured tag lists
  blocks <- roxygen2::parse_file(file)
  purrr::map(blocks, ~list(
    fn_name     = .x$object$alias,
    title       = roxygen2::block_get_tag_value(.x, "title"),
    description = roxygen2::block_get_tag_value(.x, "description"),
    params      = roxygen2::block_get_tags(.x, "param"),
    returns     = roxygen2::block_get_tag_value(.x, "return")
  ))
}
```

**Priority rule**: if a roxygen `@title` or `@description` exists for a node, use it directly as the node label — do **not** call the LLM for that node. Reserve LLM enrichment for undocumented functions. This makes the tool faster, cheaper, and respects the author's own words.

---

## Layer 2 — The Macro Graph

### Data model: a DAG of nodes and edges

```
Node types:
  - script     (a .R file)
  - variable   (a named R object, e.g. clean_df)
  - function   (a named function defined in the project)
  - source     (raw data file: CSV, xlsx, database)
  - output     (final artefact: plot, model, report)

Edge types:
  - produces   (script → variable)
  - consumes   (variable → script)
  - calls      (script → function)
  - reads      (script → source)
  - writes     (script → output)
```

### Rendering

Use `{tidygraph}` to model the graph and `{visNetwork}` for interactive HTML output.

- Layout: `layout_with_sugiyama()` from igraph — respects DAG hierarchy, time flows top-to-bottom
- Node encoding: shape encodes node type; colour encodes script/file; size encodes data dimensionality where known
- Data shape badge on each variable node: `980 × 8` (nrow × ncol) — lets a reader understand transformations without clicking

```r
library(tidygraph)
library(visNetwork)

tg <- tbl_graph(nodes = node_df, edges = edge_df)
# Convert to visNetwork format and render
```

---

## Layer 3 — The Animation Layer (datamations)

### What is datamations?

[datamations](https://microsoft.github.io/datamations/) is a Microsoft Research R package that generates frame-by-frame animated explanations of dplyr/tidyr pipelines. It compiles a pipeline into a sequence of Vega-Lite specs, then animates the transitions between them.

GitHub: https://github.com/microsoft/datamations  
Install: `remotes::install_github("microsoft/datamations")`

### How explicaR uses datamations

datamations is the **micro-level animation engine**. explicaR calls it per-node: when the user clicks a verb node in the macro graph, a datamations animation plays in a side panel, showing exactly what that transformation does to the data.

```r
library(datamations)

# datamations works by capturing a pipeline as an expression
# and generating animation JSON from it
pipeline <- quote(
  raw_df %>%
    filter(year > 2020) %>%
    pivot_longer(cols = starts_with("Q"), names_to = "quarter", values_to = "value")
)

# datamation() compiles this to an animated htmlwidget
datamation(pipeline, data = raw_df)
```

### Two modes for feeding data to datamations

**Mode 1 — Illustrative (static, no execution)**  
Generate a small synthetic dataframe that matches the *shape* of the real data (same column names and types) but with toy values. Safe, fast, works without running user code. Good for documentation.

**Mode 2 — Instrumented execution (real data)**  
Wrap pipeline execution in tracing hooks. Intercept every dplyr/tidyr verb, capture before/after snapshots, feed real data to datamations. More powerful — shows actual row counts, real value distributions.

```r
# Instrumented source: trace.R
with_pipeline_trace <- function(expr, output_path = NULL) {
  # Override dplyr/tidyr verbs with tracing wrappers
  # Capture environment snapshots before and after each call
  # Return list of (verb, input_snapshot, output_snapshot, args)
}
```

### Verbs to support (priority order)

| Verb | Animation concept |
|------|-----------------|
| `pivot_longer` | Columns fold down into rows |
| `pivot_wider` | Rows lift up into columns |
| `filter` | Rows fade out / slide away |
| `mutate` | New column slides in from right, values compute |
| `select` | Unneeded columns slide out |
| `group_by` | Rows visually cluster by group colour |
| `summarise` | Clusters collapse into single summary rows |
| `left_join` | Two tables approach, rows match and merge |
| `arrange` | Rows reorder with position transitions |

---

## Layer 4 — targets Integration

[targets](https://docs.ropensci.org/targets/) is an R pipeline toolkit that caches intermediate objects. If the user's project uses targets, explicaR can read the cache directly — giving real intermediate dataframes **without re-running anything**.

```r
# targets-aware path (soft dependency — only if targets is installed and _targets/ exists)
if (requireNamespace("targets", quietly = TRUE) && targets::tar_exist_meta()) {
  
  # Read object names from targets manifest
  manifest <- targets::tar_manifest()
  
  # Pull cached intermediate objects
  clean_df <- targets::tar_read("clean_df")
  
  # These snapshots feed directly into datamations animations
}
```

### targets-independent path

For projects not using targets, `trace.R` provides `with_pipeline_trace()` which instruments `source()` calls and captures snapshots at each assignment.

The package should **auto-detect** which mode to use:

```r
explicar_mode <- function(project_dir) {
  if (targets_available(project_dir)) "targets"
  else "instrumented"
}
```

---

## The HTML Report

The final output is a **single self-contained HTML file** — no Shiny server needed, shareable by email or as a static page.

Layout concept:
```
┌────────────────────────────────────────────────┐
│  PIPELINE GRAPH (macro)                        │
│                                                │
│  [raw.csv] → [clean.R] → [model.R] → [out.R]  │
│                  │                             │
│            [clean_df 980×8]                    │
│                                                │
│  ← click any node to see animation below →    │
├────────────────────────────────────────────────┤
│  ANIMATION PANEL (micro / datamations)         │
│                                                │
│  pivot_longer @ clean.R line 23               │
│  [animated datamations widget plays here]      │
└────────────────────────────────────────────────┘
```

Built using Quarto or `{rmarkdown}` with `self_contained: true`. The macro graph is a `visNetwork` htmlwidget. The animation panel is a datamations htmlwidget swapped in via JavaScript on node click.

---

## Key Dependencies

| Package | Role | Notes |
|---------|------|-------|
| `CodeDepends` | Cross-script dependency analysis | Core parser |
| `tidygraph` | Graph data model | Wraps igraph |
| `visNetwork` | Interactive macro graph | Wraps vis.js |
| `ggraph` | Static graph export | ggplot2 grammar |
| `datamations` | Verb-level animation | GitHub only: microsoft/datamations |
| `targets` | Cache reading | Soft dependency |
| `httr2` | LLM enrichment via Ollama | Optional |
| `quarto` or `rmarkdown` | Report rendering | Final output |
| `glue` | String interpolation | Utility |
| `purrr` | Functional pipeline walking | Utility |

---

## LLM Enrichment (Optional Layer)

Small local LLMs via [Ollama](https://ollama.com) can enrich node labels with plain-English descriptions. This is an **optional, soft dependency** — the package works without it.

### Recommended models
- `qwen2.5-coder:3b` — fast, code-aware, runs on CPU
- `llama3.2:3b` — good general summarisation

### What to use LLMs for

LLMs should be treated as a **fallback**, not a first resort. Always prefer existing human-written context in this order:

1. **roxygen `@title`/`@description`** — use verbatim if present, skip LLM entirely for that node
2. **Inline comments above a function** — extract and use as label context
3. **LLM inference** — only for completely undocumented functions

When LLM enrichment is warranted:

1. **Node labelling** — given a function body (and any partial comments), produce a ≤10 word plain-English description
2. **Implicit dependency inference** — identify variables passed through environments or `assign()` that static analysis misses
3. **Anomaly flagging** — "this filter removes 40% of rows — is that expected?"

```r
enrich_node_label <- function(fn_body, model = "qwen2.5-coder:3b") {
  prompt <- glue::glue(
    "Summarise what this R function does in under 10 words:\n\n{fn_body}"
  )
  httr2::request("http://localhost:11434/api/generate") |>
    httr2::req_body_json(list(model = model, prompt = prompt, stream = FALSE)) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("response")
}
```

---

## Design Principles

1. **Progressive disclosure** — macro graph first, click to zoom into micro animation. Never overwhelm with detail upfront.
2. **Zero mandatory re-execution** — if targets cache exists, use it. Static illustrative mode always available as fallback.
3. **Self-contained output** — the report HTML works offline, is shareable, requires no running server.
4. **Soft dependencies** — `targets`, `datamations`, and LLM enrichment are all optional. Core parsing and graph work with base R + CodeDepends only.
5. **Shape as signal** — data dimensions (nrow × ncol) on every variable node. The pipeline story should be readable without animation.
6. **targets-independent** — the package should be fully useful on projects not using targets. targets support is an enhancement, not a requirement.

---

## Suggested Build Order

1. `parse.R` — CodeDepends wrapper producing a tidy edge list
2. `graph.R` — tidygraph + visNetwork macro graph with shape badges
3. `targets.R` — targets cache reader for real intermediate data
4. `animate.R` + `verbs.R` — datamations bridge, starting with `pivot_longer` and `filter`
5. `trace.R` — instrumented `source()` for targets-independent execution
6. `report.R` — Quarto composite HTML report with click-through from graph to animation
7. `enrich.R` — optional LLM label enrichment via Ollama

---

## Reference Projects to Study

- **datamations** (Microsoft Research): https://github.com/microsoft/datamations — the animation engine we build on
- **tidyexplain** (Garrick Aden-Buie): https://github.com/gadenbuie/tidyexplain — gganimate-based join animations, good for metaphors
- **CodeDepends**: https://github.com/duncantl/CodeDepends — the static analysis backbone
- **targets**: https://docs.ropensci.org/targets/ — pipeline caching we integrate with
- **visNetwork**: https://datastorm-open.github.io/visNetwork/ — the macro graph renderer
