# explicaR

> *explicar* (Spanish) — to explain

**explicaR** is an R package that makes data pipelines interpretable and visual. It targets data analysts with complex, multi-script R workflows where the flow of data transformations is implicit and hard to communicate.

## What it does

explicaR operates at two levels:

1. **Macro** — an interactive graph showing how scripts, functions, and variables relate across your entire project
2. **Micro** — animated visualisations showing exactly what each `dplyr`/`tidyr` transformation does to your data

The output is a **single self-contained HTML file** — no Shiny server, shareable by email or as a static page.

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
│  ANIMATION PANEL (micro)                       │
│                                                │
│  pivot_longer @ clean.R line 23               │
│  [animated transformation plays here]          │
└────────────────────────────────────────────────┘
```

## Installation

```r
# Install from GitHub
remotes::install_github("CathalByrneGit/explicaR")

# Optional but recommended: datamations animation engine
remotes::install_github("microsoft/datamations")
```

Core dependencies (`CodeDepends`, `tidygraph`, `visNetwork`, `dplyr`, `purrr`, `jsonlite`) are installed automatically.

## Quick start

```r
library(explicaR)

# Point at any R project directory — opens report in your browser
explicar("path/to/your/project")
```

That's it. explicaR will:
1. Parse all `.R` files in the directory
2. Build the dependency graph
3. Generate animations for every `dplyr`/`tidyr` verb call it finds
4. Write `explicar_report.html` and open it

## Usage

### Full pipeline

```r
# Parse, graph, animate, report — all in one call
explicar(
  project_dir = "path/to/project",
  output_file = "pipeline_report.html",
  title       = "My Pipeline"
)
```

### Step by step

```r
# 1. Parse the project
pr <- explicar_parse("path/to/project")
pr$nodes   # all nodes (scripts, variables, functions, sources)
pr$edges   # all edges (produces, consumes, calls, reads, writes)
pr$verbs   # all dplyr/tidyr verb calls found

# 2. Build the macro graph
graph <- explicar_graph(pr)
graph  # renders as an interactive visNetwork widget

# 3. Attach real data shapes (if you have snapshots)
snaps <- with_pipeline_trace("path/to/project/clean.R")
pr    <- attach_shapes(pr, snaps$snapshots)

# 4. Build animations
anims <- explicar_animate(pr, snapshots = snaps$snapshots)

# 5. Render the report
explicar_report(pr, graph, anims)
```

### With a targets project

If your project uses [`{targets}`](https://docs.ropensci.org/targets/), explicaR reads the cache directly — no re-execution needed:

```r
# Auto-detected: explicar_mode() returns "targets"
explicar("path/to/targets/project")

# Or manually
cache <- explicar_targets("path/to/project")
pr    <- attach_shapes(pr, cache)
```

### With instrumented tracing

For projects not using targets:

```r
# Intercepts every dplyr/tidyr call and captures before/after snapshots
trace <- with_pipeline_trace("clean.R")
trace$snapshots   # named list of captured dataframes
trace$trace_log   # tibble: fn, input_var, output_var, elapsed_ms

# Run multiple scripts in order
trace <- trace_pipeline(c("01_load.R", "02_clean.R", "03_model.R"))
```

### Optional LLM enrichment

```r
# Enrich undocumented function nodes with plain-English labels via Ollama
# Requires Ollama running locally: https://ollama.com
# ollama pull qwen2.5-coder:3b

explicar("path/to/project", enrich = TRUE, llm_model = "qwen2.5-coder:3b")

# Or enrich a parse result directly
pr <- enrich_parse_result(pr)

# Check if Ollama is available
ollama_available()
ollama_models()
```

## Searchable code index

explicaR can build a local semantic index of your project so you can ask questions like *"how does the animation pipeline work?"* and get back relevant code and docs — all on-device, nothing sent to external services.

### Build the index

```r
# Index all R source files (functions, edges, call graph)
explicar_index_build()

# Search by keyword or natural language
explicar_index_retrieve("how does verb animation work")
explicar_index_retrieve("filter transformation")
```

### Generate documentation

Two complementary approaches, both fully private:

#### 1. Extract from source (fast, no LLM)

Pulls structured documentation straight from your existing roxygen2 comments,
`man/*.Rd` files, `README.md`, and vignettes.

```r
# Extract from all local sources
explicar_index_build_docs()

# Choose specific sources
explicar_index_build_docs(include = c("source", "readme"))

# Also embed chunks for semantic (vector) search
explicar_index_build_docs(embed = TRUE, embed_model = "nomic-embed-text")
```

Source priority:
1. `man/*.Rd` — run `devtools::document()` to generate these
2. `R/*.R` roxygen2 `#'` blocks — works directly on raw source, no generation step needed
3. `README.md` / `README.Rmd`
4. `vignettes/*.Rmd`, `.qmd`, `.md`

Every chunk stored includes a `file://` URL pointing back to the exact source location.

#### 2. Generate wiki pages with a local LLM (richer, requires Ollama)

Sends each source file to a locally-running Ollama model and asks it to write
a narrative wiki page — same idea as DeepWiki, but running entirely on your
machine with your private code.

```r
# Generate one wiki page per R source file + a cross-file architecture overview
explicar_index_generate_wiki(model = "llama3.2")

# Architecture overview only (faster)
explicar_index_generate_wiki(model = "mistral", include = "architecture")

# Regenerate with a different model
explicar_index_generate_wiki(model = "gemma3", force = TRUE)
```

Each generated page covers:
- **Overview** — what the module does and why it exists
- **Key Functions** — purpose, parameters, return value in plain English
- **How It Works** — internal flow / algorithm explained in prose
- **Usage Example** — short R snippet where relevant

The architecture overview synthesises all files together to explain how the
components connect.

### Retrieve across everything

Both extraction and generation store into the same `docs` table, so a single
retrieve call searches all of it:

```r
explicar_index_retrieve("how does the animation pipeline work")
explicar_index_retrieve("what does explicar_parse return")
```

### Privacy model

| Feature | Network calls |
|---------|--------------|
| `explicar_index_build()` | None — reads local files |
| `explicar_index_build_docs()` | None — reads local files |
| `explicar_index_generate_wiki()` | Local Ollama only |
| Embeddings (`embed = TRUE`) | Local Ollama only |

No source code or documentation ever leaves your machine.

## Architecture

```
explicaR
├── Parser Layer      parse.R    CodeDepends + getParseData() → edge list
├── Graph Layer       graph.R    tidygraph + visNetwork → interactive DAG
├── Shape Layer       shapes.R   nrow × ncol badges on variable nodes
├── Animation Layer   animate.R  datamations bridge (illustrative + real data)
│                     verbs.R    per-verb descriptors (filter, mutate, …)
├── Trace Layer       trace.R    instrumented source() → snapshots
├── targets Layer     targets.R  cache reader (soft dependency)
├── Report Layer      report.R   self-contained HTML renderer
├── Enrich Layer      enrich.R   Ollama LLM enrichment (optional)
└── Index Layer       index.R          DuckDB code index + vector search
                      index-docs.R     local doc extraction + LLM wiki generation
```

### Node types in the graph

| Shape | Colour | Meaning |
|---|---|---|
| Box | Blue | Script (`.R` file) |
| Ellipse | Green | Variable / dataframe |
| Diamond | Orange | Function |
| Database | Purple | Source file (CSV, xlsx, …) |
| Star | Red | Output artefact |

### Supported verbs

| Verb | Package | Animation concept |
|---|---|---|
| `filter` | dplyr | Rows fade out |
| `mutate` | dplyr | New column slides in |
| `select` | dplyr | Unwanted columns slide out |
| `group_by` | dplyr | Rows cluster by colour |
| `summarise` | dplyr | Clusters collapse to summary rows |
| `arrange` | dplyr | Rows reorder |
| `left_join` | dplyr | Two tables merge |
| `pivot_longer` | tidyr | Columns fold down into rows |
| `pivot_wider` | tidyr | Rows lift up into columns |

## Dependencies

| Package | Role | Required? |
|---|---|---|
| `CodeDepends` | Cross-script dependency analysis | Yes |
| `tidygraph` | Graph data model | Yes |
| `visNetwork` | Interactive macro graph | Yes |
| `dplyr` / `purrr` | Data wrangling utilities | Yes |
| `jsonlite` / `htmltools` | Report rendering | Yes |
| `datamations` | Verb-level animation | Optional (GitHub) |
| `targets` | Pipeline cache reading | Optional |
| `roxygen2` | Documentation extraction | Optional |
| `duckdb` / `DBI` | Code index and doc store | Optional |
| `httr2` | Ollama API calls (enrichment, embeddings, wiki) | Optional |
| `ggraph` | Static graph export | Optional |

## Design principles

1. **Progressive disclosure** — macro graph first, click to zoom into micro animation
2. **Zero mandatory re-execution** — uses targets cache when available; illustrative mode always works as a fallback
3. **Self-contained output** — the HTML report works offline with no running server
4. **Soft dependencies** — targets, datamations, and LLM enrichment are all optional
5. **Shape as signal** — `nrow × ncol` on every variable node tells the pipeline story without animation
6. **Prefer human-written context** — roxygen docs and inline comments take priority over LLM inference
7. **Private by default** — the index, doc extraction, and wiki generation all run locally; no source code is sent to external services

## License

MIT
