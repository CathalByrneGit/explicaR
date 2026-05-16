# explicaR

> *explicar* (Spanish) — to explain

**explicaR** is an R package that makes data pipelines interpretable and visual. Point it at any R project directory and it produces a single, self-contained HTML viewer showing how your scripts, variables, and functions relate — and what each `dplyr`/`tidyr` transformation actually does to your data.

## What it does

explicaR operates at two levels:

1. **Macro** — a [Mermaid](https://mermaid.js.org) flowchart DAG showing the full dependency graph across all `.R` files
2. **Micro** — before/after data tables showing exactly what each `dplyr`/`tidyr` verb does to your data

The output is a **single, self-contained HTML file** — no Shiny server, shareable by email or hosted as a static page.

```
┌─────────────────────────┬───────────────────────────┐
│  Pipeline Graph         │  Detail Panel             │
│  (Mermaid DAG)          │                           │
│                         │  variable: clean_df       │
│  [raw.csv]              │  file: clean.R            │
│     │ reads             │  shape: 980 × 8           │
│  [load.R]               │                           │
│     │ produces          │  filter @ clean.R:23      │
│  [raw_df 1200×12]       │  ┌──────────┐  ┌───────┐ │
│     │ uses              │  │ Before   │→ │ After │ │
│  [clean.R]              │  │ 1200 rows│  │ 980   │ │
│     │ produces          │  └──────────┘  └───────┘ │
│  [clean_df 980×8]       │                           │
│                         │  ← click any node        │
└─────────────────────────┴───────────────────────────┘
```

## Installation

```r
remotes::install_github("CathalByrneGit/explicaR")
```

Core hard dependencies (`dplyr`, `purrr`, `jsonlite`, `htmltools`, `glue`, `tibble`, `rlang`) are installed automatically. Everything else is optional — see the [dependency table](#dependencies) below.

## Quick start

```r
library(explicaR)

# Point at any R project — opens viewer in your browser
explicar("path/to/your/project")
```

explicaR will:
1. Parse all `.R` files using the best available backend (treesitter → base R)
2. Build the Mermaid dependency DAG
3. Generate before/after tables for every `dplyr`/`tidyr` verb call found
4. Write `explicar_viewer.html` and open it

## Usage

### One-line report

```r
explicar(
  project_dir = "path/to/project",
  output_file = "pipeline.html",
  title       = "My Data Pipeline",
  direction   = "LR"   # graph direction: TD | LR | BT | RL
)
```

### Step by step

```r
# 1. Parse the project (dispatches to best available backend)
pr <- explicar_parse("path/to/project")
pr$nodes   # tibble: scripts, variables, functions, source files
pr$edges   # tibble: produces, consumes, calls, reads edges
pr$verbs   # tibble: every dplyr/tidyr verb call found

# 2. Get the Mermaid diagram text
cat(explicar_graph(pr))               # paste into GitHub, Quarto, Obsidian…
cat(explicar_graph(pr, direction = "LR"))

# 3. Attach real data shapes (optional — from targets cache or trace)
snaps <- with_pipeline_trace("clean.R")
pr    <- attach_shapes(pr, snaps$snapshots)

# 4. Generate the viewer HTML
generate_viewer(pr, output_file = "pipeline.html")
```

### Choose your parse backend

```r
# Auto (default): treesitter → base-R getParseData() fallback
pr <- explicar_parse("path/to/project")

# Force a specific backend
pr <- explicar_parse("path/to/project", backend = "treesitter")
pr <- explicar_parse("path/to/project", backend = "r")
```

### With a `{targets}` project

If your project uses [`{targets}`](https://docs.ropensci.org/targets/), explicaR reads the pipeline graph and cache directly — no re-execution needed:

```r
# Auto-detected: explicar_mode() returns "targets"
explicar("path/to/targets/project")

# Or manually inspect the targets network
tnet  <- targets_network("path/to/project")
cache <- explicar_targets("path/to/project")
pr    <- attach_shapes(pr, cache)
```

### With instrumented tracing

For projects not using targets:

```r
trace <- with_pipeline_trace("clean.R")
trace$snapshots   # named list of before/after dataframes
trace$trace_log   # fn, input_var, output_var, elapsed_ms per call
```

### Optional LLM enrichment

Enrich undocumented function nodes with plain-English labels via a local [Ollama](https://ollama.com) model:

```r
explicar("path/to/project", enrich = TRUE, llm_model = "qwen2.5-coder:3b")

# Or directly on a parse result
pr <- enrich_parse_result(pr, model = "qwen2.5-coder:3b")

# Check Ollama availability
ollama_available()   # TRUE/FALSE
ollama_models()      # character vector of pulled models
```

## Code index

Build a persistent DuckDB index of your project so repeated `explicar()` runs are fast and you can search the call graph:

```r
# Build once — only re-parses changed files on subsequent calls
explicar_index_build()

# Keyword search over nodes (functions, variables, scripts)
explicar_index_retrieve("clean survey data")
explicar_index_retrieve("pivot", type = "function", top_k = 5)

# Direct DBI access for custom queries
con <- explicar_index_connect()
DBI::dbGetQuery(con, "SELECT * FROM nodes WHERE type = 'variable'")
DBI::dbDisconnect(con, shutdown = TRUE)
```

## Documentation search with ragnar

For rich hybrid BM25 + vector-similarity search over your package docs (man pages, roxygen comments, README, vignettes), use the [ragnar](https://github.com/tidyverse/ragnar)-backed doc store:

```r
# Requires: install.packages("ragnar")

# Build the doc store — BM25 only (no Ollama needed)
explicar_ragnar_build(embed = FALSE)

# Build with vector embeddings for semantic search (Ollama required)
explicar_ragnar_build(embed = TRUE, embed_model = "nomic-embed-text")

# Hybrid BM25 + VSS retrieval
explicar_doc_retrieve("how does the animation pipeline work")
explicar_doc_retrieve("filter rows", n = 5, bm25_only = TRUE)

# Wire into an ellmer chat for conversational Q&A
# Requires: install.packages(c("ragnar", "ellmer"))
library(ellmer)
chat <- chat_ollama(model = "llama3.2")
chat <- explicar_register_retrieve(chat)
chat$chat("What does explicar_parse return?")
```

Source priority (deduplication applied):
1. `man/*.Rd` — most authoritative; run `devtools::document()` to generate
2. `R/*.R` roxygen `#'` blocks — covers undocumented internals too
3. `README.md` / `README.Rmd`
4. `vignettes/*.Rmd`, `.qmd`, `.md`

### Local LLM wiki generation

Generate narrative wiki pages for each source file using a local Ollama model:

```r
explicar_index_generate_wiki(model = "llama3.2")

# Architecture overview only
explicar_index_generate_wiki(model = "llama3.2", include = "architecture")
```

### Privacy model

| Feature | Network calls |
|---|---|
| `explicar()` / `explicar_parse()` | None — reads local files |
| `explicar_index_build()` | None — reads local files |
| `explicar_index_build_docs()` | None — reads local files |
| `explicar_ragnar_build(embed = FALSE)` | None |
| `explicar_ragnar_build(embed = TRUE)` | Local Ollama only |
| `explicar_index_generate_wiki()` | Local Ollama only |
| `enrich = TRUE` / `enrich_parse_result()` | Local Ollama only |

No source code or documentation ever leaves your machine.

## Architecture

```
explicaR
├── Parse Layer       parse.R              dispatch → best available backend
│                     parse_treesitter.R   treesitter + treesitter.r (Tier 2)
│                     parse_r_fallback.R   base-R getParseData()     (Tier 3)
│                     parse_sitting_duck.R DuckDB sitting_duck ext.  (Tier 1, stub)
├── Graph Layer       graph.R              Mermaid flowchart text generator
├── Viewer Layer      generate_viewer.R    fills inst/templates/viewer.html
│                     inst/templates/      self-contained HTML template
├── Shape Layer       shapes.R             nrow × ncol badges on variable nodes
├── Animation Layer   animate.R            before/after table widgets
│                     verbs.R              per-verb descriptors (filter, pivot, …)
├── Trace Layer       trace.R              instrumented source() → snapshots
├── targets Layer     targets.R            cache reader + tar_network() topology
├── Enrich Layer      enrich.R             Ollama LLM node-label enrichment
├── Index Layer       index.R              DuckDB code graph (nodes/edges/verbs)
│                     index-docs.R         local doc extraction + wiki generation
│                     index-ragnar.R       ragnar BM25+VSS doc store
└── Report Layer      report.R             explicar() orchestrator
```

### Node types in the Mermaid graph

| Mermaid shape | Colour | Meaning |
|---|---|---|
| Rectangle `[…]` | Blue | Script (`.R` file) |
| Rounded `(…)` | Green | Variable / dataframe |
| Hexagon `{{…}}` | Orange | Function |
| Cylinder `[(…)]` | Purple | Source file (CSV, xlsx, …) |

### Supported verbs

Any function exported from `dplyr` or `tidyr` whose first parameter is `.data`, `x`, `data`, or `.tbl` is auto-detected — no hardcoded list. The description for each verb is read directly from the package's own Rd documentation.

## Dependencies

| Package | Role | Required? |
|---|---|---|
| `dplyr`, `purrr`, `tibble` | Data wrangling | Yes (hard) |
| `rlang`, `glue`, `jsonlite`, `htmltools` | Utilities / rendering | Yes (hard) |
| `treesitter` + `treesitter.r` | Tier-2 parse backend | Optional — enhances parsing |
| `targets` | Pipeline cache + tar_network() | Optional |
| `roxygen2` | Roxygen doc extraction | Optional |
| `ragnar` | Hybrid BM25+VSS doc store | Optional |
| `ellmer` | LLM chat integration | Optional |
| `duckdb` + `DBI` | Code index + ragnar store | Optional |
| `httr2` | Ollama embedding + wiki calls | Optional |
| `git2r` | Remote repo support | Optional (future) |

## Design principles

1. **Progressive disclosure** — macro graph first, click to zoom into micro detail
2. **Zero mandatory re-execution** — uses targets cache when available; illustrative mode always works as fallback
3. **Self-contained output** — the HTML viewer works offline (graph rendering requires internet for Mermaid CDN)
4. **Soft dependencies** — every optional feature degrades gracefully; the package works with base R + core tidyverse only
5. **Shape as signal** — `nrow × ncol` on every variable node tells the pipeline story without animation
6. **Prefer human-written context** — roxygen docs and inline comments take priority over LLM inference
7. **Private by default** — parsing, indexing, doc extraction, and wiki generation all run locally

## License

MIT
