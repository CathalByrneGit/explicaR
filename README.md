# explicaR

> *explicar* (Spanish) — to explain

> An R DeepWiki — point at any local R or Python project and get a browsable, searchable, LLM-annotated wiki.

```r
remotes::install_github("CathalByrneGit/explicaR")
explicar("path/to/any/project")   # → opens in browser
```

---

## What it does

Point `explicar()` at any R or Python project and get:

| Output | Description |
|---|---|
| **Wiki** | One LLM-generated page per source file: Overview, Key functions, How it works, Usage example |
| **Graph** | Interactive Mermaid dependency DAG across scripts, functions, and variables |
| **Index** | DuckDB-backed code graph — queryable from Claude Desktop via MCP or in-browser via WASM |
| **llms.txt** | Machine-readable file listing so any LLM can understand the project instantly |
| **Chat** | RAG-powered Q&A over the wiki (Tier 3 live server) |

Everything runs locally. No code leaves your machine.

---

## explicaR vs DeepWiki

| | DeepWiki | explicaR |
|---|---|---|
| **Input** | GitHub / GitLab / Bitbucket URL | Local R/Python project (+ URL via git2r) |
| **Output** | Hosted web app at `localhost:3000` | Self-contained `file://` HTML |
| **Runtime** | Docker + Python + Node.js | R package, `install.packages()` |
| **LLM** | API keys required upfront | Works offline with Ollama |
| **Queryable from Claude** | No | Yes — MCP server |
| **Installation** | `docker-compose up` | `install.packages("explicaR")` |

---

## Quick start

```r
library(explicaR)

# Tier 1 — self-contained HTML (offline, no server)
explicar("path/to/project")

# With LLM wiki pages (local Ollama — free, private)
explicar("path/to/project", llm = TRUE)

# Bring your own LLM (any ellmer provider — see table below)
library(ellmer)
explicar("path/to/project", llm_chat = chat_openai(model = "gpt-4o-mini"))

# Mixed R + Python project
explicar("path/to/project", languages = c("r", "python"))

# Remote repository (cloned to ~/.explicar/repos/)
explicar("https://github.com/tidyverse/dplyr")
```

---

## LLM provider support

Wiki generation, chat, and DeepResearch all use [ellmer](https://ellmer.tidyverse.org),
which supports every major provider with a consistent API.  Pass any `Chat`
object as `llm_chat`:

| Provider | Call | Notes |
|---|---|---|
| **Ollama** (local) | `chat_ollama("llama3.2")` | Free, private, no key needed |
| **OpenAI** | `chat_openai(model = "gpt-4o-mini")` | Reads `OPENAI_API_KEY` |
| **Anthropic** | `chat_anthropic(model = "claude-sonnet-4-5")` | Reads `ANTHROPIC_API_KEY` |
| **Google Gemini** | `chat_google_gemini()` | Reads `GOOGLE_API_KEY` |
| **Groq** | `chat_groq()` | Reads `GROQ_API_KEY` |
| **AWS Bedrock** | `chat_aws_bedrock()` | Uses AWS credentials |
| **llama.cpp / vLLM** | `chat_openai_compatible(base_url = "http://...")` | Any OpenAI-compatible endpoint |

```r
library(ellmer)
library(explicaR)

# Ollama — completely local, no cost
explicar_wiki_build("path/to/project",
  llm_chat = chat_ollama(model = "llama3.2"))

# OpenAI
explicar_wiki_build("path/to/project",
  llm_chat = chat_openai(model = "gpt-4o-mini"))

# Anthropic
explicar_wiki_build("path/to/project",
  llm_chat = chat_anthropic(model = "claude-haiku-4-5"))
```

---

## Three viewer tiers

### Tier 1 — Static HTML (`file://`)

Zero dependencies. The self-contained HTML includes the Mermaid graph, node
detail panel, wiki pages, and verb-level before/after tables (for data
pipeline projects). Works offline; only the Mermaid CDN request requires
internet.

```r
generate_viewer(pr, output_file = "wiki.html")
```

### Tier 2 — WASM SQL browser (`file://`)

Embeds DuckDB-WASM so you can run SQL queries against the code graph directly
in the browser — no server needed.

```r
generate_wasm_viewer(pr, output_file = "wiki_wasm.html")
```

Queries you can run:
```sql
SELECT name, file, line FROM nodes WHERE type = 'function' ORDER BY name
SELECT from_node, to_node, type FROM edges WHERE type = 'calls'
SELECT fn_name, file, line FROM verbs ORDER BY file, line
```

### Tier 3 — Live server with chat

```r
library(ellmer)
view_explicar_db(llm_chat = chat_ollama(model = "llama3.2"))
# → http://127.0.0.1:8080
```

Endpoints: `GET /`, `POST /chat`, `GET /chat/stream` (SSE),
`POST /search`, `GET /graph.json`, `GET /wiki.json`.
The chat panel has ragnar BM25 + VSS retrieval wired automatically when the
index exists.

---

## Claude Desktop / Claude Code integration (MCP)

Wire explicaR directly into Claude so it can answer questions about your
project without you copying and pasting code:

```r
# Step 1: build the index
explicar_index_build("path/to/project")

# Step 2: start the MCP server (stdio transport)
serve_explicar_mcp("path/to/project")
```

Add to your `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "my-project": {
      "command": "Rscript",
      "args": ["-e", "explicaR::serve_explicar_mcp('path/to/project')"]
    }
  }
}
```

Claude can then use tools: **`search_code`**, **`query_graph`** (SQL),
**`get_wiki`**, **`list_files`**.

---

## Step-by-step usage

```r
library(explicaR)

# 1. Parse the project
pr <- explicar_parse("path/to/project")
pr$nodes   # scripts, variables, functions, source files
pr$edges   # produces, consumes, calls, reads, writes

# 2. Get the Mermaid diagram text
cat(explicar_graph(pr))               # paste into GitHub, Quarto, Obsidian…
cat(explicar_graph(pr, direction = "LR"))

# 3. Generate LLM wiki pages (stored in .explicar/explicar.duckdb)
library(ellmer)
explicar_wiki_build("path/to/project",
  llm_chat = chat_ollama(model = "llama3.2"))

# 4. Ingest wiki + docs into the ragnar RAG store
explicar_ingest("path/to/project")

# 5. DeepResearch — multi-turn iterative investigation
#    Plans → iterates (up to 5 rounds with ragnar retrieval) → CONCLUSION:
chat  <- chat_ollama(model = "llama3.2")   # or any ellmer provider
store <- ragnar::ragnar_store_connect(".explicar/explicar.duckdb")
ragnar::ragnar_register_tool_retrieve(chat, store)
deep_research(chat, "How does the parse dispatch layer work?")
cat(chat$last_turn()$text)

# 6. Generate llms.txt for this project
explicar_llms_txt("path/to/project")   # → llms.txt

# 7. Build the DuckDB code index (incremental, only re-parses changed files)
explicar_index_build("path/to/project")

# 8. Search the index
explicar_index_retrieve("authentication functions", top_k = 5)
explicar_doc_retrieve("how does chunking work")
```

---

## Remote repositories

```r
# Clone from GitHub, analyse, open viewer
explicar("https://github.com/tidyverse/dplyr")

# Private repos — set GITHUB_PAT env var
Sys.setenv(GITHUB_PAT = "ghp_...")
explicar("https://github.com/myorg/private-repo")
```

Clones to `~/.explicar/repos/<host>/<owner>/<repo>/`.  On subsequent calls,
pulls latest changes before re-parsing.  Requires `git2r` (recommended) or
system `git`.

---

## Architecture

### Unified database: `.explicar/explicar.duckdb`

A single DuckDB file is **both** the ragnar RAG store and the explicaR code
graph — all tables are co-located and can be JOINed directly:

```
.explicar/explicar.duckdb
├── ragnar-owned  documents, chunks          ← BM25 + VSS retrieval
└── explicaR-owned  nodes, edges, verbs      ← code graph
                    functions, files         ← function index
                    wiki                     ← LLM-generated pages
                    _meta                    ← schema version / flags
```

### Layer map

```
explicaR
├── Parse Layer      parse.R            dispatch: treesitter → getParseData() → regex
│                    parse_python.R     Python: treesitter.python → regex
├── Graph Layer      graph.R            Mermaid flowchart string
├── Wiki Layer       wiki.R             LLM wiki via ellmer + change detection
│                    ingest.R           ragnar ingest (wiki + docs + README)
│                    embed.R            VSS: embed code-graph nodes via ragnar
├── RAG Layer        index-ragnar.R     ragnar BM25 + VSS store
│                    index.R            DuckDB code-graph index (nodes/edges/verbs)
│                    index-docs.R       Rd + roxygen + README extraction
├── Viewer Layer     generate_viewer.R  Tier 1 HTML
│                    generate_wasm.R    Tier 2 WASM HTML
│                    view_explicar_db.R Tier 3 httpuv server + SSE streaming
│                    inst/templates/    viewer.html · wasm.html · analytics.html
├── MCP Layer        serve_mcp.R        stdio MCP server → Claude Desktop / Code
├── Report Layer     report.R           explicar() orchestrator
├── Remote Layer     resolve_project.R  URL → git clone/pull → local path
├── Enrich Layer     enrich.R           Ollama LLM node-label enrichment
├── Trace Layer      trace.R            with_pipeline_trace() — data pipeline mode
├── targets Layer    targets.R          tar_network() + cache reader
└── Utilities        shapes.R · verbs.R · animate.R · llms_txt.R
```

### Node types

| Mermaid shape | Colour | Type | Meaning |
|---|---|---|---|
| Rectangle `[…]` | Blue | `script` | `.R` or `.py` source file |
| Rounded `(…)` | Green | `variable` | Named object (LHS of assignment) |
| Hexagon `{{…}}` | Orange | `function` | Named function definition |
| Cylinder `[(…)]` | Purple | `source` | Data file (CSV, xlsx) or Python import |

### Edge types

`produces` · `consumes` · `calls` · `reads` · `writes` · `depends`

---

## Data pipeline mode

For projects built around `dplyr` / `tidyr`, explicaR optionally captures
before/after snapshots of each verb call:

```r
# Instrument a script and capture snapshots
trace <- with_pipeline_trace("clean.R")
pr    <- attach_shapes(pr, trace$snapshots)
generate_viewer(pr)   # → detail panel shows before/after tables per verb

# Or use a {targets} cache (no re-execution)
explicar("path/to/targets/project")   # auto-detected
```

This is secondary to the wiki — the viewer shows verb tables only when
snapshots are available.

---

## Dependencies

| Package | Role | Required? |
|---|---|---|
| `dplyr`, `purrr`, `tibble` | Data wrangling | Yes (hard) |
| `rlang`, `glue`, `jsonlite`, `htmltools` | Utilities / rendering | Yes (hard) |
| `treesitter` + `treesitter.r` | Tier-2 parse backend (better AST) | Optional |
| `treesitter.python` | Python AST parsing | Optional |
| `ellmer` | LLM wiki generation, deep research | Optional |
| `ragnar` | BM25 + VSS RAG store | Optional |
| `httpuv` | Tier-3 live server | Optional |
| `duckdb` + `DBI` | Code-graph index + ragnar store | Optional |
| `httr2` | Ollama embeddings (lightweight path) | Optional |
| `roxygen2` | Roxygen doc extraction | Optional |
| `targets` | targets cache + `tar_network()` | Optional |
| `yaml` | Per-project config file (`.explicar/config.yml`) | Optional |
| `git2r` | Remote repo clone / pull | Optional |

---

## Privacy

| Feature | Network calls |
|---|---|
| `explicar()` / `explicar_parse()` | None |
| `explicar_index_build()` | None |
| `explicar_wiki_build(llm_chat = FALSE)` | None |
| `explicar_wiki_build(llm_chat = chat_ollama(...))` | Local Ollama only |
| `explicar_wiki_build(llm_chat = chat_openai(...))` | OpenAI API |
| `explicar_ragnar_build(embed = FALSE)` | None |
| Tier 1 viewer (browser) | Mermaid CDN only |
| Tier 2 WASM viewer (browser) | Mermaid CDN + DuckDB-WASM CDN |

---

## License

MIT
