# explicaR — Project Brief for AI Assistants

## What is explicaR?

**explicaR** is an **R DeepWiki**: point it at any local R or Python project (or
a GitHub URL) and get a browsable, searchable, LLM-annotated wiki — one page per
source file, an interactive Mermaid dependency graph, a DuckDB-backed code index,
a ragnar RAG store with hybrid BM25 + VSS semantic search, and a stdio MCP server
for Claude Desktop / Claude Code. All processing is local.

> Prior versions positioned this as a "data pipeline explorer". That framing is
> obsolete. The project is now an R DeepWiki. Any references to CodeDepends,
> visNetwork, datamations, or Quarto as output engines are legacy artefacts and
> should not be treated as current design intent.

---

## Development Branch

All work goes on **`claude/explore-repo-WzOT0`**. Never push to `main` directly.

```bash
git checkout claude/explore-repo-WzOT0
git push -u origin claude/explore-repo-WzOT0
```

---

## Three Output Tiers

| Tier | Function | Transport | LLM needed |
|------|----------|-----------|------------|
| **1** | `generate_viewer()` → `explicar_viewer.html` | `file://` static | No |
| **2** | `generate_wasm_viewer()` → WASM HTML | `file://` static | No |
| **3** | `view_explicar_db()` → httpuv server | `http://localhost` | Optional |
| **MCP** | `serve_explicar_mcp()` → stdio | Claude Desktop / Code | No |

---

## Architecture

```
explicar()
│
├─ resolve_project()         local path OR remote URL → clone/pull
│
├─ explicar_parse()          parse dispatch (R + Python)
│   ├─ .parse_sitting_duck() DuckDB AST extension stub (always falls back)
│   ├─ .parse_treesitter()   treesitter R package
│   ├─ .parse_r_fallback()   getParseData() native R parser
│   └─ .parse_python()       treesitter.python or regex
│
├─ explicar_graph()          Mermaid flowchart string
│
├─ explicar_wiki_build()     LLM wiki pages via ellmer → wiki table in DuckDB
│   └─ deep_research()       multi-turn DeepResearch loop (Plan→iterate→CONCLUSION)
│
├─ explicar_ingest()         ragnar BM25+VSS store ← wiki/source/readme/vignettes
│
├─ explicar_embed()          v0.5: embed code-graph nodes into ragnar VSS store
│
└─ generate_viewer()         Tier 1 HTML
   generate_wasm_viewer()    Tier 2 HTML
   view_explicar_db()        Tier 3 httpuv server
   serve_explicar_mcp()      stdio MCP server
```

---

## R/ File Map

| File | Purpose |
|------|---------|
| `parse.R` | `explicar_parse()` dispatch layer |
| `parse_treesitter.R` | tree-sitter backend for R |
| `parse_r_fallback.R` | `getParseData()` backend |
| `parse_sitting_duck.R` | DuckDB extension stub (always falls back) |
| `parse_python.R` | Python: treesitter.python or regex |
| `graph.R` | `explicar_graph()` — Mermaid flowchart string |
| `generate_viewer.R` | `generate_viewer()` — Tier 1 HTML |
| `generate_wasm.R` | `generate_wasm_viewer()` — Tier 2 HTML |
| `view_explicar_db.R` | `view_explicar_db()` — Tier 3 httpuv server + analytics HTML builder |
| `wiki.R` | `explicar_wiki_build()`, `deep_research()` |
| `ingest.R` | `explicar_ingest()` — ragnar store ingest |
| `embed.R` | `explicar_embed()`, `explicar_semantic_retrieve()` — v0.5 VSS |
| `llms_txt.R` | `explicar_llms_txt()` — project-level llms.txt generator |
| `serve_mcp.R` | `serve_explicar_mcp()` — stdio JSON-RPC 2.0 MCP server |
| `resolve_project.R` | `resolve_project()` — local path or remote URL → local dir |
| `index.R` | `explicar_index_build/retrieve/connect()` — DuckDB code graph |
| `index-docs.R` | `explicar_index_build_docs/generate_wiki()` — doc extraction |
| `index-ragnar.R` | `explicar_ragnar_build/doc_retrieve/register_retrieve()` |
| `report.R` | `explicar()` — main entry point orchestrator |
| `enrich.R` | `enrich_node_label/parse_result()` — Ollama label enrichment |
| `targets.R` | targets cache + network integration |
| `trace.R` | `with_pipeline_trace()` — instrumented `source()` |
| `animate.R` | `explicar_animate()` — before/after verb widgets |
| `verbs.R` | verb descriptor factories |
| `shapes.R` | shape badge utilities |

---

## Database Design

### Unified store: `.explicar/explicar.duckdb`

This single DuckDB file is **both** the ragnar RAG store (for BM25/VSS retrieval)
and the explicaR-owned code graph store. Both sets of tables coexist and can be
JOINed.

**explicaR-owned tables** (created by `explicar_index_build()`):

```sql
nodes       (name, type, file, line, label, shape_info)
edges       (from_node, to_node, type)
verbs       (file, line, fn_name, input_var, output_var, pkg)
functions   (name, file, line, language, exported, signature, description)
files       (path, language, lines, last_modified, description)
wiki        (file PK, model, generated_at DOUBLE, last_modified DOUBLE, content TEXT)
file_mtimes (file PK, mtime DOUBLE)
_meta       (key PK, value)
```

**ragnar-owned tables** (created by `ragnar_store_create()`):

```
documents, chunks   — chunked content with embeddings for BM25+VSS retrieval
```

**Legacy store**: `.explicar/index.duckdb` — created by the old `explicar_index_build()`.
Still supported as a fallback. New code targets `explicar.duckdb`.

### Node types

| Type | Meaning |
|------|---------|
| `"script"` | A `.R` or `.py` source file |
| `"variable"` | A named object (LHS of assignment) |
| `"function"` | A named function defined in the project |
| `"source"` | Raw data file (CSV, xlsx, …) or Python import |

### Edge types

`produces`, `consumes`, `calls`, `reads`, `writes`, `depends`

---

## Key Design Decisions

### Internal helper access
All `.function_name()` helpers are package-internal. Functions defined in one
file are freely callable from another — e.g. `embed.R` calls `.ragnar_connect_or_create()`
from `ingest.R`, `.require_duckdb()` from `index.R`, and `.explicar_db_path()`
from `wiki.R`. This is intentional and correct R package design.

### `.explicar_db_path(project_dir)`
Defined in `wiki.R`. Returns `file.path(project_dir, ".explicar", "explicar.duckdb")`.
Used throughout to locate the unified store.

### `%||%` operator
Imported from `rlang`. Used extensively for NULL coalescing. Declared in NAMESPACE
as `importFrom(rlang,"%||%")`.

### ragnar store connection pattern
```r
store <- .ragnar_connect_or_create(db_path, embed_fn)   # ingest.R
# ...use store...
tryCatch(ragnar::ragnar_store_disconnect(store), error = function(e) invisible())
```

### Mermaid security level
All HTML templates use `mermaid.initialize({ securityLevel: "loose" })` — required
for the `window.explicarNodeClick()` click callbacks to fire.

### MCP server
Pure R stdio JSON-RPC 2.0. No external binary. Reads from `stdin()`, writes to
`stdout()`. Blocks until stdin closes. Protocol: MCP 2024-11-05. Four tools:
`search_code`, `query_graph` (SELECT-only SQL), `get_wiki`, `list_files`.

### SSE streaming
The Tier 3 `/chat/stream` endpoint returns `text/event-stream`. ellmer's
`chat$chat()` is currently called synchronously and the full response wrapped
in a single `data:` frame.

### Change detection (wiki)
`explicar_wiki_build()` compares `file.info(path)$mtime` against `last_modified`
stored in the `wiki` table. Files with unchanged mtime are skipped unless
`force = TRUE`.

### Remote repo support
`resolve_project(url, git_pat, update)` clones to
`~/.explicar/repos/<host>/<owner>/<repo>/`. Subsequent calls run `git pull`
unless `update = FALSE`. Uses `git2r` (preferred) or `system("git ...")`.
PAT is read from `GITHUB_PAT` / `GITLAB_TOKEN` / `BITBUCKET_TOKEN` env vars or
passed explicitly via `git_pat`.

### v0.5 VSS node format
`explicar_embed()` converts each node to:
`"function: \`name()\` — label [R/file.R:42]"`
This format is reversible by `.parse_node_text()` in `embed.R`.

---

## Template Placeholders

### `inst/templates/viewer.html` (Tier 1)
`{{TITLE}}`, `{{STATS}}`, `{{GENERATED_AT}}`, `{{MERMAID_GRAPH}}`,
`{{VERB_DATA_JSON}}`, `{{NODE_DATA_JSON}}`, `{{ID_MAP_JSON}}`, `{{WIKI_DATA_JSON}}`

### `inst/templates/wasm.html` (Tier 2)
`{{TITLE}}`, `{{STATS}}`, `{{GENERATED_AT}}`, `{{MERMAID_GRAPH}}`,
`{{NODE_DATA_JSON}}`, `{{EDGE_DATA_JSON}}`, `{{VERB_DATA_JSON}}`, `{{ID_MAP_JSON}}`

### `inst/templates/analytics.html` (Tier 3)
`{{TITLE}}`, `{{STATS}}`, `{{GENERATED_AT}}`, `{{MERMAID_GRAPH}}`,
`{{NODE_DATA_JSON}}`, `{{EDGE_DATA_JSON}}`, `{{WIKI_DATA_JSON}}`,
`{{ID_MAP_JSON}}`, `{{HAS_CHAT}}`

---

## Delivery Sequence (completed)

| Version | Deliverable | Status |
|---------|-------------|--------|
| v0.1 | Parse + Mermaid + Tier 1 HTML (R + Python) | ✅ |
| v0.2 | ragnar BM25 store + Tier 2 WASM + file-filter config | ✅ |
| v0.3 | LLM wiki build + ingest + `llms.txt` + change detection | ✅ |
| v0.4 | Tier 3 server + SSE + MCP + remote repos + DeepResearch | ✅ |
| v0.5 | Unified VSS embeddings via ragnar (`explicar_embed`) | ✅ |
| v1.0 | CRAN submission | 🔜 |

---

## CRAN Checklist (v1.0 remaining)

- [ ] `devtools::check()` clean (0 errors, 0 warnings, ≤1 note)
- [ ] `cran-comments.md` created
- [ ] All `\dontrun{}` examples reviewed
- [ ] `httr2` usage in legacy embed path — decide keep (Suggests) or remove
- [ ] `NEWS.md` exists ✅ (added in gap-fix commit)
- [ ] DESCRIPTION version ✅ (bumped to 0.5.0 in gap-fix commit)

---

## Common Patterns

### Adding a new exported function

1. Create/edit the `.R` file with roxygen2 `#' @export` tag
2. Add `export(fn_name)` to `NAMESPACE` manually (roxygen2 not auto-run in CI)
3. Document in `inst/llms.txt` under `## Core functions`

### Running tests

```r
devtools::test()            # all tests
testthat::test_file("tests/testthat/test-viewer.R")
```

### Checking a parse result shape

```r
pr <- explicar_parse("path/to/project", languages = c("r", "python"))
str(pr)
# list(
#   nodes: tibble(name, type, file, line, label, shape_info)
#   edges: tibble(from, to, type)
#   verbs: tibble(file, line, fn_name, input_var, output_var, args, pkg)
# )
```

### Typical full pipeline (with LLM)

```r
library(explicaR)
library(ellmer)

chat <- chat_ollama(model = "llama3.2")

# 1. Build code graph index
explicar_index_build("path/to/project")

# 2. Generate wiki pages
explicar_wiki_build("path/to/project", llm_chat = chat)

# 3. Ingest into ragnar BM25 store
explicar_ingest("path/to/project")

# 4. Embed nodes for VSS (requires Ollama)
explicar_embed("path/to/project")

# 5. Tier 1 viewer
explicar("path/to/project", llm_chat = chat, open = TRUE)

# OR: Tier 3 interactive server
view_explicar_db("path/to/project", llm_chat = chat)

# OR: MCP server for Claude Desktop
serve_explicar_mcp("path/to/project")
```

---

## Key Dependencies

| Package | Role | Required |
|---------|------|----------|
| `dplyr`, `tibble`, `purrr`, `rlang` | Data wrangling | Imports |
| `jsonlite` | JSON for viewer data | Imports |
| `glue`, `htmltools` | String/HTML utilities | Imports |
| `duckdb` (≥ 0.10.0), `DBI` | Persistent index + WASM | Suggests |
| `ragnar` | BM25 + VSS RAG store | Suggests |
| `ellmer` | LLM calls (wiki, chat) | Suggests |
| `httpuv` | Tier 3 server | Suggests |
| `git2r` | Remote repo clone/pull | Suggests |
| `treesitter`, `treesitter.r` | AST parsing | Suggests |
| `treesitter.python` | Python AST parsing | Suggests |
| `yaml` | File filter config | Suggests |
| `httr2` | Legacy Ollama embed path | Suggests |
| `withr` | Test helpers | Suggests |
| `targets` | Pipeline cache integration | Suggests |

**Removed from original design**: `CodeDepends` (unmaintained, R-only),
`visNetwork` / `tidygraph` (replaced by Mermaid), `datamations` (GitHub-only,
post-v1), `Quarto`/`rmarkdown` as output engines (replaced by self-contained HTML).
