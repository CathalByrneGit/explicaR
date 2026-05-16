# explicaR News

## explicaR 0.5.0

### New features

* `explicar_embed()` — new v0.5 entry point that converts code-graph nodes
  to natural-language text and inserts them into the ragnar VSS store at
  `.explicar/explicar.duckdb`, enabling vector-similarity search via Ollama.

* `explicar_semantic_retrieve()` — hybrid BM25 + VSS search over the unified
  ragnar store; degrades to BM25 when no embeddings are available.

* `explicar_index_retrieve()` now tries the ragnar store on `explicar.duckdb`
  first (semantic search) before falling back to the legacy httr2 vector path
  on `index.duckdb` and finally keyword search.

* `explicar()` gained `git_pat` and `update` parameters for private repository
  authentication and controlling git pull behaviour on cached clones.

* `resolve_project()` gained `git_pat` and `update` parameters to support
  passing an explicit PAT and skipping the pull step.

* `functions` table is now populated by `explicar_index_build()` so it is
  available to `query_graph` MCP calls and browser SQL queries.

## explicaR 0.4.0

### New features

* `view_explicar_db()` — Tier 3 interactive analytics server (httpuv).
  Endpoints: `GET /`, `POST /chat`, `GET /chat/stream` (SSE), `POST /search`,
  `GET /graph.json`, `GET /wiki.json`, `GET /health`.  ragnar retrieval is
  wired automatically to `llm_chat` when the store exists.

* `serve_explicar_mcp()` — stdio MCP server (JSON-RPC 2.0, MCP 2024-11-05).
  Exposes four tools to Claude Desktop / Claude Code: `search_code`,
  `query_graph`, `get_wiki`, `list_files`.

* `resolve_project()` — normalise a local path or remote GitHub/GitLab/
  Bitbucket URL to a local directory.  Remote repos are cloned to
  `~/.explicar/repos/<host>/<owner>/<repo>/` and pulled on subsequent calls.

* `explicar()` now accepts remote URLs and uses `resolve_project()` internally.

* New `inst/templates/analytics.html` — 4-tab analytics dashboard for Tier 3.

## explicaR 0.3.0

### New features

* `explicar_wiki_build()` — generates one LLM wiki page per source file using
  an ellmer `Chat` object.  Change detection via file `mtime`; unchanged files
  are skipped.  Falls back to roxygen blocks + parse graph when no LLM is
  available.

* `deep_research()` — multi-turn research loop.  Plans, iterates (retrieving
  via ragnar tools), terminates when response starts with `"CONCLUSION:"`.

* `explicar_ingest()` — ingests wiki pages, roxygen comments, README, and
  vignettes into the ragnar BM25/VSS store at `.explicar/explicar.duckdb`.

* `explicar_llms_txt()` — generates a machine-readable `llms.txt` for the
  analysed project: stats, file list, function index, wiki summaries, Mermaid
  DAG.

## explicaR 0.2.0

### New features

* `generate_wasm_viewer()` — Tier 2 self-contained HTML viewer with in-browser
  DuckDB-WASM for interactive SQL queries over `nodes`, `edges`, and `verbs`.

* `explicar_config()` — reads active file-filter config from
  `inst/config/defaults.yml` merged with per-project `.explicar/config.yml`.

## explicaR 0.1.0

* Initial release.
* `explicar_parse()` — parse dispatch: sitting_duck → treesitter → getParseData()
  → regex fallback.  Supports R and Python.
* `explicar_graph()` — Mermaid `flowchart` string generator.
* `generate_viewer()` — self-contained Tier 1 HTML viewer.
* `explicar_ragnar_build()` / `explicar_doc_retrieve()` — BM25 + VSS doc store
  via ragnar.
* `explicar_index_build()` / `explicar_index_retrieve()` — persistent DuckDB
  code-graph index.
