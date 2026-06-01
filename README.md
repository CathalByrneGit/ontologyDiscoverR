<!-- README.md is generated from README.Rmd. Please edit that file. -->

# ontologyDiscoverR

**AI-assisted ontology discovery from documents and schemas.**

`ontologyDiscoverR` uses the Anthropic Claude API to automatically extract
ontology structure — object types, properties, link types, action types, and
concept definitions — from database schemas, API specs, documentation, and
data files. The output is a structured bundle (a plain R list) that works
standalone or can feed into downstream ontology tooling.

## Installation

``` r
# install.packages("pak")
pak::pkg_install("cathalbyrnegit/ontologydiscoverr")
```

Set your Anthropic API key before use:

``` r
Sys.setenv(ANTHROPIC_API_KEY = "your-key-here")
# Or add ANTHROPIC_API_KEY=your-key-here to your ~/.Renviron
```

## How it works

The package has two phases, each powered by a pluggable LLM provider (Anthropic,
OpenAI, Ollama/local, or any ellmer-compatible backend):

```
Phase 1 — Schema discovery
  Source documents / schemas / APIs
            ↓  parse → extract → merge → review
       ontology bundle (plain R list)

Phase 2 — Population
       ontology bundle  +  document corpus
            ↓  extract instances → validate against schema
       named data frames  +  edge list  +  provenance table
```

**Phase 1** runs three sequential LLM passes per source:

1. **Object types** — entities with identity (tables, schema components, named domain concepts)
2. **Link types** — relationships between object types (FK constraints, API references, document links)
3. **Action types & concept hints** — verbs/endpoints and business-rule conditions

**Phase 2** takes the bundle from phase 1 and extracts *specific named instances* from a
document corpus — every person, company, transaction, date, and connection mentioned.
Instances are validated against the schema; unrecognised properties are flagged as
schema amendment candidates rather than silently dropped.

## Quick start

### Phase 1 — Schema discovery

``` r
library(ontologyDiscoverR)

# Default provider: Anthropic Claude
sess <- discover_session("hospital-demo")

# Or use a local model for sensitive data:
# sess <- discover_session("epstein", llm = function(sp) ellmer::chat_ollama("llama3.3:70b", system_prompt = sp))

sess <- dis_add_file(sess, "path/to/hospital-schema.sql")
sess <- dis_add_file(sess, "path/to/api-spec.yaml")
sess <- dis_add_file(sess, "path/to/domain-docs.pdf")

sess   <- dis_extract(sess, verbose = TRUE)
sess   <- dis_review(sess)   # optional Shiny review app
bundle <- dis_to_bundle(sess, bundle_id = "hospital-v1", bundle_name = "Hospital Ontology")
```

### Phase 2 — Population (extract instances from a corpus)

``` r
# Initialise from the bundle produced above
pop <- populate_session(schema = bundle)

# Add individual files or a whole directory of PDFs
pop <- pop_add_corpus(pop, "path/to/documents/")

pop <- pop_extract(pop, verbose = TRUE)
#> ℹ Extracting instances from source 1/47: report-2019.pdf
#> ✔ Extracted 312 entities and 178 relationships
#> ! 4 schema amendment(s) flagged — review with pop_amendments()

# Named list of data frames, one per object type
records <- pop_to_records(pop)
records$instances$Person    # data frame: instance_id, name, role, ...
records$instances$Company   # data frame: instance_id, name, jurisdiction, ...
records$relationships        # from_instance_id, to_instance_id, link_type_id, evidence
records$provenance           # instance_id, source_label, page/excerpt, confidence

# Clean edge list for graph packages
edges <- pop_to_edgelist(pop)   # from, to, type, weight, evidence, from_label, to_label

# Schema amendments to review
pop_amendments(pop)

# Save/resume for large corpora
pop_save(pop, "investigation.rds")
pop <- pop_load("investigation.rds")
```

## Supported source types

| Source type | Function | Notes |
|---|---|---|
| SQL DDL file | `parse_sql_ddl()` | CREATE TABLE → object types, FK → link types |
| Live database | `parse_db_schema()` | DBI connection, reads information\_schema |
| OpenAPI spec | `parse_openapi()` | 3.x + Swagger 2.x, YAML or JSON |
| CSV file | `parse_csv()` | Header + sample rows, type inference |
| R data frame | `parse_r_dataframe()` | In-memory, includes `str()` for LLM context |
| PDF | `parse_pdf()` | Text extracted via `pdftools`, up to 50k chars |
| Markdown / HTML | `parse_markdown()` / `parse_html()` | Heading structure preserved |
| OWL / RDF | `parse_owl()` | Classes → object types, properties → links |

## LLM providers

All sessions accept an `llm` argument — a one-argument function that returns an
[ellmer](https://ellmer.tidyverse.org) Chat object:

``` r
# Anthropic Claude (default)
discover_session("demo")

# OpenAI GPT-4o
discover_session("demo", llm = function(sp) ellmer::chat_openai(model = "gpt-4o", system_prompt = sp))

# Local Ollama — recommended for sensitive/private document corpora
discover_session("epstein",
  llm = function(sp) ellmer::chat_ollama(model = "llama3.3:70b", system_prompt = sp))

# Same pattern for populate_session()
populate_session(schema = bundle,
  llm = function(sp) ellmer::chat_ollama(model = "llama3.3:70b", system_prompt = sp))
```

## Live database example

``` r
library(DBI)
library(duckdb)

con  <- dbConnect(duckdb::duckdb(), "hospital.ddb")
sess <- discover_session("from-db")
sess <- dis_add_db(sess, con, label = "hospital-db")
sess <- dis_extract(sess)
```

## Confidence scores

Every extracted candidate carries a confidence score (0–1):

| Score | Meaning |
|---|---|
| 1.0 | Explicitly defined in schema / data dictionary |
| 0.9 | Clearly named in documentation with attributes listed |
| 0.7 | Mentioned as a concept with some structure implied |
| 0.5 | Inferred from context |
| 0.2 | Speculative |

Use `min_confidence` in `dis_to_bundle()` to control which pending candidates
are included:

``` r
bundle <- dis_to_bundle(sess, "my-bundle", "My Ontology", min_confidence = 0.7)
```

## Interactive review

``` r
sess <- dis_review(sess)
```

The Shiny app has four panels:

- **Object Types** — approve/reject individual candidates or bulk-approve all ≥ 0.8 confidence
- **Link Types** — correct from/to endpoints if the LLM misidentified them
- **Concept Hints** — edit the suggested SQL expression inline, test against a live DB
- **Conflicts** — resolve pairs of conflicting candidates side by side

## MCP conversational building

If `ontologyMCP` is installed, you can build the ontology interactively:

``` r
library(ontologyMCP)

server <- mcp_server("ontology-builder")
server <- dis_add_mcp_tool(server, sess)
mcp_start(server)

# Now an LLM agent can say things like:
# "Add a link from Hospital to Patient called Admission"
# "The Patient object type should have a concept called high_risk where age > 65"
# "Looks good — generate the bundle"
```

## Testing

All tests mock `call_claude()` — no API key required, no cost:

``` r
# install.packages("mockery")
devtools::test()
```

## Package structure

```
R/
  session.R           # discover_session, dis_add_*, dis_extract, dis_summary, dis_to_bundle
  llm.R               # call_claude (Anthropic API via httr2)
  parsers/            # one file per source type
  extract.R           # four LLM extraction passes
  merge.R             # merge_candidates, detect_conflicts
  candidates.R        # S3 constructors for all four candidate types
  review.R            # dis_review() Shiny app
  mcp_builder.R       # dis_add_mcp_tool() (optional)
  types.R             # normalise_type, validate_candidate
  utils.R             # UUID, truncation, JSON helpers

inst/fixtures/        # example-schema.sql, example-openapi.yaml,
                      # example-docs.md, example-data.csv

tests/testthat/       # mocked-LLM tests for all modules
vignettes/            # four how-to vignettes
```

## License

MIT
