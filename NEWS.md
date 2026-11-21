# ontologyDiscoverR 0.2.0

## Breaking changes

* Bundles are now built and validated with ontologySpecR. `dis_to_bundle()`
  builds every element with the spec constructors (`property_def()`,
  `primary_key_def()`, `object_type()`, `link_type()`, `action_type()`,
  `concept_def()`, `bundle()`) and checks the result with
  `ontologySpecR::validate_bundle()` before returning it. On failure it aborts,
  naming the candidate behind each message. The bundle now uses the canonical
  keys (`bundleId`, `objects`, `links`, `actions`, `concepts`, ...), so code
  that read `bundle$object_types` or `bundle$bundle_id` should use the
  ontologySpecR accessors (`bundle_object_types()`, `bundle_id()`, ...).
  - LLM property types are mapped onto the spec types; unknown types become
    `"string"` with a warning.
  - Ids that are not valid spec ids are made valid, with a warning; the
    original column name is kept as the property's `source$column`.
  - An object type with no primary key gets a surrogate `<type>_id` key.
  - Provenance (candidate id, confidence, sources, model) is kept under each
    element's `extensions$ontologyDiscoverR`.
* `populate_session()` takes an ontologySpecR bundle (or anything
  `ontologySpecR::as_bundle()` accepts, such as a bundle JSON file).
* No LLM provider or model is hard-coded any more. With nothing configured,
  discovery stops with an error showing how to choose one. `llm` (in
  `discover_session()`, `populate_session()`, `call_llm()` and
  `dis_add_mcp_tool()`) accepts an ellmer Chat object, a `"provider/model"`
  string, a `function(system_prompt)` returning a Chat, or `NULL` to use
  `getOption("ontologyDiscoverR.chat")` and then the `ONTOLOGYDISCOVERR_CHAT`
  environment variable.
* The MCP builders (`dis_add_mcp_tool()`, `pop_add_mcp_tool()`) now use
  `ontologyAPI::mcp_add_tool()`; ontologyMCP is no longer used.

## New features

* Extraction uses structured output (`$chat_structured()`) with ellmer type
  specs when the provider supports it, and otherwise parses text with one
  retry that includes the parse error.
* The provider and model used are stored in `session$llm_info` and on each
  candidate as `extracted_by` (e.g. `"openai/gpt-4.1"`).
* `dis_extract()` and `pop_extract()` gain `chunk_tokens` (default
  `getOption("ontologyDiscoverR.chunk_tokens", 8000)`), useful for small local
  models.
* New "Choosing a model" section in `vignette("from-schema")`.

## Deprecations

* `call_claude()` is deprecated: it warns and calls `call_llm()` with
  `ellmer::chat_anthropic()`. It will be removed in the next minor version.

## Bug fixes

* The parsers lived in `R/parsers/`, which R does not load, so the exported
  `parse_*()` functions were missing from the installed package. They are now
  in `R/`.
* The generated `NAMESPACE` was invalid and has been regenerated.
