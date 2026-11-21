# ellmer type specifications for every structured LLM call.
#
# Each spec mirrors the JSON schema described in the matching prompt, so the
# same answer shape comes back whether the provider supports
# `$chat_structured()` or we fall back to prompt-and-parse.
#
# These are functions (not objects) so that ellmer is only needed when a call is
# actually made.

.llm_type_confidence <- function() {
  ellmer::type_number("Confidence score between 0.0 and 1.0")
}

.llm_type_object_types <- function() {
  ellmer::type_object(
    object_types = ellmer::type_array(
      ellmer::type_object(
        id           = ellmer::type_string("CamelCase identifier"),
        display_name = ellmer::type_string(),
        description  = ellmer::type_string(required = FALSE),
        source_ref   = ellmer::type_string("Source table or section", required = FALSE),
        confidence   = .llm_type_confidence(),
        primary_key  = ellmer::type_object(
          property_id = ellmer::type_string(),
          type        = ellmer::type_string(),
          .required   = FALSE
        ),
        properties = ellmer::type_array(
          ellmer::type_object(
            id          = ellmer::type_string("snake_case identifier"),
            type        = ellmer::type_string("Data type"),
            nullable    = ellmer::type_boolean(required = FALSE),
            description = ellmer::type_string(required = FALSE)
          )
        )
      )
    )
  )
}

.llm_type_link_types <- function() {
  ellmer::type_object(
    link_types = ellmer::type_array(
      ellmer::type_object(
        id           = ellmer::type_string("snake_case identifier"),
        display_name = ellmer::type_string(),
        from_type_id = ellmer::type_string(),
        to_type_id   = ellmer::type_string(),
        cardinality  = ellmer::type_enum(c("one_to_one", "one_to_many", "many_to_many")),
        directed     = ellmer::type_boolean(),
        description  = ellmer::type_string(required = FALSE),
        source_ref   = ellmer::type_string(required = FALSE),
        confidence   = .llm_type_confidence()
      )
    )
  )
}

.llm_type_action_types <- function() {
  ellmer::type_object(
    action_types = ellmer::type_array(
      ellmer::type_object(
        id             = ellmer::type_string("snake_case identifier"),
        display_name   = ellmer::type_string(),
        object_type_id = ellmer::type_string(required = FALSE),
        description    = ellmer::type_string(required = FALSE),
        source_ref     = ellmer::type_string(required = FALSE),
        confidence     = .llm_type_confidence()
      )
    )
  )
}

.llm_type_concept_hints <- function() {
  ellmer::type_object(
    concept_hints = ellmer::type_array(
      ellmer::type_object(
        id             = ellmer::type_string("snake_case identifier"),
        display_name   = ellmer::type_string(),
        object_type_id = ellmer::type_string(),
        sql_expr       = ellmer::type_string("SQL expression that is TRUE for matching rows"),
        description    = ellmer::type_string(required = FALSE),
        source_ref     = ellmer::type_string(required = FALSE),
        confidence     = .llm_type_confidence()
      )
    )
  )
}

# Entity properties are free-form, which structured output cannot express as a
# map, so they come back as [{property, value}] pairs and are turned into a
# named list by .property_pairs_to_list().
.llm_type_population <- function() {
  ellmer::type_object(
    entities = ellmer::type_array(
      ellmer::type_object(
        temp_id    = ellmer::type_string(),
        type_id    = ellmer::type_string(),
        properties = ellmer::type_array(
          ellmer::type_object(
            property = ellmer::type_string(),
            value    = ellmer::type_string("Value as text; omit if unknown", required = FALSE)
          )
        ),
        excerpt    = ellmer::type_string(required = FALSE),
        confidence = .llm_type_confidence()
      )
    ),
    relationships = ellmer::type_array(
      ellmer::type_object(
        link_type_id = ellmer::type_string(),
        from_temp_id = ellmer::type_string(),
        to_temp_id   = ellmer::type_string(),
        evidence     = ellmer::type_string(required = FALSE),
        confidence   = .llm_type_confidence()
      )
    )
  )
}

.llm_type_mcp_action <- function() {
  ellmer::type_object(
    action = ellmer::type_enum(c(
      "add_object_type", "add_link_type", "add_concept_hint", "add_action_type",
      "approve", "reject", "summarise", "generate_bundle"
    )),
    id             = ellmer::type_string(required = FALSE),
    display_name   = ellmer::type_string(required = FALSE),
    description    = ellmer::type_string(required = FALSE),
    properties     = ellmer::type_array(
      ellmer::type_object(id = ellmer::type_string(), type = ellmer::type_string()),
      required = FALSE
    ),
    from_type_id   = ellmer::type_string(required = FALSE),
    to_type_id     = ellmer::type_string(required = FALSE),
    cardinality    = ellmer::type_string(required = FALSE),
    object_type_id = ellmer::type_string(required = FALSE),
    sql_expr       = ellmer::type_string(required = FALSE),
    element_type   = ellmer::type_string(required = FALSE),
    bundle_id      = ellmer::type_string(required = FALSE),
    bundle_name    = ellmer::type_string(required = FALSE)
  )
}

# Look up a type spec by name. Returns NULL when ellmer is not installed, so
# callers simply use the prompt-and-parse path (tests run without ellmer this
# way too).
.llm_type <- function(name) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(NULL)
  switch(name,
    object_types  = .llm_type_object_types(),
    link_types    = .llm_type_link_types(),
    action_types  = .llm_type_action_types(),
    concept_hints = .llm_type_concept_hints(),
    population    = .llm_type_population(),
    mcp_action    = .llm_type_mcp_action(),
    rlang::abort(paste0("Unknown LLM type spec: ", name))
  )
}

# [{property, value}] -> list(property = value). Already-named lists (from the
# text fallback path) pass through unchanged.
.property_pairs_to_list <- function(props) {
  if (is.null(props) || length(props) == 0) return(list())
  if (!is.null(names(props)) && all(nzchar(names(props)))) return(props)
  is_pair <- vapply(props, function(p) is.list(p) && !is.null(p$property), logical(1))
  if (!all(is_pair)) return(props)
  out <- lapply(props, function(p) p$value)
  names(out) <- vapply(props, function(p) as.character(p$property), character(1))
  out
}
