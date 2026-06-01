#' Extract object types and properties from a DiscoverySource
#'
#' @param source A DiscoverySource object
#' @param existing_types List of already-discovered CandidateObjectType objects
#' @return A list of CandidateObjectType objects
extract_object_types <- function(source, existing_types = list(), llm_fn = NULL) {
  system_prompt <- paste0(
    "You are extracting the data model from a document. ",
    "Identify the main ENTITIES (things that have identity, can be stored in a database ",
    "table, and have multiple instances). For each entity:\n",
    "- Give it a CamelCase id and a human-readable display_name.\n",
    "- List its properties with id (snake_case), data type, and whether it is nullable.\n",
    "- Mark the primary key property.\n",
    "- Note which source table or section it came from.\n",
    "- Give a confidence score 0.0-1.0:\n",
    "  1.0 = Explicitly defined in a schema or data dictionary\n",
    "  0.9 = Clearly named in documentation with attributes listed\n",
    "  0.7 = Mentioned as a concept with some structure implied\n",
    "  0.5 = Inferred from context\n",
    "  0.2 = Speculative\n\n",
    "Do NOT include junction/association tables as object types.\n",
    "Do NOT include audit columns (created_at, updated_at, deleted_at) as properties ",
    "unless they are semantically meaningful to the domain.\n\n",
    "Respond ONLY with a valid JSON object. No preamble, no markdown fences.\n",
    "Schema: {\"object_types\": [{\"id\": \"string\", \"display_name\": \"string\", ",
    "\"description\": \"string\", \"source_ref\": \"string\", \"confidence\": 0.0, ",
    "\"primary_key\": {\"property_id\": \"string\", \"type\": \"string\"}, ",
    "\"properties\": [{\"id\": \"string\", \"type\": \"string\", \"nullable\": true, ",
    "\"description\": \"string\"}]}]}"
  )

  existing_ids <- paste(sapply(existing_types, function(t) t$id), collapse = ", ")

  user_prompt <- paste0(
    if (nzchar(existing_ids)) paste0("Already discovered object types: ", existing_ids, "\n\n") else "",
    "Source: ", source$source_label, " (type: ", source$source_type, ")\n\n",
    source$raw_text
  )

  result <- call_llm(llm_fn, system_prompt, user_prompt, response_format = "json")

  object_types_raw <- result$object_types %||% list()

  lapply(object_types_raw, function(ot) {
    props <- lapply(ot$properties %||% list(), function(p) {
      list(
        id          = p$id %||% "unknown",
        type        = normalise_type(p$type %||% "string"),
        nullable    = isTRUE(p$nullable),
        description = p$description %||% ""
      )
    })

    new_candidate_object_type(
      id           = ot$id %||% "Unknown",
      display_name = ot$display_name %||% ot$id %||% "Unknown",
      description  = ot$description %||% "",
      source_refs  = list(new_source_ref(source$source_id, source$source_label, ot$source_ref %||% "")),
      confidence   = as.numeric(ot$confidence %||% 0.5),
      primary_key  = ot$primary_key,
      properties   = props
    )
  })
}

#' Extract link types from a DiscoverySource given known object types
#'
#' @param source A DiscoverySource object
#' @param object_types List of CandidateObjectType objects found in pass 1
#' @return A list of CandidateLinkType objects
extract_link_types <- function(source, object_types, llm_fn = NULL) {
  type_list <- paste(
    sapply(object_types, function(t) sprintf("  - %s (%s)", t$id, t$display_name)),
    collapse = "\n"
  )

  system_prompt <- paste0(
    "You are identifying RELATIONSHIPS between object types in a data model.\n\n",
    "Given these object types:\n", type_list, "\n\n",
    "Identify relationships between them. For each relationship:\n",
    "- Give it a snake_case id.\n",
    "- Specify the from and to object type ids (must match the ids above).\n",
    "- Specify cardinality: one_to_one, one_to_many, or many_to_many.\n",
    "- Specify directed: true (asymmetric) or false (symmetric).\n",
    "- Note the source evidence (foreign key, document reference, etc.).\n",
    "- Give a confidence score 0.0-1.0.\n\n",
    "Junction tables (e.g. connecting two types) become link types, not object types.\n\n",
    "Respond ONLY with a valid JSON object. No preamble, no markdown fences.\n",
    "Schema: {\"link_types\": [{\"id\": \"string\", \"display_name\": \"string\", ",
    "\"from_type_id\": \"string\", \"to_type_id\": \"string\", ",
    "\"cardinality\": \"one_to_many\", \"directed\": true, ",
    "\"description\": \"string\", \"source_ref\": \"string\", \"confidence\": 0.0}]}"
  )

  user_prompt <- paste0(
    "Source: ", source$source_label, " (type: ", source$source_type, ")\n\n",
    source$raw_text
  )

  result <- call_llm(llm_fn, system_prompt, user_prompt, response_format = "json")
  link_types_raw <- result$link_types %||% list()

  valid_ids <- sapply(object_types, function(t) t$id)

  purrr::keep(
    lapply(link_types_raw, function(lt) {
      from_id <- lt$from_type_id %||% ""
      to_id   <- lt$to_type_id %||% ""

      if (!from_id %in% valid_ids || !to_id %in% valid_ids) return(NULL)

      new_candidate_link_type(
        id           = lt$id %||% "unknown_link",
        display_name = lt$display_name %||% lt$id %||% "Unknown Link",
        from_type_id = from_id,
        to_type_id   = to_id,
        cardinality  = lt$cardinality %||% "one_to_many",
        directed     = isTRUE(lt$directed %||% TRUE),
        description  = lt$description %||% "",
        source_refs  = list(new_source_ref(source$source_id, source$source_label, lt$source_ref %||% "")),
        confidence   = as.numeric(lt$confidence %||% 0.5)
      )
    }),
    Negate(is.null)
  )
}

#' Extract action types from a DiscoverySource
#'
#' @param source A DiscoverySource object
#' @param object_types List of CandidateObjectType objects
#' @return A list of CandidateActionType objects
extract_action_types <- function(source, object_types, llm_fn = NULL) {
  type_list <- paste(sapply(object_types, function(t) t$id), collapse = ", ")

  system_prompt <- paste0(
    "You are identifying ACTIONS or OPERATIONS on data entities.\n\n",
    "Known object types: ", type_list, "\n\n",
    "Look for: verbs in documentation, API endpoints (POST/PUT/DELETE), stored procedures, ",
    "business rules, and status transitions. For each action:\n",
    "- Give it a snake_case id and display_name.\n",
    "- Specify which object type it applies to (if clear).\n",
    "- Give a short description.\n",
    "- Give a confidence score.\n\n",
    "Respond ONLY with a valid JSON object. No preamble, no markdown fences.\n",
    "Schema: {\"action_types\": [{\"id\": \"string\", \"display_name\": \"string\", ",
    "\"object_type_id\": \"string\", \"description\": \"string\", ",
    "\"source_ref\": \"string\", \"confidence\": 0.0}]}"
  )

  user_prompt <- paste0(
    "Source: ", source$source_label, "\n\n",
    source$raw_text
  )

  result <- call_llm(llm_fn, system_prompt, user_prompt, response_format = "json")
  action_types_raw <- result$action_types %||% list()

  lapply(action_types_raw, function(at) {
    new_candidate_action_type(
      id             = at$id %||% "unknown_action",
      display_name   = at$display_name %||% at$id %||% "Unknown Action",
      object_type_id = at$object_type_id %||% NULL,
      description    = at$description %||% "",
      source_refs    = list(new_source_ref(source$source_id, source$source_label, at$source_ref %||% "")),
      confidence     = as.numeric(at$confidence %||% 0.5)
    )
  })
}

#' Extract concept hints from a DiscoverySource
#'
#' @param source A DiscoverySource object
#' @param object_types List of CandidateObjectType objects
#' @return A list of CandidateConceptHint objects
extract_concept_hints <- function(source, object_types, llm_fn = NULL) {
  type_list <- paste(
    sapply(object_types, function(t) {
      prop_names <- paste(sapply(t$properties, function(p) p$id), collapse = ", ")
      sprintf("  - %s (properties: %s)", t$id, prop_names)
    }),
    collapse = "\n"
  )

  system_prompt <- paste0(
    "You are identifying BUSINESS RULES and CONCEPTS in a data model.\n\n",
    "Known object types with properties:\n", type_list, "\n\n",
    "Look for: business rules expressed as conditions, threshold definitions, ",
    "eligibility criteria, classification rules, and status categories.\n",
    "For each concept:\n",
    "- Give it a snake_case id and display_name.\n",
    "- Specify which object type it applies to.\n",
    "- Suggest a SQL expression (sql_expr) that would evaluate to TRUE for matching rows.\n",
    "- Give a description.\n",
    "- Give a confidence score.\n\n",
    "Respond ONLY with a valid JSON object. No preamble, no markdown fences.\n",
    "Schema: {\"concept_hints\": [{\"id\": \"string\", \"display_name\": \"string\", ",
    "\"object_type_id\": \"string\", \"sql_expr\": \"string\", ",
    "\"description\": \"string\", \"source_ref\": \"string\", \"confidence\": 0.0}]}"
  )

  user_prompt <- paste0(
    "Source: ", source$source_label, "\n\n",
    source$raw_text
  )

  result <- call_llm(llm_fn, system_prompt, user_prompt, response_format = "json")
  hints_raw <- result$concept_hints %||% list()

  lapply(hints_raw, function(ch) {
    new_candidate_concept_hint(
      id             = ch$id %||% "unknown_concept",
      display_name   = ch$display_name %||% ch$id %||% "Unknown Concept",
      object_type_id = ch$object_type_id %||% NULL,
      sql_expr       = ch$sql_expr %||% "",
      description    = ch$description %||% "",
      source_refs    = list(new_source_ref(source$source_id, source$source_label, ch$source_ref %||% "")),
      confidence     = as.numeric(ch$confidence %||% 0.5)
    )
  })
}

# Chunk a large source into smaller pieces for LLM processing
chunk_source <- function(source, max_tokens = 8000L) {
  chars_per_token <- 4L
  max_chars <- max_tokens * chars_per_token
  text <- source$raw_text

  if (nchar(text) <= max_chars) return(list(source))

  if (source$source_type %in% c("sql_ddl", "db_schema") && !is.null(source$structured$tables)) {
    # Split by table
    tables <- source$structured$tables
    chunks <- lapply(tables, function(tbl) {
      tbl_text <- sprintf("TABLE %s:\n%s", tbl$name,
        paste(sapply(tbl$columns, function(col) {
          sprintf("  %s %s%s", col$name, col$raw_type %||% col$type,
                  if (isTRUE(col$pk)) " PRIMARY KEY" else if (!col$nullable) " NOT NULL" else "")
        }), collapse = "\n")
      )
      s <- source
      s$source_id  <- new_uuid()
      s$raw_text   <- tbl_text
      s$structured <- list(tables = list(tbl))
      s$source_label <- paste0(source$source_label, "/", tbl$name)
      s
    })
    return(chunks)
  }

  # Generic split by character count
  n_chunks <- ceiling(nchar(text) / max_chars)
  chunk_size <- ceiling(nchar(text) / n_chunks)

  lapply(seq_len(n_chunks), function(i) {
    start <- (i - 1L) * chunk_size + 1L
    end   <- min(i * chunk_size, nchar(text))
    s <- source
    s$source_id    <- new_uuid()
    s$raw_text     <- substr(text, start, end)
    s$source_label <- paste0(source$source_label, " [chunk ", i, "/", n_chunks, "]")
    s
  })
}
