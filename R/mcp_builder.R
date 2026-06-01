#' Add an interactive ontology-building tool to an MCP server
#'
#' Registers a "build_ontology" tool on the MCP server backed by the given
#' DiscoverySession. The tool accepts natural language instructions and uses
#' the session's configured LLM provider (respects the `llm` argument passed
#' to `discover_session()`).
#'
#' @param mcp_server An MCP server object (from ontologyMCP)
#' @param session A DiscoverySession object
#' @return The MCP server with the tool registered
#' @export
dis_add_mcp_tool <- function(mcp_server, session) {
  if (!requireNamespace("ontologyMCP", quietly = TRUE)) {
    rlang::abort("Package 'ontologyMCP' is required. It is an optional companion package.")
  }
  stopifnot(inherits(session, "DiscoverySession"))

  session_env         <- new.env(parent = emptyenv())
  session_env$session <- session

  ontologyMCP::mcp_add_tool(
    mcp_server,
    name        = "build_ontology",
    description = paste0(
      "Add or modify ontology elements (object types, link types, actions, concepts), ",
      "approve/reject candidates, summarise findings, or generate a bundle.\n",
      "Example instructions:\n",
      "  'Add an object type for Hospital with properties name, capacity, region'\n",
      "  'Add a link from Hospital to Patient called Admission (many_to_many)'\n",
      "  'Add a concept called high_risk for Patient where age > 65'\n",
      "  'Approve all pending object types'\n",
      "  'Reject the concept hint low_risk'\n",
      "  'Summarise what was found'\n",
      "  'Generate the bundle as hospital-v1'"
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        instruction = list(type = "string",
                           description = "Natural language ontology instruction")
      ),
      required = list("instruction")
    ),
    handler = function(params) {
      result <- tryCatch(
        .handle_dis_mcp_instruction(params$instruction, session_env),
        error = function(e) list(success = FALSE, message = conditionMessage(e))
      )
      list(content = list(list(
        type = "text",
        text = if (isTRUE(result$success)) paste0("Done: ", result$message)
               else                        paste0("Error: ", result$message)
      )))
    }
  )

  mcp_server
}

#' Add a population query tool to an MCP server
#'
#' Registers a "query_population" tool backed by a PopulateSession. Lets an LLM
#' agent inspect extracted instances, get entity counts, retrieve the edge list,
#' and review schema amendments through conversation.
#'
#' @param mcp_server An MCP server object (from ontologyMCP)
#' @param pop_sess A PopulateSession object
#' @return The MCP server with the tool registered
#' @export
pop_add_mcp_tool <- function(mcp_server, pop_sess) {
  if (!requireNamespace("ontologyMCP", quietly = TRUE)) {
    rlang::abort("Package 'ontologyMCP' is required. It is an optional companion package.")
  }
  stopifnot(inherits(pop_sess, "PopulateSession"))

  env         <- new.env(parent = emptyenv())
  env$pop_sess <- pop_sess

  ontologyMCP::mcp_add_tool(
    mcp_server,
    name        = "query_population",
    description = paste0(
      "Query and manage a populated instance dataset.\n",
      "Example instructions:\n",
      "  'How many Person instances were found?'\n",
      "  'List all Company instances'\n",
      "  'Show relationships involving Epstein'\n",
      "  'Show schema amendments'\n",
      "  'Approve amendment new_property on Person'\n",
      "  'Show the edge list summary'\n",
      "  'Save the session to investigation.rds'"
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        instruction = list(type = "string",
                           description = "Natural language population query or command")
      ),
      required = list("instruction")
    ),
    handler = function(params) {
      result <- tryCatch(
        .handle_pop_mcp_instruction(params$instruction, env),
        error = function(e) list(success = FALSE, message = conditionMessage(e))
      )
      list(content = list(list(
        type = "text",
        text = if (isTRUE(result$success)) result$message
               else                        paste0("Error: ", result$message)
      )))
    }
  )

  mcp_server
}

# ---------------------------------------------------------------------------
# Internal: handle a discovery-session MCP instruction
# ---------------------------------------------------------------------------

.handle_dis_mcp_instruction <- function(instruction, session_env) {
  sess   <- session_env$session
  llm_fn <- sess$llm_fn

  existing_types <- paste(
    sapply(sess$candidates$object_types, function(t) t$id),
    collapse = ", "
  )

  system_prompt <- paste0(
    "You are helping to build an ontology. Current object types: ",
    if (nzchar(existing_types)) existing_types else "(none)", ".\n\n",
    "Parse the instruction and return a JSON action object. Supported actions:\n",
    "  add_object_type:  {action, id, display_name, properties:[{id,type}], description}\n",
    "  add_link_type:    {action, id, display_name, from_type_id, to_type_id, cardinality}\n",
    "  add_concept_hint: {action, id, display_name, object_type_id, sql_expr, description}\n",
    "  add_action_type:  {action, id, display_name, object_type_id, description}\n",
    "  approve:          {action, element_type:'object_type|link_type|action_type|concept_hint|all'}\n",
    "  reject:           {action, element_type, id}\n",
    "  summarise:        {action:'summarise'}\n",
    "  generate_bundle:  {action:'generate_bundle', bundle_id, bundle_name}\n\n",
    "Respond ONLY with valid JSON. No preamble."
  )

  parsed <- call_llm(llm_fn, system_prompt, instruction, response_format = "json")
  action <- parsed$action %||% "unknown"

  if (action == "add_object_type") {
    props <- lapply(parsed$properties %||% list(), function(p) {
      list(id = p$id, type = normalise_type(p$type %||% "string"),
           nullable = TRUE, description = "")
    })
    cand <- new_candidate_object_type(
      id           = parsed$id %||% "Unknown",
      display_name = parsed$display_name %||% parsed$id %||% "Unknown",
      description  = parsed$description %||% "",
      properties   = props,
      confidence   = 0.9,
      source_refs  = list(new_source_ref("mcp", "MCP conversation", instruction))
    )
    cand$status <- "approved"
    session_env$session$candidates$object_types <- c(
      session_env$session$candidates$object_types, list(cand))
    list(success = TRUE, message = paste0("Added object type '", cand$id, "'"))

  } else if (action == "add_link_type") {
    cand <- new_candidate_link_type(
      id           = parsed$id %||% "unknown_link",
      display_name = parsed$display_name %||% parsed$id %||% "Unknown Link",
      from_type_id = parsed$from_type_id %||% "",
      to_type_id   = parsed$to_type_id   %||% "",
      cardinality  = parsed$cardinality  %||% "one_to_many",
      confidence   = 0.9,
      source_refs  = list(new_source_ref("mcp", "MCP conversation", instruction))
    )
    cand$status <- "approved"
    session_env$session$candidates$link_types <- c(
      session_env$session$candidates$link_types, list(cand))
    list(success = TRUE, message = paste0("Added link type '", cand$id, "'"))

  } else if (action == "add_concept_hint") {
    cand <- new_candidate_concept_hint(
      id             = parsed$id %||% "unknown_concept",
      display_name   = parsed$display_name %||% parsed$id %||% "Unknown Concept",
      object_type_id = parsed$object_type_id %||% NULL,
      sql_expr       = parsed$sql_expr %||% "",
      description    = parsed$description %||% "",
      confidence     = 0.9,
      source_refs    = list(new_source_ref("mcp", "MCP conversation", instruction))
    )
    cand$status <- "approved"
    session_env$session$candidates$concept_hints <- c(
      session_env$session$candidates$concept_hints, list(cand))
    list(success = TRUE, message = paste0("Added concept hint '", cand$id, "'"))

  } else if (action == "add_action_type") {
    cand <- new_candidate_action_type(
      id             = parsed$id %||% "unknown_action",
      display_name   = parsed$display_name %||% parsed$id %||% "Unknown Action",
      object_type_id = parsed$object_type_id %||% NULL,
      description    = parsed$description %||% "",
      confidence     = 0.9,
      source_refs    = list(new_source_ref("mcp", "MCP conversation", instruction))
    )
    cand$status <- "approved"
    session_env$session$candidates$action_types <- c(
      session_env$session$candidates$action_types, list(cand))
    list(success = TRUE, message = paste0("Added action type '", cand$id, "'"))

  } else if (action == "approve") {
    et <- parsed$element_type %||% "all"
    fields <- if (et == "all") {
      c("object_types", "link_types", "action_types", "concept_hints")
    } else {
      paste0(et, "s")
    }
    for (field in fields) {
      session_env$session$candidates[[field]] <- lapply(
        session_env$session$candidates[[field]],
        function(c) { if (c$status == "pending") c$status <- "approved"; c }
      )
    }
    list(success = TRUE, message = paste0("Approved ", et, " candidates"))

  } else if (action == "reject") {
    et  <- parsed$element_type %||% "object_type"
    rid <- parsed$id %||% ""
    field <- switch(et,
      object_type  = "object_types",
      link_type    = "link_types",
      action_type  = "action_types",
      concept_hint = "concept_hints",
      "object_types"
    )
    session_env$session$candidates[[field]] <- lapply(
      session_env$session$candidates[[field]],
      function(c) { if (c$id == rid) c$status <- "rejected"; c }
    )
    list(success = TRUE, message = paste0("Rejected '", rid, "'"))

  } else if (action == "summarise") {
    sess <- session_env$session
    ot   <- sess$candidates$object_types
    lt   <- sess$candidates$link_types
    msg  <- sprintf(
      "Session '%s': %d sources | %d object types (%d approved) | %d link types (%d approved) | extracted: %s",
      sess$label,
      length(sess$sources),
      length(ot), sum(sapply(ot, function(x) x$status == "approved")),
      length(lt), sum(sapply(lt, function(x) x$status == "approved")),
      if (sess$extracted) "yes" else "no"
    )
    list(success = TRUE, message = msg)

  } else if (action == "generate_bundle") {
    bundle <- dis_to_bundle(
      session_env$session,
      bundle_id   = parsed$bundle_id   %||% paste0("bundle-", format(Sys.time(), "%Y%m%d")),
      bundle_name = parsed$bundle_name %||% session_env$session$label
    )
    session_env$session$last_bundle <- bundle
    msg <- sprintf("Bundle '%s' generated: %d object types, %d link types",
                   bundle$bundle_id, length(bundle$object_types), length(bundle$link_types))
    list(success = TRUE, message = msg)

  } else {
    list(success = FALSE, message = paste0("Unknown action: ", action))
  }
}

# ---------------------------------------------------------------------------
# Internal: handle a population-session MCP instruction
# ---------------------------------------------------------------------------

.handle_pop_mcp_instruction <- function(instruction, env) {
  pop    <- env$pop_sess
  schema <- pop$schema

  type_ids  <- sapply(schema$object_types %||% list(), function(t) t$id)
  n_by_type <- vapply(type_ids, function(tid) {
    sum(sapply(pop$instances$entities, function(e) e$type_id == tid))
  }, integer(1))

  summary_text <- paste(
    sprintf("  %s: %d instances", type_ids, n_by_type),
    collapse = "\n"
  )

  # Simple keyword dispatch — avoids an extra LLM call for common queries
  lower <- tolower(instruction)

  if (grepl("how many|count|summary|summarise|summarize", lower)) {
    n_ent <- length(pop$instances$entities)
    n_rel <- length(pop$instances$relationships)
    msg   <- sprintf(
      "Population session '%s': %d sources | %d entities | %d relationships | %d amendments\n\nEntities by type:\n%s",
      pop$label, length(pop$sources), n_ent, n_rel, length(pop$amendments), summary_text
    )
    return(list(success = TRUE, message = msg))
  }

  if (grepl("amendment", lower)) {
    amends <- pop$amendments
    if (length(amends) == 0) return(list(success = TRUE, message = "No schema amendments flagged."))
    rows <- sapply(amends, function(a) {
      sprintf("  [%s] %s.%s — %s (evidence: %s)",
              a$status, a$type_id, a$property_id %||% "(new type)",
              a$inferred_type %||% "", substr(a$evidence %||% "", 1, 60))
    })
    return(list(success = TRUE, message = paste(c("Schema amendments:", rows), collapse = "\n")))
  }

  if (grepl("edge.?list|edges|relationships|connections", lower)) {
    n_rel <- length(pop$instances$relationships)
    if (n_rel == 0) return(list(success = TRUE, message = "No relationships extracted yet."))
    type_counts <- table(sapply(pop$instances$relationships, function(r) r$link_type_id))
    rows <- paste(sprintf("  %s: %d", names(type_counts), as.integer(type_counts)), collapse = "\n")
    return(list(success = TRUE, message = paste0("Relationships by type:\n", rows)))
  }

  if (grepl("save", lower)) {
    path_match <- regmatches(instruction, regexpr("[\\w./]+\\.rds", instruction, perl = TRUE))
    path <- if (length(path_match) > 0) path_match[1] else "population_session.rds"
    pop_save(pop, path)
    env$pop_sess <- pop
    return(list(success = TRUE, message = paste0("Session saved to ", path)))
  }

  # For anything else, look up instances of a named type
  for (tid in type_ids) {
    if (grepl(tolower(tid), lower)) {
      type_entities <- Filter(function(e) e$type_id == tid, pop$instances$entities)
      if (length(type_entities) == 0) {
        return(list(success = TRUE, message = sprintf("No %s instances found.", tid)))
      }
      rows <- sapply(head(type_entities, 20), function(e) {
        name_val <- e$properties[["name"]] %||% e$properties[[1]] %||% e$instance_id
        sprintf("  [%.2f] %s", e$confidence, as.character(name_val))
      })
      msg <- sprintf("%d %s instance(s)%s:\n%s",
                     length(type_entities), tid,
                     if (length(type_entities) > 20) " (showing first 20)" else "",
                     paste(rows, collapse = "\n"))
      return(list(success = TRUE, message = msg))
    }
  }

  list(success = FALSE,
       message = paste0("Could not interpret: '", instruction,
                        "'. Try: 'How many X instances?', 'Show amendments', 'Show edge list', 'Save to file.rds'"))
}
