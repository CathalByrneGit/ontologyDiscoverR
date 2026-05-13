#' Add an interactive ontology-building tool to an MCP server
#'
#' If `ontologyMCP` is installed, registers a "build_ontology" tool that lets
#' an LLM agent interactively build and modify the ontology through conversation.
#'
#' @param mcp_server An MCP server object (from ontologyMCP)
#' @param session A DiscoverySession object
#' @return The MCP server with the tool registered
#' @export
dis_add_mcp_tool <- function(mcp_server, session) {
  if (!requireNamespace("ontologyMCP", quietly = TRUE)) {
    rlang::abort("Package 'ontologyMCP' is required for dis_add_mcp_tool(). It is an optional companion package.")
  }

  session_env <- new.env(parent = emptyenv())
  session_env$session <- session

  ontologyMCP::mcp_add_tool(
    mcp_server,
    name        = "build_ontology",
    description = paste0(
      "Add or modify ontology elements (object types, link types, actions, concepts) ",
      "in the current discovery session. Accepts natural language instructions such as:\n",
      "  'Add an object type for Hospital with properties name, capacity, region'\n",
      "  'Add a link from Hospital to Patient called Admission (many_to_many)'\n",
      "  'Add a concept called high_risk for Patient where age > 65'\n",
      "  'Approve all pending object types'\n",
      "  'Reject the concept hint low_risk'"
    ),
    input_schema = list(
      type       = "object",
      properties = list(
        instruction = list(type = "string", description = "Natural language ontology modification instruction")
      ),
      required = list("instruction")
    ),
    handler = function(params) {
      instruction <- params$instruction

      result <- tryCatch(
        .handle_mcp_instruction(instruction, session_env),
        error = function(e) list(success = FALSE, message = conditionMessage(e))
      )

      list(
        content = list(list(
          type = "text",
          text = if (isTRUE(result$success)) {
            paste0("Done: ", result$message)
          } else {
            paste0("Error: ", result$message)
          }
        ))
      )
    }
  )

  mcp_server
}

.handle_mcp_instruction <- function(instruction, session_env) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(api_key)) {
    return(list(success = FALSE, message = "ANTHROPIC_API_KEY not set"))
  }

  sess <- session_env$session

  existing_types <- paste(
    sapply(sess$candidates$object_types, function(t) t$id),
    collapse = ", "
  )

  system_prompt <- paste0(
    "You are helping to build an ontology. The current object types are: ",
    existing_types, ".\n\n",
    "Parse the user's instruction and return a JSON action object.\n",
    "Supported action types:\n",
    "  add_object_type: {action: 'add_object_type', id, display_name, properties: [{id, type}], description}\n",
    "  add_link_type: {action: 'add_link_type', id, display_name, from_type_id, to_type_id, cardinality}\n",
    "  add_concept_hint: {action: 'add_concept_hint', id, display_name, object_type_id, sql_expr, description}\n",
    "  add_action_type: {action: 'add_action_type', id, display_name, object_type_id, description}\n",
    "  approve: {action: 'approve', element_type: 'object_type|link_type|all', filter: 'all|pending'}\n",
    "  reject: {action: 'reject', element_type, id}\n\n",
    "Respond ONLY with valid JSON. No preamble."
  )

  parsed <- call_claude(system_prompt, instruction, response_format = "json")

  action <- parsed$action %||% "unknown"

  if (action == "add_object_type") {
    props <- lapply(parsed$properties %||% list(), function(p) {
      list(id = p$id, type = normalise_type(p$type %||% "string"), nullable = TRUE, description = "")
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
      session_env$session$candidates$object_types, list(cand)
    )
    list(success = TRUE, message = paste0("Added object type '", cand$id, "'"))

  } else if (action == "add_link_type") {
    cand <- new_candidate_link_type(
      id           = parsed$id %||% "unknown_link",
      display_name = parsed$display_name %||% parsed$id %||% "Unknown Link",
      from_type_id = parsed$from_type_id %||% "",
      to_type_id   = parsed$to_type_id %||% "",
      cardinality  = parsed$cardinality %||% "one_to_many",
      confidence   = 0.9,
      source_refs  = list(new_source_ref("mcp", "MCP conversation", instruction))
    )
    cand$status <- "approved"
    session_env$session$candidates$link_types <- c(
      session_env$session$candidates$link_types, list(cand)
    )
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
      session_env$session$candidates$concept_hints, list(cand)
    )
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
      session_env$session$candidates$action_types, list(cand)
    )
    list(success = TRUE, message = paste0("Added action type '", cand$id, "'"))

  } else if (action == "approve") {
    et <- parsed$element_type %||% "all"
    if (et %in% c("object_type", "all")) {
      session_env$session$candidates$object_types <- lapply(
        session_env$session$candidates$object_types,
        function(c) { if (c$status == "pending") c$status <- "approved"; c }
      )
    }
    if (et %in% c("link_type", "all")) {
      session_env$session$candidates$link_types <- lapply(
        session_env$session$candidates$link_types,
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

  } else {
    list(success = FALSE, message = paste0("Unknown action: ", action))
  }
}
