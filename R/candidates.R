#' Create a new CandidateObjectType
#'
#' @param id CamelCase identifier for the object type
#' @param display_name Human-readable display name
#' @param description Description of the object type
#' @param source_refs List of source references
#' @param confidence Confidence score 0.0-1.0
#' @param primary_key List with property_id and type
#' @param properties List of property definitions
#' @return A CandidateObjectType S3 object
#' @export
new_candidate_object_type <- function(id, display_name, description = "",
                                       source_refs = list(), confidence = 0.5,
                                       primary_key = NULL, properties = list()) {
  obj <- list(
    candidate_id  = new_uuid(),
    element_type  = "object_type",
    id            = id,
    display_name  = display_name,
    description   = description,
    source_refs   = source_refs,
    confidence    = confidence,
    primary_key   = primary_key,
    properties    = properties,
    status        = "pending",
    conflicts     = list(),
    merged_into   = NULL
  )
  class(obj) <- c("CandidateObjectType", "Candidate", "list")
  validate_candidate(obj)
  obj
}

#' Create a new CandidateLinkType
#'
#' @param id snake_case identifier for the link type
#' @param display_name Human-readable display name
#' @param from_type_id Object type id this link originates from
#' @param to_type_id Object type id this link points to
#' @param cardinality One of: one_to_one, one_to_many, many_to_many
#' @param directed Logical; whether link is directed
#' @param source_refs List of source references
#' @param confidence Confidence score 0.0-1.0
#' @return A CandidateLinkType S3 object
#' @export
new_candidate_link_type <- function(id, display_name, from_type_id, to_type_id,
                                     cardinality = "one_to_many", directed = TRUE,
                                     description = "", source_refs = list(),
                                     confidence = 0.5) {
  obj <- list(
    candidate_id  = new_uuid(),
    element_type  = "link_type",
    id            = id,
    display_name  = display_name,
    description   = description,
    from_type_id  = from_type_id,
    to_type_id    = to_type_id,
    cardinality   = cardinality,
    directed      = directed,
    source_refs   = source_refs,
    confidence    = confidence,
    status        = "pending",
    conflicts     = list(),
    merged_into   = NULL
  )
  class(obj) <- c("CandidateLinkType", "Candidate", "list")
  validate_candidate(obj)
  obj
}

#' Create a new CandidateActionType
#'
#' @param id snake_case identifier for the action type
#' @param display_name Human-readable display name
#' @param object_type_id Object type this action applies to
#' @param description Description of the action
#' @param source_refs List of source references
#' @param confidence Confidence score 0.0-1.0
#' @return A CandidateActionType S3 object
#' @export
new_candidate_action_type <- function(id, display_name, object_type_id = NULL,
                                       description = "", source_refs = list(),
                                       confidence = 0.5) {
  obj <- list(
    candidate_id    = new_uuid(),
    element_type    = "action_type",
    id              = id,
    display_name    = display_name,
    description     = description,
    object_type_id  = object_type_id,
    source_refs     = source_refs,
    confidence      = confidence,
    status          = "pending",
    conflicts       = list(),
    merged_into     = NULL
  )
  class(obj) <- c("CandidateActionType", "Candidate", "list")
  validate_candidate(obj)
  obj
}

#' Create a new CandidateConceptHint
#'
#' @param id snake_case identifier for the concept
#' @param display_name Human-readable display name
#' @param object_type_id Object type this concept applies to
#' @param sql_expr Suggested SQL expression for the concept
#' @param description Description of the concept
#' @param source_refs List of source references
#' @param confidence Confidence score 0.0-1.0
#' @return A CandidateConceptHint S3 object
#' @export
new_candidate_concept_hint <- function(id, display_name, object_type_id = NULL,
                                        sql_expr = "", description = "",
                                        source_refs = list(), confidence = 0.5) {
  obj <- list(
    candidate_id    = new_uuid(),
    element_type    = "concept_hint",
    id              = id,
    display_name    = display_name,
    description     = description,
    object_type_id  = object_type_id,
    sql_expr        = sql_expr,
    source_refs     = source_refs,
    confidence      = confidence,
    status          = "pending",
    conflicts       = list(),
    merged_into     = NULL
  )
  class(obj) <- c("CandidateConceptHint", "Candidate", "list")
  validate_candidate(obj)
  obj
}

#' Print method for Candidate objects
#' @export
print.Candidate <- function(x, ...) {
  cat(sprintf("<Candidate[%s]> %s (conf=%.2f, status=%s)\n",
              x$element_type, x$id, x$confidence, x$status))
  invisible(x)
}

# Helper: create a source_ref entry
new_source_ref <- function(source_id, source_label, excerpt = "") {
  list(source_id = source_id, source_label = source_label, excerpt = excerpt)
}
