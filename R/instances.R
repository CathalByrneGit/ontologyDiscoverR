#' Create a new EntityInstance
#'
#' An EntityInstance is a specific record of a known object type, extracted
#' from a document corpus and validated against the schema bundle.
#'
#' @param type_id The object type id this instance belongs to (must exist in schema)
#' @param properties Named list of property values (names must match schema properties)
#' @param source_refs List of source references (use new_source_ref())
#' @param confidence Confidence score 0.0-1.0
#' @param schema The ontology_bundle used for validation (optional; validates if provided)
#' @return An EntityInstance S3 object
#' @export
new_entity_instance <- function(type_id, properties = list(),
                                 source_refs = list(), confidence = 0.8,
                                 schema = NULL) {
  obj <- list(
    instance_id  = new_uuid(),
    type_id      = type_id,
    properties   = properties,
    source_refs  = source_refs,
    confidence   = confidence,
    status       = "pending",
    amendments   = list()   # schema amendment candidates flagged during extraction
  )
  class(obj) <- c("EntityInstance", "Instance", "list")

  if (!is.null(schema)) {
    obj <- .validate_entity_instance(obj, schema)
  }
  obj
}

#' Create a new RelationshipInstance
#'
#' A RelationshipInstance is a specific connection between two EntityInstances,
#' validated against a link type in the schema bundle.
#'
#' @param link_type_id The link type id (must exist in schema)
#' @param from_instance_id instance_id of the source EntityInstance
#' @param to_instance_id instance_id of the target EntityInstance
#' @param evidence Character string: the exact excerpt from the source that supports this relationship
#' @param source_refs List of source references
#' @param confidence Confidence score 0.0-1.0
#' @param schema The ontology_bundle used for validation (optional)
#' @return A RelationshipInstance S3 object
#' @export
new_relationship_instance <- function(link_type_id, from_instance_id, to_instance_id,
                                       evidence = "", source_refs = list(),
                                       confidence = 0.8, schema = NULL) {
  obj <- list(
    instance_id      = new_uuid(),
    link_type_id     = link_type_id,
    from_instance_id = from_instance_id,
    to_instance_id   = to_instance_id,
    evidence         = evidence,
    source_refs      = source_refs,
    confidence       = confidence,
    status           = "pending"
  )
  class(obj) <- c("RelationshipInstance", "Instance", "list")

  if (!is.null(schema)) {
    obj <- .validate_relationship_instance(obj, schema)
  }
  obj
}

#' Create a SchemaAmendment candidate
#'
#' Flagged when population discovers a property or type not present in the schema.
#' These can be reviewed and merged back into the schema.
#'
#' @param amendment_type One of "new_property", "new_type"
#' @param type_id The object type this amendment applies to
#' @param property_id The new property id (for new_property amendments)
#' @param inferred_type The inferred data type of the new property
#' @param evidence Example value or excerpt that prompted this amendment
#' @param source_refs List of source references
#' @return A SchemaAmendment S3 object
#' @export
new_schema_amendment <- function(amendment_type = c("new_property", "new_type"),
                                  type_id, property_id = NULL,
                                  inferred_type = "string",
                                  evidence = "", source_refs = list()) {
  amendment_type <- match.arg(amendment_type)
  obj <- list(
    amendment_id   = new_uuid(),
    amendment_type = amendment_type,
    type_id        = type_id,
    property_id    = property_id,
    inferred_type  = inferred_type,
    evidence       = evidence,
    source_refs    = source_refs,
    status         = "pending"
  )
  class(obj) <- c("SchemaAmendment", "list")
  obj
}

#' Print method for Instance objects
#' @export
print.Instance <- function(x, ...) {
  cat(sprintf("<Instance[%s]> type=%s conf=%.2f status=%s\n",
              class(x)[1], x$type_id %||% x$link_type_id, x$confidence, x$status))
  invisible(x)
}

# Validate an EntityInstance against a schema bundle
.validate_entity_instance <- function(instance, schema) {
  valid_type_ids <- sapply(schema$object_types %||% list(), function(t) t$id)

  if (!instance$type_id %in% valid_type_ids) {
    rlang::warn(sprintf(
      "EntityInstance has unknown type_id '%s' (not in schema). Flagged as amendment.",
      instance$type_id
    ))
    instance$amendments <- c(instance$amendments, list(
      new_schema_amendment("new_type", type_id = instance$type_id,
                           evidence = "Instance extracted during population")
    ))
    return(instance)
  }

  # Check for unknown properties
  schema_type <- schema$object_types[[which(valid_type_ids == instance$type_id)]]
  valid_prop_ids <- sapply(schema_type$properties %||% list(), function(p) p$id)

  unknown_props <- setdiff(names(instance$properties), valid_prop_ids)
  for (prop_id in unknown_props) {
    val <- instance$properties[[prop_id]]
    instance$amendments <- c(instance$amendments, list(
      new_schema_amendment(
        "new_property",
        type_id       = instance$type_id,
        property_id   = prop_id,
        inferred_type = normalise_type(.infer_value_type(val)),
        evidence      = as.character(val)[1]
      )
    ))
  }

  instance
}

# Validate a RelationshipInstance against a schema bundle
.validate_relationship_instance <- function(instance, schema) {
  valid_link_ids <- sapply(schema$link_types %||% list(), function(lt) lt$id)

  if (!instance$link_type_id %in% valid_link_ids) {
    rlang::warn(sprintf(
      "RelationshipInstance has unknown link_type_id '%s' (not in schema).",
      instance$link_type_id
    ))
  }
  instance
}

# Infer a type string from a scalar value
.infer_value_type <- function(val) {
  if (is.logical(val))                                      return("boolean")
  if (is.integer(val))                                      return("integer")
  if (is.numeric(val))                                      return("number")
  if (inherits(val, "Date"))                                return("date")
  if (inherits(val, "POSIXct") || inherits(val, "POSIXlt")) return("datetime")
  "string"
}
