#' Normalise a raw type string to a canonical property type
#'
#' @param raw_type Character string of the raw type (e.g. "varchar(255)", "TEXT", "int")
#' @return One of: "string", "integer", "number", "boolean", "date", "datetime", "object", "array"
#' @export
normalise_type <- function(raw_type) {
  if (is.na(raw_type) || is.null(raw_type) || !nzchar(raw_type)) return("string")

  t <- trimws(tolower(raw_type))
  # Strip size/precision modifiers like varchar(255)
  t <- sub("\\(.*\\)$", "", t)
  t <- trimws(t)

  if (t %in% c("varchar", "text", "char", "nvarchar", "nchar", "string", "str",
               "character", "character varying", "tinytext", "mediumtext", "longtext",
               "clob", "name", "citext", "uuid")) {
    "string"
  } else if (t %in% c("int", "integer", "bigint", "smallint", "tinyint", "serial",
                       "bigserial", "int2", "int4", "int8", "mediumint")) {
    "integer"
  } else if (t %in% c("float", "double", "numeric", "real", "decimal",
                       "double precision", "float4", "float8", "money",
                       "number")) {
    "number"
  } else if (t %in% c("bool", "boolean", "bit", "tinyint(1)")) {
    "boolean"
  } else if (t == "date") {
    "date"
  } else if (t %in% c("timestamp", "datetime", "timestamp without time zone",
                       "timestamp with time zone", "timestamptz", "time")) {
    "datetime"
  } else if (t %in% c("jsonb", "json", "object", "hstore")) {
    "object"
  } else if (t %in% c("array", "_text", "_int4", "_int8")) {
    "array"
  } else {
    rlang::warn(paste0("Unknown type '", raw_type, "'; defaulting to 'string'"))
    "string"
  }
}

#' Validate a candidate object
#'
#' @param candidate A candidate object (CandidateObjectType, CandidateLinkType, etc.)
#' @return Invisibly returns the candidate; aborts if invalid
validate_candidate <- function(candidate) {
  stopifnot(is.list(candidate))
  required <- c("candidate_id", "status", "confidence")
  missing <- setdiff(required, names(candidate))
  if (length(missing) > 0) {
    rlang::abort(paste0("Candidate missing required fields: ", paste(missing, collapse = ", ")))
  }
  if (!candidate$status %in% c("pending", "approved", "rejected", "merged")) {
    rlang::abort(paste0("Invalid candidate status: ", candidate$status))
  }
  if (!is.numeric(candidate$confidence) || candidate$confidence < 0 || candidate$confidence > 1) {
    rlang::abort("Candidate confidence must be numeric between 0 and 1")
  }
  invisible(candidate)
}
