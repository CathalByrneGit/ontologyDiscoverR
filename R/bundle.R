#' Export approved candidates as an ontologySpecR bundle
#'
#' Builds a canonical `ontology_bundle` with the ontologySpecR constructors
#' (`ontologySpecR::property_def()`, `ontologySpecR::primary_key_def()`,
#' `ontologySpecR::object_type()`, `ontologySpecR::link_type()`,
#' `ontologySpecR::action_type()`, `ontologySpecR::concept_def()` and
#' `ontologySpecR::bundle()`) and checks it with
#' `ontologySpecR::validate_bundle()` before returning it.
#'
#' Candidates are included when they are approved, or pending with confidence
#' of at least `min_confidence`. Link types whose endpoints are not included,
#' and action types or concepts whose object type is not included, are dropped
#' with a warning.
#'
#' Each element keeps its discovery provenance (candidate id, confidence,
#' sources and the `"provider/model"` that proposed it) under
#' `extensions$ontologyDiscoverR`.
#'
#' @param session A DiscoverySession object.
#' @param bundle_id Bundle id (letters, digits, `_`, `.`, `-`; at least 3
#'   characters, starting with a letter).
#' @param bundle_name Human-readable bundle name, stored in `metadata$name`.
#' @param min_confidence Minimum confidence for auto-including pending
#'   candidates (default 0.5).
#' @param include_concepts Whether to include concept hints as concept
#'   definitions (default `FALSE`).
#' @param bundle_version SemVer version of the bundle (default `"0.1.0"`).
#' @param concept_scope Scope given to every concept definition (default
#'   `"discovered"`).
#' @return An `ontology_bundle` that passes `ontologySpecR::validate_bundle()`.
#' @export
dis_to_bundle <- function(session, bundle_id, bundle_name,
                          min_confidence = 0.5, include_concepts = FALSE,
                          bundle_version = "0.1.0",
                          concept_scope = "discovered") {
  stopifnot(inherits(session, "DiscoverySession"))

  include_candidate <- function(cand) {
    identical(cand$status, "approved") ||
      (identical(cand$status, "pending") && isTRUE(cand$confidence >= min_confidence))
  }

  cand_ot <- Filter(include_candidate, session$candidates$object_types)
  cand_lt <- Filter(include_candidate, session$candidates$link_types)
  cand_at <- Filter(include_candidate, session$candidates$action_types)
  cand_ch <- if (isTRUE(include_concepts)) {
    Filter(include_candidate, session$candidates$concept_hints)
  } else {
    list()
  }

  # Name each list by position so built elements can be traced back to the
  # candidate they came from.
  name_by_pos <- function(x) {
    if (length(x)) names(x) <- as.character(seq_along(x))
    x
  }
  cand_ot <- name_by_pos(cand_ot)
  cand_lt <- name_by_pos(cand_lt)
  cand_at <- name_by_pos(cand_at)
  cand_ch <- name_by_pos(cand_ch)

  problems <- .new_problem_log()

  # Object types first: everything else refers to them by (spec) id.
  objects <- .build_each(cand_ot, "object_type", problems, .candidate_to_object_type)
  type_map <- stats::setNames(
    vapply(objects, function(o) o$id, character(1)),
    vapply(cand_ot[names(objects)], function(c) c$id, character(1))
  )

  links <- .build_each(
    .keep_with_targets(cand_lt, "link_type", type_map,
                       function(c) c(c$from_type_id, c$to_type_id)),
    "link_type", problems,
    function(c) .candidate_to_link_type(c, type_map)
  )
  actions <- .build_each(
    .keep_with_targets(cand_at, "action_type", type_map, function(c) c$object_type_id),
    "action_type", problems,
    function(c) .candidate_to_action_type(c, type_map)
  )
  concepts <- .build_each(
    .keep_with_targets(cand_ch, "concept_hint", type_map, function(c) c$object_type_id),
    "concept_hint", problems,
    function(c) .candidate_to_concept_def(c, type_map, concept_scope)
  )

  .check_duplicate_ids(objects,  cand_ot, "object_type",  problems)
  .check_duplicate_ids(links,    cand_lt, "link_type",    problems)
  .check_duplicate_ids(actions,  cand_at, "action_type",  problems)
  .check_duplicate_ids(concepts, cand_ch, "concept_hint", problems)
  .abort_on_problems(problems)

  b <- tryCatch(
    ontologySpecR::bundle(
      bundle_id      = bundle_id,
      bundle_version = bundle_version,
      objects        = unname(objects),
      links          = unname(links),
      actions        = unname(actions),
      concepts       = unname(concepts),
      metadata       = list(
        name      = bundle_name,
        createdAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ),
      extensions     = .bundle_extensions(session)
    ),
    error = function(e) {
      rlang::abort(c("Could not build the bundle.", x = conditionMessage(e)), call = NULL)
    }
  )

  .validate_or_abort(b, list(
    objects  = cand_ot[names(objects)],
    links    = cand_lt[names(links)],
    actions  = cand_at[names(actions)],
    concepts = cand_ch[names(concepts)]
  ))
  b
}

# ---------------------------------------------------------------------------
# One mapping helper per element type
# ---------------------------------------------------------------------------

# CandidateObjectType -> ontologySpecR::object_type()
#
# Candidate field            -> spec field
#   id                       -> id (sanitised by .spec_id())
#   display_name             -> display$name
#   description              -> display$description
#   properties[[i]]          -> properties (see .candidate_to_property_def())
#   primary_key$property_id  -> primaryKey$properties (natural key); when the
#                               candidate has none, a surrogate "<type>_id" key
#   source_table / source_refs[[1]]$excerpt
#                            -> source (kind = "table", table = ...)
#   provenance               -> extensions$ontologyDiscoverR
.candidate_to_object_type <- function(cand) {
  id <- .spec_id(cand$id, "object type", cand$id)

  props <- lapply(cand$properties %||% list(), function(p) {
    .candidate_to_property_def(p, owner = cand$id)
  })
  prop_ids <- vapply(props, function(p) p$id, character(1))
  # Keep the first of any properties that end up with the same id.
  keep  <- !duplicated(prop_ids)
  props <- props[keep]
  prop_ids <- prop_ids[keep]

  pk <- .candidate_primary_key(cand, prop_ids)
  if (!pk$id %in% prop_ids) {
    props <- c(props, list(ontologySpecR::property_def(
      id = pk$id, type = pk$type, nullable = FALSE,
      source_column = if (pk$strategy == "natural") pk$column else NULL
    )))
    prop_ids <- c(prop_ids, pk$id)
  }
  # A key column can never be null.
  props <- lapply(props, function(p) {
    if (identical(p$id, pk$id)) p$nullable <- FALSE
    p
  })

  ontologySpecR::object_type(
    id                  = id,
    properties          = props,
    primary_key         = ontologySpecR::primary_key_def(pk$id, strategy = pk$strategy),
    display_name        = .nz_or_null(cand$display_name),
    display_description = .nz_or_null(cand$description),
    source_kind         = "table",
    source_table        = .candidate_source_table(cand),
    extensions          = .provenance_extension(cand)
  )
}

# Candidate property list(id, type, nullable, description)
#   -> ontologySpecR::property_def()
# The original name is kept as source$column, since the spec id may have been
# sanitised.
.candidate_to_property_def <- function(p, owner) {
  raw_id <- as.character(p$id %||% "")
  ontologySpecR::property_def(
    id                  = .spec_id(raw_id, "property", paste0(owner, ".", raw_id)),
    type                = .to_spec_property_type(p$type, where = paste0(owner, ".", raw_id)),
    nullable            = !isFALSE(p$nullable),
    display_description = .nz_or_null(p$description),
    source_column       = if (nzchar(raw_id)) raw_id else NULL
  )
}

# CandidateLinkType -> ontologySpecR::link_type()
#   id, display_name, description -> id, display$name, display$description
#   from_type_id / to_type_id     -> from / to (mapped to the spec object ids)
#   cardinality (one_to_many)     -> cardinality ("one-to-many")
#   directed                      -> directed
.candidate_to_link_type <- function(cand, type_map) {
  ontologySpecR::link_type(
    id                  = .spec_id(cand$id, "link type", cand$id),
    from                = type_map[[cand$from_type_id]],
    to                  = type_map[[cand$to_type_id]],
    cardinality         = .to_spec_cardinality(cand$cardinality, cand$id),
    directed            = !isFALSE(cand$directed),
    display_name        = .nz_or_null(cand$display_name),
    display_description = .nz_or_null(cand$description),
    extensions          = .provenance_extension(cand)
  )
}

# CandidateActionType -> ontologySpecR::action_type()
#   object_type_id -> targets
.candidate_to_action_type <- function(cand, type_map) {
  ontologySpecR::action_type(
    id                  = .spec_id(cand$id, "action type", cand$id),
    targets             = type_map[[cand$object_type_id]],
    display_name        = .nz_or_null(cand$display_name),
    display_description = .nz_or_null(cand$description),
    extensions          = .provenance_extension(cand)
  )
}

# CandidateConceptHint -> ontologySpecR::concept_def()
#   object_type_id -> object_type_id; sql_expr -> sql_expr
#   Always version 1, status "draft": a discovered concept is a proposal.
.candidate_to_concept_def <- function(cand, type_map, scope) {
  sql <- trimws(as.character(cand$sql_expr %||% ""))
  if (!nzchar(sql)) {
    rlang::abort("Concept hint has an empty sql_expr.", call = NULL)
  }
  ontologySpecR::concept_def(
    id                  = .spec_id(cand$id, "concept", cand$id),
    object_type_id      = type_map[[cand$object_type_id]],
    scope               = scope,
    version             = 1L,
    sql_expr            = sql,
    status              = "draft",
    display_name        = .nz_or_null(cand$display_name),
    display_description = .nz_or_null(cand$description),
    extensions          = .provenance_extension(cand)
  )
}

# ---------------------------------------------------------------------------
# Field mapping helpers
# ---------------------------------------------------------------------------

# Data types accepted by ontologySpecR::property_def().
.spec_property_types <- c("string", "integer", "number", "boolean", "date", "datetime", "json")

# Types an LLM (or normalise_type()) may propose -> spec type.
.spec_property_type_map <- c(
  # string
  string = "string", str = "string", text = "string", char = "string",
  character = "string", varchar = "string", nvarchar = "string", uuid = "string",
  enum = "string", email = "string", url = "string", uri = "string",
  # integer
  integer = "integer", int = "integer", bigint = "integer", smallint = "integer",
  tinyint = "integer", long = "integer", serial = "integer", int32 = "integer",
  int64 = "integer",
  # number
  number = "number", numeric = "number", float = "number", double = "number",
  decimal = "number", real = "number", money = "number", currency = "number",
  float64 = "number",
  # boolean
  boolean = "boolean", bool = "boolean", logical = "boolean", bit = "boolean",
  # date / datetime
  date = "date", datetime = "datetime", timestamp = "datetime",
  timestamptz = "datetime", time = "datetime", posixct = "datetime",
  "date-time" = "datetime",
  # json
  json = "json", jsonb = "json", object = "json", array = "json", list = "json",
  map = "json", dict = "json", record = "json"
)

.to_spec_property_type <- function(type, where = "") {
  t <- tolower(trimws(as.character(type %||% "")))
  t <- sub("\\(.*\\)$", "", t)
  t <- trimws(t)
  if (length(t) != 1L || !nzchar(t)) return("string")
  if (t %in% .spec_property_types) return(t)
  mapped <- .spec_property_type_map[t]
  if (!is.na(mapped)) return(unname(mapped))
  rlang::warn(
    paste0("Unknown property type '", type, "'",
           if (nzchar(where)) paste0(" for ", where) else "",
           "; using \"string\"."),
    class = "ontologyDiscoverR_unknown_type"
  )
  "string"
}

.to_spec_cardinality <- function(cardinality, where = "") {
  c0 <- tolower(gsub("[_ ]", "-", as.character(cardinality %||% "")))
  valid <- c("one-to-one", "one-to-many", "many-to-one", "many-to-many")
  if (length(c0) == 1L && c0 %in% valid) return(c0)
  rlang::warn(paste0("Unknown cardinality '", cardinality, "' for link type '",
                     where, "'; using \"many-to-many\"."))
  "many-to-many"
}

# Make `x` a valid spec id (^[A-Za-z][A-Za-z0-9_.-]{2,128}$), warning if it had
# to change.
.spec_id <- function(x, what, where) {
  x0 <- as.character(x %||% "")
  if (length(x0) != 1L || is.na(x0)) x0 <- ""
  if (grepl("^[A-Za-z][A-Za-z0-9_.-]{2,128}$", x0)) return(x0)
  y <- gsub("[^A-Za-z0-9_.-]+", "_", trimws(x0))
  if (!grepl("^[A-Za-z]", y)) y <- paste0("x", y)
  while (nchar(y) < 3L) y <- paste0(y, "_")
  y <- substr(y, 1L, 129L)
  rlang::warn(
    paste0("The ", what, " id '", x0, "' (", where, ") is not a valid spec id; ",
           "using '", y, "'."),
    class = "ontologyDiscoverR_renamed_id"
  )
  y
}

# Work out the primary key: the candidate's own key if it has one, else an
# obvious id column, else a surrogate key.
.candidate_primary_key <- function(cand, prop_ids) {
  pk <- cand$primary_key
  pk_name <- if (is.character(pk)) pk[1] else pk$property_id %||% NULL
  if (is.list(pk_name)) pk_name <- unlist(pk_name)[1]
  pk_type <- if (is.list(pk)) pk$type else NULL

  if (!is.null(pk_name) && nzchar(pk_name)) {
    props <- cand$properties %||% list()
    match <- Filter(function(p) identical(as.character(p$id), as.character(pk_name)), props)
    if (length(match) > 0) pk_type <- match[[1]]$type
    return(list(
      id       = .spec_id(pk_name, "primary key", paste0(cand$id, ".", pk_name)),
      column   = pk_name,
      type     = .to_spec_property_type(pk_type %||% "string", paste0(cand$id, ".", pk_name)),
      strategy = "natural"
    ))
  }

  guesses <- c("id", paste0(.snake_case(cand$id), "_id"))
  hit <- intersect(guesses, prop_ids)
  if (length(hit) > 0) {
    props <- cand$properties %||% list()
    match <- Filter(function(p) identical(p$id, hit[1]), props)
    return(list(
      id = hit[1], column = hit[1],
      type = .to_spec_property_type(if (length(match)) match[[1]]$type else "string"),
      strategy = "natural"
    ))
  }

  surrogate <- .spec_id(paste0(.snake_case(cand$id), "_id"), "primary key", cand$id)
  rlang::warn(
    paste0("Object type '", cand$id, "' has no primary key; adding surrogate key '",
           surrogate, "'."),
    class = "ontologyDiscoverR_surrogate_key"
  )
  list(id = surrogate, column = NULL, type = "integer", strategy = "surrogate")
}

# Table name for an object type: an explicit `source_table` field if present,
# else the source reference when it names a single table ("patients" or
# "patients table"), else the snake_case id.
.candidate_source_table <- function(cand) {
  if (is.character(cand$source_table) && nzchar(cand$source_table[1])) {
    return(cand$source_table[1])
  }
  for (ref in cand$source_refs %||% list()) {
    ex <- trimws(as.character(ref$excerpt %||% ""))
    ex <- trimws(sub("(?i)^table\\s+|\\s+table$", "", ex, perl = TRUE))
    if (length(ex) == 1L && grepl("^[A-Za-z_][A-Za-z0-9_.]*$", ex)) return(ex)
  }
  .snake_case(cand$id)
}

.snake_case <- function(x) {
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  tolower(gsub("^_+|_+$", "", x))
}

.nz_or_null <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(x)) NULL else x
}

# Provenance kept on each element under extensions$ontologyDiscoverR.
.provenance_extension <- function(cand) {
  refs <- cand$source_refs %||% list()
  by <- unique(stats::na.omit(c(
    cand$extracted_by %||% NA_character_,
    vapply(refs, function(r) as.character(r$extracted_by %||% NA_character_), character(1))
  )))
  srcs <- unique(vapply(refs, function(r) as.character(r$source_label %||% ""), character(1)))
  srcs <- srcs[nzchar(srcs)]
  list(ontologyDiscoverR = .compact(list(
    candidateId  = cand$candidate_id,
    confidence   = cand$confidence,
    status       = cand$status,
    # Plain character vectors: these serialise the same way before and after
    # a bundle_to_json() / bundle_from_json() round trip.
    extractedBy  = if (length(by)) by else NULL,
    sources      = if (length(srcs)) srcs else NULL
  )))
}

.bundle_extensions <- function(session) {
  label <- session$llm_info$label %||% NA_character_
  list(ontologyDiscoverR = .compact(list(
    sessionId   = session$session_id,
    sessionLabel = session$label,
    extractedBy = if (!is.na(label)) label else NULL,
    generator   = tryCatch(
      paste0("ontologyDiscoverR ", as.character(utils::packageVersion("ontologyDiscoverR"))),
      error = function(e) "ontologyDiscoverR"
    )
  )))
}

.compact <- function(x) x[!vapply(x, is.null, logical(1))]

# ---------------------------------------------------------------------------
# Building, problem tracking and validation
# ---------------------------------------------------------------------------

.new_problem_log <- function() {
  env <- new.env(parent = emptyenv())
  env$items <- character(0)
  env
}

.add_problem <- function(log, element_type, cand, message) {
  log$items <- c(log$items, sprintf(
    "%s '%s' (candidate %s): %s",
    element_type, cand$id %||% "?", cand$candidate_id %||% "?", message
  ))
}

.abort_on_problems <- function(log) {
  if (length(log$items) == 0L) return(invisible(TRUE))
  rlang::abort(
    c("The discovered candidates do not form a valid ontologySpecR bundle.",
      stats::setNames(log$items, rep("x", length(log$items)))),
    class = "ontologyDiscoverR_invalid_bundle",
    call  = NULL
  )
}

# Apply `builder` to each candidate; record constructor errors against the
# candidate. Returns the built elements, named by position in `cands`.
.build_each <- function(cands, element_type, log, builder) {
  if (length(cands) == 0L) return(list())
  if (is.null(names(cands))) names(cands) <- as.character(seq_along(cands))
  out <- list()
  for (nm in names(cands)) {
    cand <- cands[[nm]]
    el <- tryCatch(builder(cand), error = function(e) {
      .add_problem(log, element_type, cand, conditionMessage(e))
      NULL
    })
    if (!is.null(el)) out[[nm]] <- el
  }
  out
}

# Drop candidates whose referenced object types are not in the bundle. Keeps
# the names (positions) so validation errors can be traced back.
.keep_with_targets <- function(cands, element_type, type_map, targets_of) {
  if (length(cands) == 0L) return(list())
  keep <- vapply(cands, function(c) {
    t <- targets_of(c)
    ok <- length(t) > 0L && all(vapply(t, function(x) !is.null(x) && x %in% names(type_map), logical(1)))
    if (!ok) {
      rlang::warn(sprintf(
        "Dropping %s '%s': it refers to object type(s) not in the bundle (%s).",
        element_type, c$id, paste(vapply(t, function(x) x %||% "<none>", character(1)), collapse = ", ")
      ), class = "ontologyDiscoverR_dropped_candidate")
    }
    ok
  }, logical(1))
  cands[keep]
}

.check_duplicate_ids <- function(elements, cands, element_type, log) {
  if (length(elements) < 2L) return(invisible())
  ids <- vapply(elements, function(e) e$id, character(1))
  dup <- duplicated(ids)
  for (nm in names(elements)[dup]) {
    .add_problem(log, element_type, cands[[nm]],
                 paste0("duplicate id '", elements[[nm]]$id, "' in the bundle"))
  }
}

# Run validate_bundle(); on failure, find which candidate caused each message
# by validating each element on its own inside a minimal bundle.
.validate_or_abort <- function(b, cands_by_slot) {
  msgs <- ontologySpecR::validate_bundle(b, error = FALSE)
  if (length(msgs) == 0L) return(invisible(TRUE))

  log <- .new_problem_log()
  element_type <- c(objects = "object_type", links = "link_type",
                    actions = "action_type", concepts = "concept_hint")
  attributed <- FALSE
  for (slot in names(element_type)) {
    els <- b[[slot]] %||% list()
    cands <- cands_by_slot[[slot]]
    for (i in seq_along(els)) {
      args <- list(bundle_id = "probe", bundle_version = "0.0.0")
      args[[slot]] <- list(els[[i]])
      el_msgs <- ontologySpecR::validate_bundle(do.call(ontologySpecR::bundle, args), error = FALSE)
      for (m in el_msgs) {
        .add_problem(log, element_type[[slot]], cands[[i]], m)
        attributed <- TRUE
      }
    }
  }
  # Messages not tied to one element (e.g. an invalid bundle id).
  probe_bundle <- b
  for (slot in names(element_type)) probe_bundle[[slot]] <- list()
  for (m in ontologySpecR::validate_bundle(probe_bundle, error = FALSE)) {
    log$items <- c(log$items, paste0("bundle '", b$bundleId, "': ", m))
    attributed <- TRUE
  }
  if (!attributed) log$items <- c(log$items, msgs)
  .abort_on_problems(log)
}
