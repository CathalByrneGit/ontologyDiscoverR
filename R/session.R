#' Create a new discovery session
#'
#' @param label Optional human-readable label for the session
#' @return A DiscoverySession S3 object
#' @export
discover_session <- function(label = NULL) {
  session <- list(
    session_id  = new_uuid(),
    label       = label %||% paste0("session-", format(Sys.time(), "%Y%m%d-%H%M%S")),
    created_at  = Sys.time(),
    sources     = list(),
    candidates  = list(
      object_types   = list(),
      link_types     = list(),
      action_types   = list(),
      concept_hints  = list()
    ),
    db_connection = NULL,
    extracted     = FALSE
  )
  class(session) <- c("DiscoverySession", "list")
  session
}

#' Add a pre-parsed DiscoverySource to a session
#'
#' @param session A DiscoverySession object
#' @param source A DiscoverySource object (output of any parse_*() function)
#' @return Updated DiscoverySession
#' @export
dis_add_source <- function(session, source) {
  stopifnot(inherits(session, "DiscoverySession"))
  if (!inherits(source, "DiscoverySource")) {
    rlang::abort("'source' must be a DiscoverySource object (output of parse_*() functions)")
  }
  session$sources <- c(session$sources, list(source))
  session
}

#' Add a live database connection as a source
#'
#' @param session A DiscoverySession object
#' @param connection A DBI connection object
#' @param label Optional label for the source
#' @return Updated DiscoverySession
#' @export
dis_add_db <- function(session, connection, label = NULL) {
  stopifnot(inherits(session, "DiscoverySession"))
  source <- parse_db_schema(connection)
  if (!is.null(label)) source$source_label <- label
  session$db_connection <- connection
  dis_add_source(session, source)
}

#' Add a file or URL as a source
#'
#' @param session A DiscoverySession object
#' @param path File path or URL
#' @param type Source type (auto-detected from extension if NULL)
#' @return Updated DiscoverySession
#' @export
dis_add_file <- function(session, path, type = NULL) {
  stopifnot(inherits(session, "DiscoverySession"))

  detected_type <- type %||% .detect_source_type(path)

  source <- switch(detected_type,
    sql_ddl    = parse_sql_ddl(path),
    csv        = parse_csv(path),
    pdf        = parse_pdf(path),
    markdown   = parse_markdown(path),
    html       = parse_html(path),
    openapi    = parse_openapi(path),
    owl_rdf    = parse_owl(path),
    rlang::abort(paste0("Unknown source type '", detected_type,
                        "'. Specify type= explicitly. Supported: sql_ddl, csv, pdf, markdown, html, openapi, owl_rdf"))
  )

  dis_add_source(session, source)
}

.detect_source_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    sql             = "sql_ddl",
    csv             = "csv",
    pdf             = "pdf",
    md              = "markdown",
    markdown        = "markdown",
    html            = "html",
    htm             = "html",
    yaml            = ,
    yml             = "openapi",
    json            = "openapi",
    owl             = ,
    rdf             = "owl_rdf",
    ttl             = "owl_rdf",
    rlang::abort(paste0("Cannot auto-detect source type for extension '.", ext,
                        "'. Use dis_add_file(type=...) to specify."))
  )
}

#' Run the full extraction pipeline on all sources in a session
#'
#' @param session A DiscoverySession object
#' @param verbose Show progress messages (default TRUE)
#' @return Updated DiscoverySession with populated $candidates
#' @export
dis_extract <- function(session, verbose = TRUE) {
  stopifnot(inherits(session, "DiscoverySession"))

  if (length(session$sources) == 0) {
    rlang::abort("No sources added to session. Use dis_add_file() or dis_add_source() first.")
  }

  all_object_types  <- list()
  all_link_types    <- list()
  all_action_types  <- list()
  all_concept_hints <- list()

  n_sources <- length(session$sources)

  for (i in seq_along(session$sources)) {
    source <- session$sources[[i]]
    if (verbose) cli::cli_alert_info("Processing source {i}/{n_sources}: {source$source_label}")

    # Chunk large sources
    chunks <- chunk_source(source)

    for (chunk in chunks) {
      # Pass 1: Object types
      if (verbose && length(chunks) > 1) {
        cli::cli_alert_info("  Pass 1 (object types): {chunk$source_label}")
      }

      new_types <- tryCatch(
        extract_object_types(chunk, existing_types = all_object_types),
        error = function(e) {
          rlang::warn(paste0("Object type extraction failed for '", chunk$source_label, "': ", conditionMessage(e)))
          list()
        }
      )
      all_object_types <- c(all_object_types, new_types)

      # Pass 2: Link types (needs object types from pass 1)
      if (verbose && length(chunks) > 1) {
        cli::cli_alert_info("  Pass 2 (link types): {chunk$source_label}")
      }

      new_links <- tryCatch(
        extract_link_types(chunk, all_object_types),
        error = function(e) {
          rlang::warn(paste0("Link type extraction failed for '", chunk$source_label, "': ", conditionMessage(e)))
          list()
        }
      )
      all_link_types <- c(all_link_types, new_links)

      # Pass 3: Actions and concepts
      if (verbose && length(chunks) > 1) {
        cli::cli_alert_info("  Pass 3 (actions/concepts): {chunk$source_label}")
      }

      new_actions <- tryCatch(
        extract_action_types(chunk, all_object_types),
        error = function(e) {
          rlang::warn(paste0("Action type extraction failed for '", chunk$source_label, "': ", conditionMessage(e)))
          list()
        }
      )
      all_action_types <- c(all_action_types, new_actions)

      new_concepts <- tryCatch(
        extract_concept_hints(chunk, all_object_types),
        error = function(e) {
          rlang::warn(paste0("Concept hint extraction failed for '", chunk$source_label, "': ", conditionMessage(e)))
          list()
        }
      )
      all_concept_hints <- c(all_concept_hints, new_concepts)
    }
  }

  if (verbose) cli::cli_alert_info("Merging and deduplicating candidates...")

  all_candidates <- c(all_object_types, all_link_types, all_action_types, all_concept_hints)
  merged <- merge_candidates(all_candidates)

  session$candidates$object_types   <- Filter(function(c) c$element_type == "object_type",   merged)
  session$candidates$link_types     <- Filter(function(c) c$element_type == "link_type",     merged)
  session$candidates$action_types   <- Filter(function(c) c$element_type == "action_type",   merged)
  session$candidates$concept_hints  <- Filter(function(c) c$element_type == "concept_hint",  merged)

  session$extracted <- TRUE

  if (verbose) {
    dis_summary(session)
  }

  session
}

#' Summarise the candidates discovered in a session
#'
#' @param session A DiscoverySession object
#' @return Invisibly returns the session
#' @export
dis_summary <- function(session) {
  stopifnot(inherits(session, "DiscoverySession"))

  ot <- session$candidates$object_types
  lt <- session$candidates$link_types
  at <- session$candidates$action_types
  ch <- session$candidates$concept_hints

  conf_bucket <- function(candidates) {
    list(
      high   = sum(sapply(candidates, function(c) c$confidence >= 0.8)),
      medium = sum(sapply(candidates, function(c) c$confidence >= 0.5 & c$confidence < 0.8)),
      low    = sum(sapply(candidates, function(c) c$confidence < 0.5))
    )
  }

  ot_conf <- conf_bucket(ot)

  n_conflicts <- sum(sapply(c(ot, lt), function(c) length(c$conflicts) > 0))

  cli::cli_alert_info("Sources processed: {length(session$sources)}")
  cli::cli_alert_info("Object types found: {length(ot)} ({ot_conf$high} high-confidence, {ot_conf$medium} medium, {ot_conf$low} low)")
  cli::cli_alert_info("Link types found: {length(lt)}")
  cli::cli_alert_info("Action types found: {length(at)}")
  cli::cli_alert_info("Concept hints found: {length(ch)}")
  cli::cli_alert_info("Conflicts requiring review: {n_conflicts}")

  invisible(session)
}

#' Export approved candidates as an ontologySpecR-style bundle
#'
#' @param session A DiscoverySession object
#' @param bundle_id Character id for the bundle
#' @param bundle_name Human-readable bundle name
#' @param min_confidence Minimum confidence for auto-including pending candidates (default 0.5)
#' @param include_concepts Whether to include concept hints as concept_defs (default FALSE)
#' @return A list representing an ontology bundle
#' @export
dis_to_bundle <- function(session, bundle_id, bundle_name,
                           min_confidence = 0.5, include_concepts = FALSE) {
  stopifnot(inherits(session, "DiscoverySession"))

  include_candidate <- function(cand) {
    cand$status == "approved" ||
      (cand$status == "pending" && cand$confidence >= min_confidence)
  }

  approved_ot <- Filter(include_candidate, session$candidates$object_types)
  approved_lt <- Filter(include_candidate, session$candidates$link_types)
  approved_at <- Filter(include_candidate, session$candidates$action_types)
  approved_ch <- if (include_concepts) Filter(include_candidate, session$candidates$concept_hints) else list()

  approved_ot_ids <- sapply(approved_ot, function(t) t$id)

  object_types <- lapply(approved_ot, function(ot) {
    props <- lapply(ot$properties %||% list(), function(p) {
      list(
        id          = p$id,
        type        = p$type,
        nullable    = isTRUE(p$nullable),
        description = p$description %||% ""
      )
    })
    list(
      id           = ot$id,
      display_name = ot$display_name,
      description  = ot$description %||% "",
      primary_key  = ot$primary_key,
      properties   = props
    )
  })

  link_types <- lapply(
    Filter(function(lt) lt$from_type_id %in% approved_ot_ids && lt$to_type_id %in% approved_ot_ids, approved_lt),
    function(lt) {
      list(
        id           = lt$id,
        display_name = lt$display_name,
        description  = lt$description %||% "",
        from_type_id = lt$from_type_id,
        to_type_id   = lt$to_type_id,
        cardinality  = lt$cardinality,
        directed     = lt$directed
      )
    }
  )

  action_types <- lapply(approved_at, function(at) {
    list(
      id             = at$id,
      display_name   = at$display_name,
      description    = at$description %||% "",
      object_type_id = at$object_type_id
    )
  })

  concept_defs <- lapply(approved_ch, function(ch) {
    list(
      id             = ch$id,
      display_name   = ch$display_name,
      description    = ch$description %||% "",
      object_type_id = ch$object_type_id,
      sql_expr       = ch$sql_expr %||% ""
    )
  })

  bundle <- list(
    bundle_id    = bundle_id,
    bundle_name  = bundle_name,
    version      = "0.1.0",
    created_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    object_types = object_types,
    link_types   = link_types,
    action_types = action_types,
    concept_defs = concept_defs
  )

  class(bundle) <- c("ontology_bundle", "list")
  bundle
}

#' Print method for DiscoverySession
#' @export
print.DiscoverySession <- function(x, ...) {
  cat(sprintf(
    "<DiscoverySession> '%s'\n  Sources: %d | Extracted: %s\n  Object types: %d | Link types: %d | Actions: %d | Concepts: %d\n",
    x$label,
    length(x$sources),
    if (x$extracted) "yes" else "no",
    length(x$candidates$object_types),
    length(x$candidates$link_types),
    length(x$candidates$action_types),
    length(x$candidates$concept_hints)
  ))
  invisible(x)
}
