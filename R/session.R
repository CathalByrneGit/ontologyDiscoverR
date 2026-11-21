#' Create a new discovery session
#'
#' @param label Optional human-readable label for the session
#' @param llm The LLM provider: an ellmer Chat object (e.g.
#'   `ellmer::chat_openai(model = "<model>")`), a function `f(system_prompt)`
#'   returning one, a `"provider/model"` string for `ellmer::chat()`, or `NULL`
#'   to use `getOption("ontologyDiscoverR.chat")` or the
#'   `ONTOLOGYDISCOVERR_CHAT` environment variable. There is no built-in
#'   default provider; see [call_llm()].
#' @return A DiscoverySession S3 object
#' @export
discover_session <- function(label = NULL, llm = NULL) {
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
    llm_fn        = llm,
    llm_info      = .llm_info(llm),
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
#' @param chunk_tokens Approximate maximum tokens per LLM request. Large
#'   sources are split into chunks of this size. Smaller values (e.g. 2000-4000)
#'   help small local models. Defaults to `getOption("ontologyDiscoverR.chunk_tokens", 8000)`.
#' @return Updated DiscoverySession with populated $candidates
#' @export
dis_extract <- function(session, verbose = TRUE,
                        chunk_tokens = getOption("ontologyDiscoverR.chunk_tokens", 8000L)) {
  stopifnot(inherits(session, "DiscoverySession"))

  if (length(session$sources) == 0) {
    rlang::abort("No sources added to session. Use dis_add_file() or dis_add_source() first.")
  }

  # Fail before the per-source error handling below can swallow it.
  .check_provider(session$llm_fn)
  session$llm_info <- .llm_info(session$llm_fn)
  llm          <- session$llm_fn
  extracted_by <- session$llm_info$label

  all_object_types  <- list()
  all_link_types    <- list()
  all_action_types  <- list()
  all_concept_hints <- list()

  n_sources <- length(session$sources)

  for (i in seq_along(session$sources)) {
    source <- session$sources[[i]]
    if (verbose) cli::cli_alert_info("Processing source {i}/{n_sources}: {source$source_label}")

    # Chunk large sources
    chunks <- chunk_source(source, max_tokens = chunk_tokens)

    for (chunk in chunks) {
      # Pass 1: Object types
      if (verbose && length(chunks) > 1) {
        cli::cli_alert_info("  Pass 1 (object types): {chunk$source_label}")
      }

      new_types <- tryCatch(
        extract_object_types(chunk, existing_types = all_object_types,
                             llm = llm, extracted_by = extracted_by),
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
        extract_link_types(chunk, all_object_types, llm = llm, extracted_by = extracted_by),
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
        extract_action_types(chunk, all_object_types, llm = llm, extracted_by = extracted_by),
        error = function(e) {
          rlang::warn(paste0("Action type extraction failed for '", chunk$source_label, "': ", conditionMessage(e)))
          list()
        }
      )
      all_action_types <- c(all_action_types, new_actions)

      new_concepts <- tryCatch(
        extract_concept_hints(chunk, all_object_types, llm = llm, extracted_by = extracted_by),
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

#' Print method for DiscoverySession
#' @param x The object to print.
#' @param ... Ignored.
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
  label <- x$llm_info$label %||% NA_character_
  cat("  LLM: ", if (is.na(label)) "(not configured)" else label, "\n", sep = "")
  invisible(x)
}
