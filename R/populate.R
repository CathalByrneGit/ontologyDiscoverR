#' Create a new population session
#'
#' A population session extracts specific entity instances from a document corpus,
#' constrained by the types and properties defined in a schema bundle.
#'
#' @param schema An ontology_bundle (output of dis_to_bundle()) defining the entity types
#' @param llm A one-argument function f(system_prompt) returning an ellmer Chat object.
#'   Defaults to Anthropic Claude. Use Ollama for sensitive/private corpora:
#'   `function(sp) ellmer::chat_ollama(model = "llama3.3:70b", system_prompt = sp)`
#' @param label Optional label for the session
#' @return A PopulateSession S3 object
#' @export
populate_session <- function(schema, llm = NULL, label = NULL) {
  if (!inherits(schema, "ontology_bundle")) {
    rlang::abort("'schema' must be an ontology_bundle (output of dis_to_bundle())")
  }
  if (length(schema$object_types) == 0) {
    rlang::abort("Schema has no object types. Run dis_extract() and dis_to_bundle() first.")
  }

  session <- list(
    session_id  = new_uuid(),
    label       = label %||% paste0("populate-", format(Sys.time(), "%Y%m%d-%H%M%S")),
    created_at  = Sys.time(),
    schema      = schema,
    llm_fn      = llm,
    sources     = list(),
    instances   = list(
      entities       = list(),
      relationships  = list()
    ),
    amendments  = list(),
    populated   = FALSE
  )
  class(session) <- c("PopulateSession", "list")
  session
}

#' Add a file to a population session
#'
#' @param pop_sess A PopulateSession object
#' @param path File path (PDF, Markdown, HTML, CSV, SQL DDL)
#' @param type Source type (auto-detected from extension if NULL)
#' @return Updated PopulateSession
#' @export
pop_add_file <- function(pop_sess, path, type = NULL) {
  stopifnot(inherits(pop_sess, "PopulateSession"))
  detected_type <- type %||% .detect_source_type(path)
  source <- switch(detected_type,
    sql_ddl    = parse_sql_ddl(path),
    csv        = parse_csv(path),
    pdf        = parse_pdf(path),
    markdown   = parse_markdown(path),
    html       = parse_html(path),
    openapi    = parse_openapi(path),
    rlang::abort(paste0("Unsupported source type for population: ", detected_type))
  )
  pop_sess$sources <- c(pop_sess$sources, list(source))
  pop_sess
}

#' Add all supported files in a directory to a population session
#'
#' @param pop_sess A PopulateSession object
#' @param dir Path to directory
#' @param recursive Recurse into subdirectories (default TRUE)
#' @param pattern Optional regex pattern to filter file names
#' @return Updated PopulateSession
#' @export
pop_add_corpus <- function(pop_sess, dir, recursive = TRUE, pattern = NULL) {
  stopifnot(inherits(pop_sess, "PopulateSession"))
  if (!dir.exists(dir)) rlang::abort(paste0("Directory not found: ", dir))

  supported_exts <- c("pdf", "md", "markdown", "html", "htm", "csv", "sql", "yaml", "yml", "json")
  ext_pattern    <- paste0("\\.(", paste(supported_exts, collapse = "|"), ")$")

  files <- list.files(dir, full.names = TRUE, recursive = recursive,
                      pattern = ext_pattern, ignore.case = TRUE)

  if (!is.null(pattern)) {
    files <- files[grepl(pattern, basename(files))]
  }

  if (length(files) == 0) {
    rlang::warn(paste0("No supported files found in: ", dir))
    return(pop_sess)
  }

  cli::cli_alert_info("Adding {length(files)} file(s) from {dir}")

  for (f in files) {
    pop_sess <- tryCatch(
      pop_add_file(pop_sess, f),
      error = function(e) {
        rlang::warn(paste0("Skipping '", basename(f), "': ", conditionMessage(e)))
        pop_sess
      }
    )
  }
  pop_sess
}

#' Run instance extraction across all sources in a population session
#'
#' For each source, the LLM is given the full schema and asked to extract
#' every instance of every object type and every relationship between instances.
#' Extracted instances are validated against the schema; unknown properties are
#' flagged as SchemaAmendments.
#'
#' @param pop_sess A PopulateSession object
#' @param verbose Show progress messages (default TRUE)
#' @return Updated PopulateSession with populated $instances
#' @export
pop_extract <- function(pop_sess, verbose = TRUE) {
  stopifnot(inherits(pop_sess, "PopulateSession"))

  if (length(pop_sess$sources) == 0) {
    rlang::abort("No sources added. Use pop_add_file() or pop_add_corpus() first.")
  }

  schema       <- pop_sess$schema
  all_entities <- list()
  all_rels     <- list()
  all_amends   <- list()
  n            <- length(pop_sess$sources)

  for (i in seq_along(pop_sess$sources)) {
    source <- pop_sess$sources[[i]]
    if (verbose) cli::cli_alert_info("Extracting instances from source {i}/{n}: {source$source_label}")

    chunks <- chunk_source(source)

    for (chunk in chunks) {
      extracted <- tryCatch(
        .extract_instances(chunk, schema, pop_sess$llm_fn),
        error = function(e) {
          rlang::warn(paste0("Instance extraction failed for '", chunk$source_label,
                             "': ", conditionMessage(e)))
          list(entities = list(), relationships = list())
        }
      )

      # Build EntityInstances, validate against schema
      new_entities <- lapply(extracted$entities %||% list(), function(e) {
        inst <- new_entity_instance(
          type_id     = e$type_id %||% "Unknown",
          properties  = e$properties %||% list(),
          source_refs = list(new_source_ref(chunk$source_id, chunk$source_label,
                                            e$excerpt %||% "")),
          confidence  = as.numeric(e$confidence %||% 0.8),
          schema      = schema
        )
        inst
      })

      # Collect amendments from entity validation
      for (inst in new_entities) {
        all_amends <- c(all_amends, inst$amendments)
        inst$amendments <- list()  # don't duplicate in session
      }

      # Map temporary extraction ids to real instance_ids
      id_map <- stats::setNames(
        sapply(new_entities, function(e) e$instance_id),
        sapply(extracted$entities %||% list(), function(e) e$temp_id %||% e$instance_id %||% "")
      )

      new_rels <- lapply(extracted$relationships %||% list(), function(r) {
        from_id <- id_map[r$from_temp_id %||% ""] %||% r$from_temp_id %||% ""
        to_id   <- id_map[r$to_temp_id   %||% ""] %||% r$to_temp_id   %||% ""

        new_relationship_instance(
          link_type_id     = r$link_type_id %||% "unknown",
          from_instance_id = from_id,
          to_instance_id   = to_id,
          evidence         = r$evidence %||% "",
          source_refs      = list(new_source_ref(chunk$source_id, chunk$source_label, r$evidence %||% "")),
          confidence       = as.numeric(r$confidence %||% 0.8),
          schema           = schema
        )
      })

      all_entities <- c(all_entities, new_entities)
      all_rels     <- c(all_rels,     new_rels)
    }
  }

  pop_sess$instances$entities      <- all_entities
  pop_sess$instances$relationships <- all_rels
  pop_sess$amendments              <- all_amends
  pop_sess$populated               <- TRUE

  if (verbose) {
    n_ent   <- length(all_entities)
    n_rel   <- length(all_rels)
    n_amend <- length(all_amends)
    cli::cli_alert_success("Extracted {n_ent} entities and {n_rel} relationships")
    if (n_amend > 0) {
      cli::cli_alert_warning("{n_amend} schema amendment(s) flagged — review with pop_amendments()")
    }
  }

  pop_sess
}

#' Export instances as a named list of data frames
#'
#' Returns one data frame per object type, plus a provenance table.
#' Each data frame has an `instance_id` column plus one column per property.
#'
#' @param pop_sess A PopulateSession object
#' @param min_confidence Minimum confidence to include (default 0.0 = all)
#' @param status_filter Character vector of statuses to include (default all non-rejected)
#' @return A named list with:
#'   - `$instances`: named list of data frames, one per object type
#'   - `$relationships`: data frame with link_type_id, from_instance_id, to_instance_id, evidence, confidence
#'   - `$provenance`: data frame with instance_id, source_label, excerpt, confidence
#' @export
pop_to_records <- function(pop_sess, min_confidence = 0.0,
                            status_filter = c("pending", "approved")) {
  stopifnot(inherits(pop_sess, "PopulateSession"))

  entities <- Filter(function(e) {
    e$confidence >= min_confidence && e$status %in% status_filter
  }, pop_sess$instances$entities)

  rels <- Filter(function(r) {
    r$confidence >= min_confidence && r$status %in% status_filter
  }, pop_sess$instances$relationships)

  # Group entities by type
  type_ids <- unique(sapply(entities, function(e) e$type_id))

  instance_dfs <- lapply(stats::setNames(type_ids, type_ids), function(tid) {
    type_entities <- Filter(function(e) e$type_id == tid, entities)
    if (length(type_entities) == 0) return(data.frame())

    # Get all property names for this type
    all_prop_names <- unique(unlist(lapply(type_entities, function(e) names(e$properties))))

    df <- do.call(rbind, lapply(type_entities, function(e) {
      row <- c(
        list(instance_id = e$instance_id, confidence = e$confidence, status = e$status),
        lapply(stats::setNames(all_prop_names, all_prop_names), function(p) {
          val <- e$properties[[p]]
          if (is.null(val)) NA_character_ else as.character(val)
        })
      )
      as.data.frame(row, stringsAsFactors = FALSE)
    }))
    df
  })

  # Relationships data frame
  rel_df <- if (length(rels) == 0) {
    data.frame(link_type_id     = character(),
               from_instance_id = character(),
               to_instance_id   = character(),
               evidence         = character(),
               confidence       = numeric(),
               status           = character(),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, lapply(rels, function(r) {
      data.frame(
        link_type_id     = r$link_type_id,
        from_instance_id = r$from_instance_id,
        to_instance_id   = r$to_instance_id,
        evidence         = r$evidence %||% "",
        confidence       = r$confidence,
        status           = r$status,
        stringsAsFactors = FALSE
      )
    }))
  }

  # Provenance table
  prov_df <- do.call(rbind, c(
    lapply(entities, function(e) {
      src <- if (length(e$source_refs) > 0) e$source_refs[[1]] else list(source_label = "", excerpt = "")
      data.frame(instance_id  = e$instance_id,
                 type_id      = e$type_id,
                 source_label = src$source_label %||% "",
                 excerpt      = src$excerpt      %||% "",
                 confidence   = e$confidence,
                 stringsAsFactors = FALSE)
    }),
    list(data.frame(instance_id  = character(), type_id = character(),
                    source_label = character(), excerpt = character(),
                    confidence   = numeric(),   stringsAsFactors = FALSE))
  ))

  list(
    instances     = instance_dfs,
    relationships = rel_df,
    provenance    = prov_df
  )
}

#' Export relationship instances as a clean edge list
#'
#' Produces a data frame ready for use with graph packages.
#' Columns: from, to, type, weight (= confidence), evidence, from_label, to_label.
#'
#' @param pop_sess A PopulateSession object
#' @param min_confidence Minimum confidence to include (default 0.0)
#' @param label_property Property name to use as node label (default "name")
#' @return A data frame with columns: from, to, type, weight, evidence, from_label, to_label
#' @export
pop_to_edgelist <- function(pop_sess, min_confidence = 0.0, label_property = "name") {
  stopifnot(inherits(pop_sess, "PopulateSession"))

  rels <- Filter(function(r) r$confidence >= min_confidence,
                 pop_sess$instances$relationships)

  if (length(rels) == 0) {
    return(data.frame(from = character(), to = character(), type = character(),
                      weight = numeric(), evidence = character(),
                      from_label = character(), to_label = character(),
                      stringsAsFactors = FALSE))
  }

  # Build id -> label lookup
  entity_lookup <- stats::setNames(
    lapply(pop_sess$instances$entities, function(e) {
      e$properties[[label_property]] %||% e$instance_id
    }),
    sapply(pop_sess$instances$entities, function(e) e$instance_id)
  )

  do.call(rbind, lapply(rels, function(r) {
    data.frame(
      from       = r$from_instance_id,
      to         = r$to_instance_id,
      type       = r$link_type_id,
      weight     = r$confidence,
      evidence   = r$evidence %||% "",
      from_label = as.character(entity_lookup[[r$from_instance_id]] %||% r$from_instance_id),
      to_label   = as.character(entity_lookup[[r$to_instance_id]]   %||% r$to_instance_id),
      stringsAsFactors = FALSE
    )
  }))
}

#' List schema amendments flagged during population
#'
#' @param pop_sess A PopulateSession object
#' @return A data frame of pending schema amendments
#' @export
pop_amendments <- function(pop_sess) {
  stopifnot(inherits(pop_sess, "PopulateSession"))
  amends <- pop_sess$amendments

  if (length(amends) == 0) {
    cli::cli_alert_info("No schema amendments flagged.")
    return(invisible(data.frame()))
  }

  do.call(rbind, lapply(amends, function(a) {
    data.frame(
      amendment_id   = a$amendment_id,
      amendment_type = a$amendment_type,
      type_id        = a$type_id,
      property_id    = a$property_id %||% NA_character_,
      inferred_type  = a$inferred_type %||% NA_character_,
      evidence       = substr(a$evidence %||% "", 1, 80),
      status         = a$status,
      stringsAsFactors = FALSE
    )
  }))
}

#' Save a population session to disk
#'
#' Useful for large corpora where extraction takes a long time.
#'
#' @param pop_sess A PopulateSession object
#' @param path File path to save to (use .rds extension)
#' @export
pop_save <- function(pop_sess, path) {
  stopifnot(inherits(pop_sess, "PopulateSession"))
  saveRDS(pop_sess, path)
  cli::cli_alert_success("Session saved to {path}")
  invisible(pop_sess)
}

#' Load a population session from disk
#'
#' @param path File path to load from
#' @return A PopulateSession object
#' @export
pop_load <- function(path) {
  if (!file.exists(path)) rlang::abort(paste0("File not found: ", path))
  sess <- readRDS(path)
  if (!inherits(sess, "PopulateSession")) {
    rlang::abort("File does not contain a PopulateSession object.")
  }
  sess
}

#' Print method for PopulateSession
#' @export
print.PopulateSession <- function(x, ...) {
  cat(sprintf(
    "<PopulateSession> '%s'\n  Schema: %d types, %d link types\n  Sources: %d | Populated: %s\n  Entities: %d | Relationships: %d | Amendments: %d\n",
    x$label,
    length(x$schema$object_types),
    length(x$schema$link_types),
    length(x$sources),
    if (x$populated) "yes" else "no",
    length(x$instances$entities),
    length(x$instances$relationships),
    length(x$amendments)
  ))
  invisible(x)
}

# Internal: call the LLM to extract instances from a single source chunk
.extract_instances <- function(source, schema, llm_fn = NULL) {
  type_schema <- paste(
    sapply(schema$object_types, function(ot) {
      props <- paste(sapply(ot$properties %||% list(), function(p) {
        sprintf("    %s: %s%s", p$id, p$type, if (isTRUE(p$nullable)) "" else " (required)")
      }), collapse = "\n")
      sprintf("  %s:\n%s", ot$id, if (nzchar(props)) props else "    (no properties defined)")
    }),
    collapse = "\n"
  )

  link_schema <- paste(
    sapply(schema$link_types %||% list(), function(lt) {
      sprintf("  %s: %s -> %s (%s)", lt$id, lt$from_type_id, lt$to_type_id, lt$cardinality)
    }),
    collapse = "\n"
  )

  system_prompt <- paste0(
    "You are extracting specific named INSTANCES (individual records) from a document.\n\n",
    "Extract every occurrence of the following entity types. Be thorough — extract ALL instances mentioned.\n\n",
    "ENTITY TYPES AND THEIR PROPERTIES:\n", type_schema, "\n\n",
    "RELATIONSHIP TYPES:\n", link_schema, "\n\n",
    "For each entity instance:\n",
    "- Assign a short unique temp_id (e.g. 'p1', 'c1') for use in relationships.\n",
    "- Set type_id to exactly one of the entity type ids above.\n",
    "- Populate as many properties as the document supports. Use null for unknown values.\n",
    "- Include a short excerpt (the text that mentions this entity).\n",
    "- Give a confidence score 0.0-1.0.\n\n",
    "For each relationship instance:\n",
    "- Set link_type_id to exactly one of the relationship type ids above.\n",
    "- Reference entities by their temp_id (from_temp_id, to_temp_id).\n",
    "- Include the exact text excerpt as evidence.\n",
    "- Give a confidence score.\n\n",
    "If you see a property that is NOT in the schema but appears consistently, include it anyway — it will be flagged for schema review.\n\n",
    "Respond ONLY with valid JSON. No preamble, no markdown fences.\n",
    "Schema: {",
    "\"entities\": [{\"temp_id\": \"string\", \"type_id\": \"string\", ",
    "\"properties\": {}, \"excerpt\": \"string\", \"confidence\": 0.0}], ",
    "\"relationships\": [{\"link_type_id\": \"string\", \"from_temp_id\": \"string\", ",
    "\"to_temp_id\": \"string\", \"evidence\": \"string\", \"confidence\": 0.0}]}"
  )

  user_prompt <- paste0(
    "Source: ", source$source_label, "\n\n",
    source$raw_text
  )

  call_llm(llm_fn, system_prompt, user_prompt, response_format = "json")
}
