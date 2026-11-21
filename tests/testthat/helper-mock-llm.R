# Fake ellmer Chat for deterministic, offline tests. No network, no API key.
#
# fake_chat(responses) returns an object with the parts of the ellmer Chat API
# that ontologyDiscoverR uses: $chat(), $chat_structured(), $clone(),
# $set_system_prompt(), $set_turns(), $get_model() and $get_provider().
#
# `responses` is either
#   - a list of canned answers, used in order (an R list is a structured
#     answer; a character string is raw text returned by $chat()), or
#   - a function(prompt, system_prompt) returning one answer.
#
# With `structured = FALSE`, $chat_structured() errors, as for a provider
# without structured output, so the text fallback path runs.
#
# All clones share one log: fake$log() lists every call as
# list(method, system_prompt, prompt).
fake_chat <- function(responses = list(), structured = TRUE,
                      provider = "FakeProvider", model = "fake-model-1") {
  state <- new.env(parent = emptyenv())
  state$i     <- 0L
  state$calls <- list()

  next_answer <- function(prompt, system_prompt) {
    if (is.function(responses)) return(responses(prompt, system_prompt))
    state$i <- state$i + 1L
    if (state$i > length(responses)) stop("fake_chat: no more canned responses")
    responses[[state$i]]
  }
  record <- function(method, system_prompt, prompt) {
    state$calls[[length(state$calls) + 1L]] <-
      list(method = method, system_prompt = system_prompt, prompt = prompt)
  }

  make <- function(system_prompt = NULL) {
    self <- new.env(parent = emptyenv())
    self$system_prompt <- system_prompt
    self$turns <- list()

    self$chat <- function(..., echo = NULL) {
      prompt <- paste(..., sep = "\n")
      record("chat", self$system_prompt, prompt)
      ans <- next_answer(prompt, self$system_prompt)
      if (is.character(ans)) ans else as.character(jsonlite::toJSON(ans, auto_unbox = TRUE, null = "null"))
    }
    self$chat_structured <- function(..., type, echo = "none", convert = TRUE) {
      prompt <- paste(..., sep = "\n")
      record("chat_structured", self$system_prompt, prompt)
      if (!structured) stop("This provider does not support structured output.")
      ans <- next_answer(prompt, self$system_prompt)
      if (is.character(ans)) jsonlite::fromJSON(ans, simplifyVector = FALSE) else ans
    }
    self$clone <- function(deep = FALSE) {
      cl <- make(self$system_prompt)
      cl$turns <- self$turns
      cl
    }
    self$set_system_prompt <- function(value) {
      self$system_prompt <- value
      invisible(self)
    }
    self$set_turns <- function(value) {
      self$turns <- value
      invisible(self)
    }
    self$get_model    <- function() model
    self$get_provider <- function() structure(list(), name = provider)
    self$log          <- function() state$calls
    class(self) <- "fake_chat"
    self
  }

  make()
}

# Route answers by what the system prompt asks for, so a whole dis_extract()
# run can be driven by one fake.
hospital_responder <- function(prompt, system_prompt) {
  sp <- if (is.null(system_prompt)) "" else system_prompt
  if (grepl("extracting the data model", sp, fixed = TRUE)) return(hospital_object_types_response())
  if (grepl("RELATIONSHIPS", sp, fixed = TRUE))              return(hospital_link_types_response())
  if (grepl("ACTIONS", sp, fixed = TRUE))                    return(hospital_action_types_response())
  if (grepl("BUSINESS RULES", sp, fixed = TRUE))             return(hospital_concept_hints_response())
  stop("hospital_responder: unexpected prompt")
}

# A minimal DiscoverySource without touching the file system.
fake_source <- function(text = "TABLE patients (patient_id int);",
                        type = "sql_ddl", label = "test.sql") {
  structure(list(
    source_id    = "test-src",
    source_type  = type,
    source_label = label,
    raw_text     = text,
    structured   = list(tables = list()),
    metadata     = list()
  ), class = c("DiscoverySource", "list"))
}

# Fixture: object type extraction response for hospital schema
hospital_object_types_response <- function() {
  list(
    object_types = list(
      list(id = "Patient", display_name = "Patient",
           description = "A patient in the hospital system",
           source_ref = "patients table", confidence = 0.95,
           primary_key = list(property_id = "patient_id", type = "integer"),
           properties = list(
             list(id = "patient_id",    type = "integer", nullable = FALSE, description = "PK"),
             list(id = "first_name",    type = "string",  nullable = FALSE, description = ""),
             list(id = "last_name",     type = "string",  nullable = FALSE, description = ""),
             list(id = "date_of_birth", type = "date",    nullable = FALSE, description = ""),
             list(id = "gender",        type = "string",  nullable = TRUE,  description = "")
           )),
      list(id = "Ward", display_name = "Ward",
           description = "A hospital ward",
           source_ref = "wards table", confidence = 0.92,
           primary_key = list(property_id = "ward_id", type = "integer"),
           properties = list(
             list(id = "ward_id",  type = "integer", nullable = FALSE, description = "PK"),
             list(id = "name",     type = "string",  nullable = FALSE, description = ""),
             list(id = "capacity", type = "integer", nullable = FALSE, description = "")
           )),
      list(id = "Doctor", display_name = "Doctor",
           description = "A medical doctor",
           source_ref = "doctors table", confidence = 0.93,
           primary_key = list(property_id = "doctor_id", type = "integer"),
           properties = list(
             list(id = "doctor_id",      type = "integer", nullable = FALSE, description = "PK"),
             list(id = "first_name",     type = "string",  nullable = FALSE, description = ""),
             list(id = "last_name",      type = "string",  nullable = FALSE, description = ""),
             list(id = "specialisation", type = "string",  nullable = TRUE,  description = "")
           ))
    )
  )
}

# Fixture: link type extraction response
hospital_link_types_response <- function() {
  list(
    link_types = list(
      list(id = "patient_admission",
           display_name = "Patient Admission",
           from_type_id = "Patient", to_type_id = "Ward",
           cardinality = "many_to_many", directed = TRUE,
           description = "Patient admitted to ward",
           source_ref = "admissions table", confidence = 0.9),
      list(id = "patient_appointment",
           display_name = "Patient Appointment",
           from_type_id = "Patient", to_type_id = "Doctor",
           cardinality = "many_to_many", directed = TRUE,
           description = "Patient has appointment with doctor",
           source_ref = "appointments table", confidence = 0.88)
    )
  )
}

# Fixture: action types response
hospital_action_types_response <- function() {
  list(
    action_types = list(
      list(id = "admit_patient", display_name = "Admit Patient",
           object_type_id = "Patient", description = "Admit a patient to a ward",
           source_ref = "admissions", confidence = 0.8),
      list(id = "discharge_patient", display_name = "Discharge Patient",
           object_type_id = "Patient", description = "Discharge a patient from ward",
           source_ref = "admissions", confidence = 0.8)
    )
  )
}

# Fixture: concept hints response
hospital_concept_hints_response <- function() {
  list(
    concept_hints = list(
      list(id = "high_risk_patient", display_name = "High Risk Patient",
           object_type_id = "Patient",
           sql_expr = "age > 65 OR previous_admissions > 0",
           description = "Patient considered high risk",
           source_ref = "docs", confidence = 0.75),
      list(id = "ready_for_discharge", display_name = "Ready for Discharge",
           object_type_id = "Patient",
           sql_expr = "length_of_stay > 3 AND diagnosis_complete = TRUE",
           description = "Patient ready to be discharged",
           source_ref = "docs", confidence = 0.7)
    )
  )
}

# Make sure no test picks up a real provider from the developer's setup.
without_configured_provider <- function(code) {
  withr::with_options(list(ontologyDiscoverR.chat = NULL),
    withr::with_envvar(c(ONTOLOGYDISCOVERR_CHAT = NA), code))
}
