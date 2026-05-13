# Helper to mock call_claude() for deterministic, free tests.
# Usage: withr::with_envvar(list(ANTHROPIC_API_KEY="test"), { mock_claude(my_response); ... })
# Or use local_mock_claude() inside a test.

mock_claude <- function(response_list, envir = parent.frame()) {
  mockery::stub(call_claude, "httr2::req_perform",
    function(...) {
      structure(
        list(
          status  = 200L,
          headers = list(`content-type` = "application/json"),
          body    = charToRaw(jsonlite::toJSON(
            list(content = list(list(type = "text",
                                     text = jsonlite::toJSON(response_list, auto_unbox = TRUE)))),
            auto_unbox = TRUE
          ))
        ),
        class = "httr2_response"
      )
    },
    envir = envir
  )
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

# Fixture: merge deduplication response
merge_response <- function() {
  list(
    is_duplicate = TRUE,
    canonical_id = "Patient",
    confidence   = 0.95
  )
}
