approved <- function(cand) {
  cand$status <- "approved"
  cand
}

pk_prop <- function(id) list(id = id, type = "integer", nullable = FALSE, description = "")

hospital_session <- function() {
  sess <- discover_session("full-test", llm = fake_chat(list()))
  sess$candidates$object_types <- list(
    approved(new_candidate_object_type("Patient", "Patient", confidence = 0.95,
      primary_key = list(property_id = "patient_id", type = "integer"),
      properties = list(pk_prop("patient_id"),
                        list(id = "age", type = "int", nullable = TRUE, description = "Age")),
      source_refs = list(new_source_ref("s1", "schema.sql", "patients table",
                                        extracted_by = "fakeprovider/fake-model-1")),
      extracted_by = "fakeprovider/fake-model-1")),
    approved(new_candidate_object_type("Doctor", "Doctor", confidence = 0.92,
      primary_key = list(property_id = "doctor_id", type = "integer"),
      properties = list(pk_prop("doctor_id")))),
    approved(new_candidate_object_type("Ward", "Ward", confidence = 0.90,
      primary_key = list(property_id = "ward_id", type = "integer"),
      properties = list(pk_prop("ward_id"))))
  )
  sess$candidates$link_types <- list(
    approved(new_candidate_link_type("patient_doctor", "Patient Doctor", "Patient", "Doctor",
                                     cardinality = "many_to_many")),
    approved(new_candidate_link_type("patient_ward", "Patient Ward", "Patient", "Ward",
                                     cardinality = "one_to_many"))
  )
  sess$candidates$action_types <- list(
    approved(new_candidate_action_type("admit_patient", "Admit Patient", "Patient"))
  )
  sess$candidates$concept_hints <- list(
    approved(new_candidate_concept_hint("high_risk", "High Risk", "Patient",
                                        sql_expr = "age > 65", confidence = 0.8))
  )
  sess
}

test_that("dis_to_bundle builds a canonical ontologySpecR bundle", {
  bundle <- dis_to_bundle(hospital_session(), "test-bundle", "Test Bundle")

  expect_s3_class(bundle, "ontology_bundle")
  expect_true(ontologySpecR::is_bundle(bundle))
  expect_true(ontologySpecR::validate_bundle(bundle))

  expect_equal(ontologySpecR::bundle_id(bundle), "test-bundle")
  expect_equal(ontologySpecR::bundle_version(bundle), "0.1.0")
  expect_equal(bundle$metadata$name, "Test Bundle")

  objs <- ontologySpecR::bundle_object_types(bundle)
  expect_setequal(names(objs), c("Patient", "Doctor", "Ward"))
  for (ot in objs) expect_true(ontologySpecR::is_object_type(ot))

  patient <- objs$Patient
  expect_equal(ontologySpecR::object_pk(patient), "patient_id")
  expect_equal(ontologySpecR::object_table(patient), "patients")
  expect_equal(patient$source$kind, "table")
  expect_equal(ontologySpecR::object_property(patient, "age")$type, "integer")
  expect_false(ontologySpecR::object_property(patient, "patient_id")$nullable)
  expect_equal(patient$extensions$ontologyDiscoverR$extractedBy,
               "fakeprovider/fake-model-1")

  links <- ontologySpecR::bundle_link_types(bundle)
  expect_setequal(names(links), c("patient_doctor", "patient_ward"))
  expect_equal(links$patient_doctor$cardinality, "many-to-many")
  expect_equal(links$patient_ward$cardinality,   "one-to-many")
  expect_equal(ontologySpecR::link_endpoints(links$patient_ward),
               list(from = "Patient", to = "Ward"))

  actions <- ontologySpecR::bundle_action_types(bundle)
  expect_equal(unlist(actions$admit_patient$targets), "Patient")

  # Concepts are opt-in.
  expect_length(ontologySpecR::bundle_concepts(bundle), 0L)
})

test_that("dis_to_bundle round-trips through bundle_to_json() / bundle_from_json()", {
  bundle <- dis_to_bundle(hospital_session(), "test-bundle", "Test Bundle",
                          include_concepts = TRUE)
  json  <- ontologySpecR::bundle_to_json(bundle)
  again <- ontologySpecR::bundle_from_json(json)

  expect_s3_class(again, "ontology_bundle")
  expect_true(ontologySpecR::validate_bundle(again))
  expect_identical(as.character(ontologySpecR::bundle_to_json(again)), as.character(json))
})

test_that("dis_to_bundle includes concept definitions when include_concepts = TRUE", {
  sess <- hospital_session()
  bundle_no  <- dis_to_bundle(sess, "test", "Test", include_concepts = FALSE)
  bundle_yes <- dis_to_bundle(sess, "test", "Test", include_concepts = TRUE)

  expect_length(ontologySpecR::bundle_concepts(bundle_no), 0L)
  concepts <- ontologySpecR::bundle_concepts(bundle_yes)
  expect_length(concepts, 1L)
  expect_true(ontologySpecR::is_concept_def(concepts$high_risk))
  expect_equal(concepts$high_risk$sqlExpr, "age > 65")
  expect_equal(concepts$high_risk$objectTypeId, "Patient")
  expect_equal(concepts$high_risk$status, "draft")
})

test_that("dis_to_bundle on a mocked dis_extract() session passes validate_bundle()", {
  skip_if_not_installed("mockery")
  fake_ot <- function(...) {
    lapply(hospital_object_types_response()$object_types, function(ot) {
      new_candidate_object_type(ot$id, ot$display_name, confidence = ot$confidence,
        primary_key = ot$primary_key,
        properties = ot$properties,
        source_refs = list(new_source_ref("s", "schema.sql", ot$source_ref)))
    })
  }
  fake_lt <- function(...) {
    lapply(hospital_link_types_response()$link_types, function(lt) {
      new_candidate_link_type(lt$id, lt$display_name, lt$from_type_id, lt$to_type_id,
                              cardinality = lt$cardinality, confidence = lt$confidence)
    })
  }
  mockery::stub(dis_extract, "extract_object_types",  fake_ot)
  mockery::stub(dis_extract, "extract_link_types",    fake_lt)
  mockery::stub(dis_extract, "extract_action_types",  function(...) list())
  mockery::stub(dis_extract, "extract_concept_hints", function(...) list())

  sess <- dis_add_source(discover_session("mocked", llm = fake_chat(list())), fake_source())
  sess <- dis_extract(sess, verbose = FALSE)
  bundle <- dis_to_bundle(sess, "hospital-mocked", "Hospital")

  expect_true(ontologySpecR::validate_bundle(bundle))
  expect_length(bundle$objects, 3L)
  expect_length(bundle$links, 2L)
  again <- ontologySpecR::bundle_from_json(ontologySpecR::bundle_to_json(bundle))
  expect_identical(as.character(ontologySpecR::bundle_to_json(again)),
                   as.character(ontologySpecR::bundle_to_json(bundle)))
})

test_that("a full fake-Chat extraction produces a valid bundle", {
  fixture_path <- system.file("fixtures", "example-schema.sql", package = "ontologyDiscoverR")
  skip_if(!nzchar(fixture_path), "Fixture file not found")

  sess <- discover_session("e2e", llm = fake_chat(hospital_responder))
  sess <- dis_extract(dis_add_file(sess, fixture_path), verbose = FALSE)
  bundle <- dis_to_bundle(sess, "hospital-e2e", "Hospital", include_concepts = TRUE)

  expect_true(ontologySpecR::validate_bundle(bundle))
  expect_length(ontologySpecR::bundle_concepts(bundle), 2L)
  expect_equal(bundle$extensions$ontologyDiscoverR$extractedBy,
               "fakeprovider/fake-model-1")
})

test_that("unknown property types become 'string' with a warning", {
  expect_equal(.to_spec_property_type("timestamp"), "datetime")
  expect_equal(.to_spec_property_type("object"),    "json")
  expect_equal(.to_spec_property_type("array"),     "json")
  expect_equal(.to_spec_property_type("VARCHAR(20)"), "string")
  expect_warning(t <- .to_spec_property_type("geometry"),
                 class = "ontologyDiscoverR_unknown_type")
  expect_equal(t, "string")
})

test_that("an object type without a primary key gets a surrogate key", {
  sess <- discover_session("pk", llm = fake_chat(list()))
  sess$candidates$object_types <- list(approved(
    new_candidate_object_type("Hospital", "Hospital",
      properties = list(list(id = "name", type = "string", nullable = TRUE)))
  ))
  expect_warning(b <- dis_to_bundle(sess, "pk-test", "PK"),
                 class = "ontologyDiscoverR_surrogate_key")
  ot <- ontologySpecR::bundle_object_type(b, "Hospital")
  expect_equal(ontologySpecR::object_pk(ot), "hospital_id")
  expect_equal(ot$primaryKey$strategy, "surrogate")
  expect_true(ontologySpecR::validate_bundle(b))
})

test_that("short or invalid ids are made spec-valid and keep their source column", {
  sess <- discover_session("ids", llm = fake_chat(list()))
  sess$candidates$object_types <- list(approved(
    new_candidate_object_type("Thing", "Thing",
      primary_key = list(property_id = "id", type = "integer"),
      properties = list(list(id = "id", type = "integer", nullable = FALSE),
                        list(id = "full name", type = "string")))
  ))
  warnings <- capture_warnings(b <- dis_to_bundle(sess, "ids-test", "IDs"))
  expect_length(warnings, 3L)   # id (property), id (primary key), "full name"
  expect_true(all(grepl("not a valid spec id", warnings)))
  ot <- ontologySpecR::bundle_object_type(b, "Thing")
  expect_equal(ontologySpecR::object_pk(ot), "id_")
  expect_equal(ontologySpecR::property_column(ontologySpecR::object_property(ot, "id_")), "id")
  expect_true("full_name" %in% ontologySpecR::object_property_ids(ot))
  expect_true(ontologySpecR::validate_bundle(b))
})

test_that("validation failures abort and name the candidate that caused them", {
  sess <- hospital_session()
  sess$candidates$object_types[[1]]$candidate_id <- "cand-123"
  # An invalid bundle id is reported against the bundle.
  expect_error(dis_to_bundle(sess, "x", "Bad"), class = "ontologyDiscoverR_invalid_bundle")
  err <- tryCatch(dis_to_bundle(sess, "x", "Bad"), error = identity)
  expect_match(conditionMessage(err), "bundle 'x'")

  # Constructor errors are reported against the candidate.
  sess$candidates$concept_hints[[1]]$sql_expr <- ""
  sess$candidates$concept_hints[[1]]$candidate_id <- "cand-concept"
  err <- tryCatch(dis_to_bundle(sess, "ok-bundle", "Bad", include_concepts = TRUE),
                  error = identity)
  expect_s3_class(err, "ontologyDiscoverR_invalid_bundle")
  expect_match(conditionMessage(err), "concept_hint 'high_risk' \\(candidate cand-concept\\)")
})
