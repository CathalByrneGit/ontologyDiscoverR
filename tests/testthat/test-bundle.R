test_that("dis_to_bundle with 3 object types and 2 link types produces a complete bundle", {
  make_ot <- function(id, ...) {
    ot <- new_candidate_object_type(id, id, ...)
    ot$status <- "approved"
    ot
  }
  make_lt <- function(id, from, to) {
    lt <- new_candidate_link_type(id, id, from_type_id = from, to_type_id = to)
    lt$status <- "approved"
    lt
  }

  ot1 <- make_ot("Patient", confidence = 0.95,
    properties = list(list(id = "patient_id", type = "integer", nullable = FALSE, description = "")))
  ot2 <- make_ot("Doctor",  confidence = 0.92,
    properties = list(list(id = "doctor_id",  type = "integer", nullable = FALSE, description = "")))
  ot3 <- make_ot("Ward",    confidence = 0.90,
    properties = list(list(id = "ward_id",    type = "integer", nullable = FALSE, description = "")))
  lt1 <- make_lt("patient_doctor", "Patient", "Doctor")
  lt2 <- make_lt("patient_ward",   "Patient", "Ward")

  sess <- discover_session("full-test")
  sess$candidates$object_types <- list(ot1, ot2, ot3)
  sess$candidates$link_types   <- list(lt1, lt2)

  bundle <- dis_to_bundle(sess, "test-bundle", "Test Bundle")

  expect_s3_class(bundle, "ontology_bundle")
  expect_equal(length(bundle$object_types), 3L)
  expect_equal(length(bundle$link_types),   2L)

  # Check bundle structure
  expect_true(!is.null(bundle$bundle_id))
  expect_true(!is.null(bundle$bundle_name))
  expect_true(!is.null(bundle$version))
  expect_true(!is.null(bundle$created_at))

  # Check object type structure
  ot_ids <- sapply(bundle$object_types, function(x) x$id)
  expect_true(all(c("Patient", "Doctor", "Ward") %in% ot_ids))

  for (ot in bundle$object_types) {
    expect_true(!is.null(ot$id))
    expect_true(!is.null(ot$display_name))
    expect_true(is.list(ot$properties))
  }

  # Check link type structure
  lt_ids <- sapply(bundle$link_types, function(x) x$id)
  expect_true(all(c("patient_doctor", "patient_ward") %in% lt_ids))

  for (lt in bundle$link_types) {
    expect_true(!is.null(lt$id))
    expect_true(!is.null(lt$from_type_id))
    expect_true(!is.null(lt$to_type_id))
    expect_true(!is.null(lt$cardinality))
  }
})

test_that("dis_to_bundle includes concept_defs when include_concepts = TRUE", {
  ot <- new_candidate_object_type("Patient", "Patient", confidence = 0.9)
  ot$status <- "approved"

  ch <- new_candidate_concept_hint(
    id             = "high_risk",
    display_name   = "High Risk",
    object_type_id = "Patient",
    sql_expr       = "age > 65",
    confidence     = 0.8
  )
  ch$status <- "approved"

  sess <- discover_session("concept-test")
  sess$candidates$object_types  <- list(ot)
  sess$candidates$concept_hints <- list(ch)

  bundle_no  <- dis_to_bundle(sess, "test", "Test", include_concepts = FALSE)
  bundle_yes <- dis_to_bundle(sess, "test", "Test", include_concepts = TRUE)

  expect_equal(length(bundle_no$concept_defs),  0L)
  expect_equal(length(bundle_yes$concept_defs), 1L)
  expect_equal(bundle_yes$concept_defs[[1]]$id,       "high_risk")
  expect_equal(bundle_yes$concept_defs[[1]]$sql_expr, "age > 65")
})
