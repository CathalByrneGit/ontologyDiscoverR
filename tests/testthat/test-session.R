test_that("discover_session creates a valid session", {
  sess <- discover_session("test-session")

  expect_s3_class(sess, "DiscoverySession")
  expect_equal(sess$label, "test-session")
  expect_equal(length(sess$sources), 0L)
  expect_false(sess$extracted)
  expect_true(nzchar(sess$session_id))
})

test_that("dis_add_source adds a source to the session", {
  sess <- discover_session()
  df   <- data.frame(id = 1:3, name = letters[1:3])
  src  <- parse_r_dataframe(df, "TestTable")

  sess <- dis_add_source(sess, src)
  expect_equal(length(sess$sources), 1L)
  expect_equal(sess$sources[[1]]$source_type, "r_dataframe")
})

test_that("dis_add_file auto-detects type from extension", {
  fixture_path <- file.path(
    system.file(package = "ontologyDiscoverR"),
    "fixtures", "example-schema.sql"
  )
  skip_if(!file.exists(fixture_path), "Fixture file not found")

  sess <- discover_session()
  sess <- dis_add_file(sess, fixture_path)

  expect_equal(length(sess$sources), 1L)
  expect_equal(sess$sources[[1]]$source_type, "sql_ddl")
})

test_that("dis_extract runs the full pipeline on the fake Chat", {
  fixture_path <- system.file("fixtures", "example-schema.sql", package = "ontologyDiscoverR")
  skip_if(!nzchar(fixture_path), "Fixture file not found")

  chat <- fake_chat(hospital_responder, provider = "Ollama", model = "llama-test")
  sess <- discover_session("test", llm = chat)
  sess <- dis_add_file(sess, fixture_path)

  result <- dis_extract(sess, verbose = FALSE)

  expect_s3_class(result, "DiscoverySession")
  expect_true(result$extracted)
  expect_length(result$candidates$object_types, 3L)
  expect_length(result$candidates$link_types,   2L)
  expect_equal(result$llm_info$label, "ollama/llama-test")
  expect_equal(result$candidates$object_types[[1]]$extracted_by, "ollama/llama-test")
})

test_that("dis_extract errors clearly when no provider is configured", {
  without_configured_provider({
    sess <- dis_add_source(discover_session("none"), fake_source())
    expect_error(dis_extract(sess, verbose = FALSE),
                 class = "ontologyDiscoverR_no_provider")
  })
})

test_that("dis_to_bundle creates a valid bundle from approved candidates", {
  ot1 <- new_candidate_object_type("Patient", "Patient",
    properties = list(list(id = "patient_id", type = "integer", nullable = FALSE, description = "")),
    confidence = 0.95)
  ot1$status <- "approved"

  ot2 <- new_candidate_object_type("Ward", "Ward",
    properties = list(list(id = "ward_id", type = "integer", nullable = FALSE, description = "")),
    confidence = 0.92)
  ot2$status <- "approved"

  lt1 <- new_candidate_link_type("patient_ward", "Patient Ward",
    from_type_id = "Patient", to_type_id = "Ward",
    confidence = 0.9)
  lt1$status <- "approved"

  sess <- discover_session("bundle-test")
  sess$candidates$object_types <- list(ot1, ot2)
  sess$candidates$link_types   <- list(lt1)

  bundle <- dis_to_bundle(sess, bundle_id = "hospital-demo", bundle_name = "Hospital Demo")

  expect_s3_class(bundle, "ontology_bundle")
  expect_true(ontologySpecR::validate_bundle(bundle))
  expect_equal(ontologySpecR::bundle_id(bundle), "hospital-demo")
  expect_equal(bundle$metadata$name, "Hospital Demo")
  expect_length(ontologySpecR::bundle_object_types(bundle), 2L)
  expect_length(ontologySpecR::bundle_link_types(bundle),   1L)
})

test_that("dis_to_bundle filters by min_confidence for pending candidates", {
  ot_high <- new_candidate_object_type("Patient", "Patient", confidence = 0.9)
  ot_low  <- new_candidate_object_type("Unknown", "Unknown", confidence = 0.3)

  sess <- discover_session("conf-test")
  sess$candidates$object_types <- list(ot_high, ot_low)

  bundle <- suppressWarnings(dis_to_bundle(sess, "test", "Test", min_confidence = 0.5))

  included_ids <- names(ontologySpecR::bundle_object_types(bundle))
  expect_true("Patient" %in% included_ids)
  expect_false("Unknown" %in% included_ids)
})

test_that("dis_to_bundle excludes link types referencing non-included object types", {
  ot_approved <- new_candidate_object_type("Patient", "Patient", confidence = 0.9)
  ot_approved$status <- "approved"

  lt_valid <- new_candidate_link_type("self_ref", "Self Ref",
    from_type_id = "Patient", to_type_id = "Patient", confidence = 0.8)
  lt_valid$status <- "approved"

  lt_dangling <- new_candidate_link_type("dangling", "Dangling",
    from_type_id = "Patient", to_type_id = "Ghost", confidence = 0.8)
  lt_dangling$status <- "approved"

  sess <- discover_session("link-test")
  sess$candidates$object_types <- list(ot_approved)
  sess$candidates$link_types   <- list(lt_valid, lt_dangling)

  expect_warning(
    bundle <- suppressWarnings(dis_to_bundle(sess, "test", "Test"),
                               classes = "ontologyDiscoverR_surrogate_key"),
    class = "ontologyDiscoverR_dropped_candidate"
  )

  link_ids <- names(ontologySpecR::bundle_link_types(bundle))
  expect_true("self_ref"  %in% link_ids)
  expect_false("dangling" %in% link_ids)
})
