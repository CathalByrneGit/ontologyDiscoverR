test_that("extract_object_types returns correctly-shaped CandidateObjectType objects", {
  skip_if_not_installed("mockery")

  fixture_path <- file.path(
    system.file(package = "ontologyDiscoverR"),
    "fixtures", "example-schema.sql"
  )
  skip_if(!file.exists(fixture_path), "Fixture file not found")

  source <- parse_sql_ddl(fixture_path)

  withr::with_envvar(list(ANTHROPIC_API_KEY = "test-key"), {
    mockery::stub(
      extract_object_types, "call_claude",
      function(...) hospital_object_types_response()
    )
    result <- extract_object_types(source)
  })

  expect_true(length(result) >= 1)
  expect_s3_class(result[[1]], "CandidateObjectType")
  expect_s3_class(result[[1]], "Candidate")

  first <- result[[1]]
  expect_true(nzchar(first$candidate_id))
  expect_equal(first$status, "pending")
  expect_true(first$confidence >= 0 && first$confidence <= 1)
  expect_equal(first$element_type, "object_type")
  expect_true(is.list(first$properties))
  expect_true(is.list(first$source_refs))
})

test_that("extract_link_types returns CandidateLinkType objects with valid from/to", {
  skip_if_not_installed("mockery")

  object_types <- list(
    new_candidate_object_type("Patient", "Patient"),
    new_candidate_object_type("Ward",    "Ward"),
    new_candidate_object_type("Doctor",  "Doctor")
  )

  source <- structure(list(
    source_id    = "test-src-1",
    source_type  = "sql_ddl",
    source_label = "test.sql",
    raw_text     = "TABLE patients; TABLE wards; TABLE admissions(patient_id FK patients);",
    structured   = list(tables = list()),
    metadata     = list()
  ), class = c("DiscoverySource", "list"))

  withr::with_envvar(list(ANTHROPIC_API_KEY = "test-key"), {
    mockery::stub(
      extract_link_types, "call_claude",
      function(...) hospital_link_types_response()
    )
    result <- extract_link_types(source, object_types)
  })

  expect_true(length(result) >= 1)
  expect_s3_class(result[[1]], "CandidateLinkType")

  valid_ids <- c("Patient", "Ward", "Doctor")
  for (lt in result) {
    expect_true(lt$from_type_id %in% valid_ids)
    expect_true(lt$to_type_id   %in% valid_ids)
    expect_true(lt$cardinality  %in% c("one_to_one", "one_to_many", "many_to_many"))
  }
})

test_that("extract_object_types normalises property types", {
  skip_if_not_installed("mockery")

  source <- structure(list(
    source_id    = "test-src-2",
    source_type  = "sql_ddl",
    source_label = "test.sql",
    raw_text     = "TABLE foo (id int, name varchar(100));",
    structured   = list(tables = list()),
    metadata     = list()
  ), class = c("DiscoverySource", "list"))

  raw_response <- list(
    object_types = list(
      list(id = "Foo", display_name = "Foo",
           description = "", source_ref = "foo", confidence = 0.9,
           primary_key = list(property_id = "id", type = "int"),
           properties = list(
             list(id = "id",   type = "int",          nullable = FALSE, description = ""),
             list(id = "name", type = "varchar(100)",  nullable = TRUE,  description = "")
           ))
    )
  )

  withr::with_envvar(list(ANTHROPIC_API_KEY = "test-key"), {
    mockery::stub(extract_object_types, "call_claude", function(...) raw_response)
    result <- extract_object_types(source)
  })

  expect_equal(length(result), 1L)
  props <- result[[1]]$properties
  id_prop   <- Filter(function(p) p$id == "id",   props)[[1]]
  name_prop <- Filter(function(p) p$id == "name", props)[[1]]

  expect_equal(id_prop$type,   "integer")
  expect_equal(name_prop$type, "string")
})

test_that("extract_concept_hints returns CandidateConceptHint with sql_expr", {
  skip_if_not_installed("mockery")

  object_types <- list(
    new_candidate_object_type("Patient", "Patient",
      properties = list(
        list(id = "age", type = "integer", nullable = TRUE, description = ""),
        list(id = "length_of_stay", type = "integer", nullable = TRUE, description = "")
      ))
  )

  source <- structure(list(
    source_id    = "test-src-3",
    source_type  = "markdown",
    source_label = "docs.md",
    raw_text     = "High risk patients are those over 65.",
    structured   = NULL,
    metadata     = list()
  ), class = c("DiscoverySource", "list"))

  withr::with_envvar(list(ANTHROPIC_API_KEY = "test-key"), {
    mockery::stub(extract_concept_hints, "call_claude", function(...) hospital_concept_hints_response())
    result <- extract_concept_hints(source, object_types)
  })

  expect_true(length(result) >= 1)
  expect_s3_class(result[[1]], "CandidateConceptHint")
  expect_true(nzchar(result[[1]]$sql_expr))
})
