# Every extractor runs through the fake Chat: no network, no API key.

test_that("extract_object_types returns correctly-shaped CandidateObjectType objects", {
  fixture_path <- system.file("fixtures", "example-schema.sql", package = "ontologyDiscoverR")
  skip_if(!nzchar(fixture_path), "Fixture file not found")
  source <- parse_sql_ddl(fixture_path)

  chat   <- fake_chat(list(hospital_object_types_response()))
  result <- extract_object_types(source, llm = chat)

  expect_length(result, 3L)
  first <- result[[1]]
  expect_s3_class(first, "CandidateObjectType")
  expect_s3_class(first, "Candidate")
  expect_true(nzchar(first$candidate_id))
  expect_equal(first$status, "pending")
  expect_true(first$confidence >= 0 && first$confidence <= 1)
  expect_equal(first$element_type, "object_type")
  expect_true(is.list(first$properties))
  expect_true(is.list(first$source_refs))

  # Structured output was used, with the system prompt set on a clone.
  log <- chat$log()
  expect_equal(log[[1]]$method, "chat_structured")
  expect_match(log[[1]]$system_prompt, "extracting the data model")
  expect_null(chat$system_prompt)
})

test_that("extractors record the provider and model in provenance", {
  chat   <- fake_chat(list(hospital_object_types_response()),
                      provider = "OpenAI", model = "gpt-test")
  result <- extract_object_types(fake_source(), llm = chat)

  expect_equal(result[[1]]$extracted_by, "openai/gpt-test")
  expect_equal(result[[1]]$source_refs[[1]]$extracted_by, "openai/gpt-test")
})

test_that("extract_link_types returns CandidateLinkType objects with valid from/to", {
  object_types <- list(
    new_candidate_object_type("Patient", "Patient"),
    new_candidate_object_type("Ward",    "Ward"),
    new_candidate_object_type("Doctor",  "Doctor")
  )
  chat   <- fake_chat(list(hospital_link_types_response()))
  result <- extract_link_types(fake_source(), object_types, llm = chat)

  expect_length(result, 2L)
  expect_s3_class(result[[1]], "CandidateLinkType")
  for (lt in result) {
    expect_true(lt$from_type_id %in% c("Patient", "Ward", "Doctor"))
    expect_true(lt$to_type_id   %in% c("Patient", "Ward", "Doctor"))
    expect_true(lt$cardinality  %in% c("one_to_one", "one_to_many", "many_to_many"))
  }
})

test_that("extract_link_types drops links to unknown object types", {
  object_types <- list(new_candidate_object_type("Patient", "Patient"))
  chat   <- fake_chat(list(hospital_link_types_response()))
  result <- extract_link_types(fake_source(), object_types, llm = chat)
  expect_length(result, 0L)
})

test_that("extract_object_types normalises property types", {
  raw_response <- list(
    object_types = list(
      list(id = "Foo", display_name = "Foo",
           description = "", source_ref = "foo", confidence = 0.9,
           primary_key = list(property_id = "id", type = "int"),
           properties = list(
             list(id = "id",   type = "int",          nullable = FALSE, description = ""),
             list(id = "name", type = "varchar(100)", nullable = TRUE,  description = "")
           ))
    )
  )
  result <- extract_object_types(fake_source(), llm = fake_chat(list(raw_response)))

  props     <- result[[1]]$properties
  id_prop   <- Filter(function(p) p$id == "id",   props)[[1]]
  name_prop <- Filter(function(p) p$id == "name", props)[[1]]
  expect_equal(id_prop$type,   "integer")
  expect_equal(name_prop$type, "string")
})

test_that("extract_action_types returns CandidateActionType objects", {
  object_types <- list(new_candidate_object_type("Patient", "Patient"))
  result <- extract_action_types(fake_source(), object_types,
                                 llm = fake_chat(list(hospital_action_types_response())))
  expect_length(result, 2L)
  expect_s3_class(result[[1]], "CandidateActionType")
  expect_equal(result[[1]]$object_type_id, "Patient")
})

test_that("extract_concept_hints returns CandidateConceptHint with sql_expr", {
  object_types <- list(
    new_candidate_object_type("Patient", "Patient",
      properties = list(list(id = "age", type = "integer", nullable = TRUE, description = "")))
  )
  result <- extract_concept_hints(fake_source(type = "markdown", label = "docs.md"),
                                  object_types,
                                  llm = fake_chat(list(hospital_concept_hints_response())))
  expect_true(length(result) >= 1)
  expect_s3_class(result[[1]], "CandidateConceptHint")
  expect_true(nzchar(result[[1]]$sql_expr))
})

test_that("extractors work with a provider that has no structured output", {
  chat <- fake_chat(
    list(paste0("```json\n", jsonlite::toJSON(hospital_object_types_response(), auto_unbox = TRUE), "\n```")),
    structured = FALSE
  )
  result <- extract_object_types(fake_source(), llm = chat)
  expect_length(result, 3L)
  methods <- vapply(chat$log(), function(x) x$method, character(1))
  expect_equal(methods, c("chat_structured", "chat"))
})
