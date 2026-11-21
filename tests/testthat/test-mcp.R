test_that("MCP builders name ontologyAPI when it is missing", {
  skip_if(requireNamespace("ontologyAPI", quietly = TRUE), "ontologyAPI is installed")
  sess <- discover_session("mcp", llm = fake_chat(list()))
  expect_error(dis_add_mcp_tool(list(), sess), "ontologyAPI")
})

test_that("ontologyAPI::mcp_server() registers the discover and populate tools", {
  skip_if_not_installed("ontologyAPI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  sess <- discover_session("mcp", llm = fake_chat(list()))
  sess$candidates$object_types <- list(local({
    ot <- new_candidate_object_type("Patient", "Patient",
      primary_key = list(property_id = "patient_id", type = "integer"),
      properties  = list(list(id = "patient_id", type = "integer", nullable = FALSE),
                         list(id = "full_name",  type = "string")))
    ot$status <- "approved"
    ot
  }))
  bundle <- dis_to_bundle(sess, "mcp-test", "MCP test")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "patients", data.frame(patient_id = 1:2, full_name = c("A", "B")))

  svc    <- ontologyAPI::ontology_service(bundle, con, actor = "test")
  server <- ontologyAPI::mcp_server(svc, name = "discover-test")
  dis_add_mcp_tool(server, sess)
  pop_add_mcp_tool(server, populate_session(bundle, llm = fake_chat(list())))

  expect_true(all(c("build_ontology", "query_population") %in% names(server$tools)))
  expect_true(is.function(server$tools$build_ontology$handler))
})

test_that("the build_ontology handler adds candidates and generates a valid bundle", {
  chat <- fake_chat(list(
    list(action = "add_object_type", id = "Hospital", display_name = "Hospital",
         description = "A hospital",
         properties = list(list(id = "hospital_id", type = "integer"),
                           list(id = "name", type = "text"))),
    list(action = "generate_bundle", bundle_id = "hospital-v1", bundle_name = "Hospitals")
  ), provider = "OpenAI", model = "gpt-test")
  env <- new.env()
  env$session <- discover_session("mcp", llm = chat)

  r1 <- .handle_dis_mcp_instruction("Add an object type for Hospital", env)
  expect_true(r1$success)
  expect_equal(env$session$candidates$object_types[[1]]$extracted_by, "openai/gpt-test")

  r2 <- .handle_dis_mcp_instruction("Generate the bundle as hospital-v1", env)
  expect_true(r2$success)
  expect_match(r2$message, "hospital-v1")
  expect_true(ontologySpecR::validate_bundle(env$session$last_bundle))
})

test_that("the query_population handler reads a canonical bundle", {
  sess <- discover_session("pop", llm = fake_chat(list()))
  sess$candidates$object_types <- list(local({
    ot <- new_candidate_object_type("Person", "Person",
      primary_key = list(property_id = "person_id", type = "integer"),
      properties  = list(list(id = "person_id", type = "integer", nullable = FALSE),
                         list(id = "name", type = "string")))
    ot$status <- "approved"
    ot
  }))
  bundle <- dis_to_bundle(sess, "pop-test", "Pop")
  env <- new.env()
  env$pop_sess <- populate_session(bundle, llm = fake_chat(list()))
  out <- .handle_pop_mcp_instruction("How many Person instances?", env)
  expect_true(out$success)
  expect_match(out$message, "Person: 0 instances")
})
