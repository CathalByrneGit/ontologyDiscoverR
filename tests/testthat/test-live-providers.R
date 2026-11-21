# Optional live smoke tests: one per provider, each skipped unless its
# credentials or local server are available. They never run on CRAN or CI by
# default. Set ONTOLOGYDISCOVERR_LIVE_TESTS=true to opt in.

live_smoke <- function(llm) {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("ONTOLOGYDISCOVERR_LIVE_TESTS"), "true"),
              "Set ONTOLOGYDISCOVERR_LIVE_TESTS=true to run live provider tests")
  src <- fake_source("CREATE TABLE patients (patient_id INTEGER PRIMARY KEY, name TEXT);")
  result <- extract_object_types(src, llm = llm)
  expect_true(length(result) >= 1)
  expect_s3_class(result[[1]], "CandidateObjectType")
}

test_that("live: Anthropic", {
  skip_if(Sys.getenv("ANTHROPIC_API_KEY") == "")
  live_smoke(Sys.getenv("ONTOLOGYDISCOVERR_TEST_ANTHROPIC", "anthropic"))
})

test_that("live: OpenAI", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "")
  live_smoke(Sys.getenv("ONTOLOGYDISCOVERR_TEST_OPENAI", "openai"))
})

test_that("live: Gemini", {
  skip_if(Sys.getenv("GEMINI_API_KEY") == "" && Sys.getenv("GOOGLE_API_KEY") == "")
  live_smoke(Sys.getenv("ONTOLOGYDISCOVERR_TEST_GEMINI", "google_gemini"))
})

test_that("live: Ollama", {
  ollama_up <- tryCatch({
    con <- url("http://localhost:11434/api/tags")
    on.exit(close(con))
    length(readLines(con, warn = FALSE)) > 0
  }, error = function(e) FALSE, warning = function(w) FALSE)
  skip_if_not(ollama_up, "Ollama is not reachable on localhost:11434")
  model <- Sys.getenv("ONTOLOGYDISCOVERR_TEST_OLLAMA_MODEL", "")
  skip_if(model == "", "Set ONTOLOGYDISCOVERR_TEST_OLLAMA_MODEL to an installed model")
  live_smoke(paste0("ollama/", model))
})
