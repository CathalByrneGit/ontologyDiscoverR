# .resolve_chat(): every accepted form of `llm`, and the fallback order.

test_that(".resolve_chat() clones a Chat object and sets the system prompt", {
  template <- fake_chat(list())
  template$set_turns(list("old turn"))
  chat <- .resolve_chat(template, "SYSTEM A")

  expect_false(identical(chat, template))
  expect_equal(chat$system_prompt, "SYSTEM A")
  expect_length(chat$turns, 0L)          # no shared history
  expect_null(template$system_prompt)    # template untouched
  expect_equal(template$turns, list("old turn"))
})

test_that(".resolve_chat() calls a function(system_prompt)", {
  seen <- NULL
  f <- function(sp) {
    seen <<- sp
    fake_chat(list())
  }
  chat <- .resolve_chat(f, "SYSTEM B")
  expect_equal(seen, "SYSTEM B")
  expect_true(.is_chat(chat))

  expect_error(.resolve_chat(function(sp) "not a chat", "x"), "must return an ellmer Chat")
})

test_that(".resolve_chat() passes a 'provider/model' string to ellmer::chat()", {
  skip_if_not(.ellmer_has_chat_string())
  captured <- NULL
  local_mocked_bindings(
    chat = function(name, ..., system_prompt = NULL) {
      captured <<- list(name = name, system_prompt = system_prompt)
      fake_chat(list())
    },
    .package = "ellmer"
  )
  chat <- .resolve_chat("openai/gpt-test", "SYSTEM C")
  expect_equal(captured, list(name = "openai/gpt-test", system_prompt = "SYSTEM C"))
  expect_true(.is_chat(chat))
})

test_that(".resolve_chat() falls back to the option, then the env var", {
  opt_chat <- fake_chat(list(), provider = "FromOption")
  withr::local_options(ontologyDiscoverR.chat = opt_chat)
  withr::local_envvar(ONTOLOGYDISCOVERR_CHAT = "envprovider/env-model")
  expect_equal(attr(.resolve_chat(NULL, "s")$get_provider(), "name"), "FromOption")

  # An explicit llm beats the option.
  explicit <- fake_chat(list(), provider = "Explicit")
  expect_equal(attr(.resolve_chat(explicit, "s")$get_provider(), "name"), "Explicit")

  # Without the option, the env var string is used.
  withr::local_options(ontologyDiscoverR.chat = NULL)
  skip_if_not(.ellmer_has_chat_string())
  captured <- NULL
  local_mocked_bindings(
    chat = function(name, ..., system_prompt = NULL) {
      captured <<- name
      fake_chat(list())
    },
    .package = "ellmer"
  )
  .resolve_chat(NULL, "s")
  expect_equal(captured, "envprovider/env-model")
})

test_that(".resolve_chat() aborts with setup help when nothing is configured", {
  without_configured_provider({
    err <- tryCatch(.resolve_chat(NULL, "s"), error = identity)
    expect_s3_class(err, "ontologyDiscoverR_no_provider")
    msg <- conditionMessage(err)
    expect_match(msg, "chat_anthropic", fixed = TRUE)
    expect_match(msg, "chat_openai", fixed = TRUE)
    expect_match(msg, "chat_ollama", fixed = TRUE)
    expect_match(msg, "ontologyDiscoverR.chat", fixed = TRUE)
    expect_match(msg, "ONTOLOGYDISCOVERR_CHAT", fixed = TRUE)
  })
})

test_that(".resolve_chat() rejects other inputs", {
  expect_error(.resolve_chat(42, "s"), "must be an ellmer Chat")
})

test_that("call_llm() uses chat_structured() when a type is given", {
  chat <- fake_chat(list(list(answer = "yes")))
  out  <- call_llm(chat, "sys", "question", type = .llm_type("mcp_action"))
  expect_equal(out, list(answer = "yes"))
  expect_equal(chat$log()[[1]]$method, "chat_structured")
})

test_that("call_llm() falls back to text parsing when structured output fails", {
  chat <- fake_chat(list('Here you go: {"answer": "parsed"}'), structured = FALSE)
  out  <- call_llm(chat, "sys", "question", type = .llm_type("mcp_action"))
  expect_equal(out, list(answer = "parsed"))
  methods <- vapply(chat$log(), function(x) x$method, character(1))
  expect_equal(methods, c("chat_structured", "chat"))
})

test_that("call_llm() retries once with the parse error, then succeeds", {
  chat <- fake_chat(list("not json at all", '{"answer": "second try"}'), structured = FALSE)
  out  <- call_llm(chat, "sys", "question", type = .llm_type("mcp_action"))
  expect_equal(out, list(answer = "second try"))

  log <- chat$log()
  expect_length(log, 3L)                      # structured attempt, text, retry
  expect_match(log[[3]]$prompt, "could not be parsed as JSON")
})

test_that("call_llm() aborts with the trimmed raw output when the retry also fails", {
  long <- paste(rep("garbage", 200), collapse = " ")
  chat <- fake_chat(list("nope", long), structured = FALSE)
  err  <- tryCatch(call_llm(chat, "sys", "q", type = .llm_type("mcp_action")),
                   error = identity)
  expect_s3_class(err, "ontologyDiscoverR_bad_llm_json")
  expect_match(conditionMessage(err), "Raw output (trimmed): garbage", fixed = TRUE)
  expect_match(conditionMessage(err), "[...]", fixed = TRUE)
  expect_lt(nchar(conditionMessage(err)), nchar(long))
})

test_that("call_llm() without a type parses text directly", {
  chat <- fake_chat(list('{"a": 1}'))
  expect_equal(call_llm(chat, "sys", "q"), list(a = 1L))
  expect_equal(chat$log()[[1]]$method, "chat")
})

test_that("call_llm(response_format = 'text') returns the text", {
  chat <- fake_chat(list("plain answer"))
  expect_equal(call_llm(chat, "sys", "q", response_format = "text"), "plain answer")
})

test_that("separate call_llm() calls never share history", {
  template <- fake_chat(list(list(n = 1), list(n = 2)))
  call_llm(template, "first system", "q1", type = .llm_type("mcp_action"))
  call_llm(template, "second system", "q2", type = .llm_type("mcp_action"))
  log <- template$log()
  expect_equal(log[[1]]$system_prompt, "first system")
  expect_equal(log[[2]]$system_prompt, "second system")
  expect_null(template$system_prompt)
})

test_that("discover_session() records the provider and model", {
  sess <- discover_session("info", llm = fake_chat(list(), provider = "Gemini", model = "g-1"))
  expect_equal(sess$llm_info$provider, "Gemini")
  expect_equal(sess$llm_info$model, "g-1")
  expect_equal(sess$llm_info$label, "gemini/g-1")

  sess2 <- discover_session("info2", llm = "ollama/llama3.3")
  expect_equal(sess2$llm_info$label, "ollama/llama3.3")
})

test_that("call_claude() is deprecated", {
  local_mocked_bindings(call_llm = function(...) "called")
  local_mocked_bindings(chat_anthropic = function(...) fake_chat(list()), .package = "ellmer")
  expect_warning(out <- call_claude("s", "u"), class = "lifecycle_warning_deprecated")
  expect_equal(out, "called")
})

test_that(".property_pairs_to_list() handles both answer shapes", {
  expect_equal(
    .property_pairs_to_list(list(list(property = "name", value = "Ann"),
                                 list(property = "age",  value = "40"))),
    list(name = "Ann", age = "40")
  )
  expect_equal(.property_pairs_to_list(list(name = "Ann")), list(name = "Ann"))
  expect_equal(.property_pairs_to_list(NULL), list())
})
