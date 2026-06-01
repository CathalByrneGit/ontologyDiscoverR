#' Call an LLM via an ellmer chat provider
#'
#' @param llm_fn A one-argument function `f(system_prompt)` that returns an ellmer Chat
#'   object. Examples:
#'   - `function(sp) ellmer::chat_anthropic(system_prompt = sp)` (default)
#'   - `function(sp) ellmer::chat_openai(model = "gpt-4o", system_prompt = sp)`
#'   - `function(sp) ellmer::chat_ollama(model = "llama3.3:70b", system_prompt = sp)`
#' @param system_prompt System prompt string
#' @param user_prompt User message string
#' @param response_format Either "json" or "text"
#' @return Parsed list if json, character if text
#' @export
call_llm <- function(llm_fn = NULL, system_prompt, user_prompt,
                     response_format = c("json", "text")) {
  response_format <- match.arg(response_format)
  fn   <- llm_fn %||% .default_llm_fn()
  chat <- fn(system_prompt)
  text <- chat$chat(user_prompt)
  .parse_llm_response(text, response_format)
}

#' Call the Anthropic Claude API directly
#'
#' Convenience wrapper around `call_llm()` that always uses the Anthropic provider.
#' Reads `ANTHROPIC_API_KEY` from the environment via ellmer.
#'
#' @inheritParams call_llm
#' @param max_tokens Ignored (kept for backwards compatibility)
#' @param temperature Ignored (kept for backwards compatibility)
#' @export
call_claude <- function(system_prompt, user_prompt,
                        max_tokens   = 4096L,
                        temperature  = 0.1,
                        response_format = c("json", "text")) {
  response_format <- match.arg(response_format)
  call_llm(.default_llm_fn(), system_prompt, user_prompt, response_format)
}

# Returns a one-argument function sp -> Chat using Anthropic Claude
.default_llm_fn <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    rlang::abort(
      c("Package 'ellmer' is required for LLM calls.",
        i = "Install it with: install.packages('ellmer')",
        i = "Or pass your own provider: discover_session(llm = function(sp) ellmer::chat_ollama('llama3.3', system_prompt = sp))"),
      call = NULL
    )
  }
  function(system_prompt) {
    ellmer::chat_anthropic(
      system_prompt = system_prompt,
      model         = "claude-sonnet-4-20250514"
    )
  }
}

.parse_llm_response <- function(text, response_format) {
  if (response_format == "text") return(text)
  text <- sub("^```json\\s*\\n?", "", text)
  text <- sub("\\n?```\\s*$",     "", text)
  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) rlang::abort(
      paste0("LLM returned invalid JSON: ", conditionMessage(e),
             "\nRaw: ", substr(text, 1, 500)),
      call = NULL
    )
  )
}

# Append a JSON schema instruction to a system prompt
json_schema_footer <- function(schema_json) {
  paste0(
    "\n\nRespond ONLY with a valid JSON object matching the schema provided. ",
    "No preamble, no explanation, no markdown fences.\n\nSchema:\n",
    jsonlite::toJSON(schema_json, auto_unbox = TRUE, pretty = TRUE)
  )
}
