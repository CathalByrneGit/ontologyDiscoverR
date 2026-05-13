#' Call the Anthropic Claude API
#'
#' @param system_prompt Character string for the system prompt
#' @param user_prompt Character string for the user message
#' @param max_tokens Maximum tokens to generate (default 4096)
#' @param temperature Sampling temperature (default 0.1)
#' @param response_format Either "json" or "text"
#' @return Parsed response: a list if json, character if text
#' @export
call_claude <- function(system_prompt, user_prompt,
                        max_tokens = 4096L,
                        temperature = 0.1,
                        response_format = c("json", "text")) {
  response_format <- match.arg(response_format)

  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(api_key)) {
    rlang::abort(
      "ANTHROPIC_API_KEY environment variable is not set. Please set it before calling ontologyDiscoverR functions.",
      call = NULL
    )
  }

  body <- list(
    model       = "claude-sonnet-4-20250514",
    max_tokens  = as.integer(max_tokens),
    temperature = temperature,
    system      = system_prompt,
    messages    = list(
      list(role = "user", content = user_prompt)
    )
  )

  req <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_retry(
      max_tries = 4L,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429L, 529L),
      backoff = function(i) 2^i
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)

  if (httr2::resp_is_error(resp)) {
    status <- httr2::resp_status(resp)
    body_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    rlang::abort(sprintf("Claude API error (HTTP %d): %s", status, body_text), call = NULL)
  }

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- parsed$content

  if (length(content) == 0) {
    rlang::abort("Claude API returned empty content", call = NULL)
  }

  text <- content[[1]]$text

  if (response_format == "json") {
    # Strip markdown fences if present
    text <- sub("^```json\\s*\\n?", "", text)
    text <- sub("\\n?```\\s*$", "", text)
    result <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(e) rlang::abort(
        paste0("Claude returned invalid JSON: ", conditionMessage(e), "\nRaw: ", substr(text, 1, 500)),
        call = NULL
      )
    )
    result
  } else {
    text
  }
}

# Build the JSON schema section appended to every system prompt
json_schema_footer <- function(schema_json) {
  paste0(
    "\n\nRespond ONLY with a valid JSON object matching the schema provided. ",
    "No preamble, no explanation, no markdown fences.\n\nSchema:\n",
    jsonlite::toJSON(schema_json, auto_unbox = TRUE, pretty = TRUE)
  )
}
