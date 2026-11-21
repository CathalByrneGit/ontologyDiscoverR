#' Call an LLM through any ellmer chat provider
#'
#' Sends one request to a language model. The provider is chosen by `llm`
#' (see **Choosing a provider**); nothing is hard-coded. Each call starts from a
#' fresh conversation, so separate calls never share history.
#'
#' @section Choosing a provider:
#' `llm` may be:
#' * an ellmer `Chat` object, e.g. `ellmer::chat_openai(model = "<model>")`.
#'   It is cloned for each request and given the system prompt with
#'   `$set_system_prompt()`;
#' * a function `f(system_prompt)` returning a `Chat`;
#' * a `"provider/model"` string, passed to `ellmer::chat()`;
#' * `NULL`, which uses `getOption("ontologyDiscoverR.chat")` and then the
#'   `ONTOLOGYDISCOVERR_CHAT` environment variable. If neither is set, the call
#'   errors: there is no silent default vendor.
#'
#' @param llm The chat provider (see **Choosing a provider**).
#' @param system_prompt System prompt string.
#' @param user_prompt User message string.
#' @param response_format `"json"` (returns a parsed list) or `"text"`
#'   (returns a character string).
#' @param type Optional ellmer type specification (see `R/llm_types.R`). When
#'   given and `response_format = "json"`, the request uses
#'   `$chat_structured()`. If the provider cannot do structured output, the
#'   answer is parsed from text instead, with one retry.
#' @param llm_fn Deprecated alias for `llm`.
#' @return A list (json) or a character string (text).
#' @export
call_llm <- function(llm = NULL, system_prompt, user_prompt,
                     response_format = c("json", "text"),
                     type = NULL, llm_fn = NULL) {
  response_format <- match.arg(response_format)
  llm  <- llm %||% llm_fn
  chat <- .resolve_chat(llm, system_prompt)

  if (response_format == "text") {
    return(.chat_text(chat, user_prompt))
  }

  if (!is.null(type) && is.function(chat$chat_structured)) {
    structured <- tryCatch(
      # convert = FALSE returns plain lists (as jsonlite::fromJSON(simplifyVector
      # = FALSE) would), which is what every caller expects.
      list(ok = TRUE, value = chat$chat_structured(user_prompt, type = type,
                                                   convert = FALSE)),
      error = function(e) list(ok = FALSE, error = e)
    )
    if (isTRUE(structured$ok)) return(.normalise_structured(structured$value))
    # Provider does not support structured output (or it failed): fall back to
    # prompt-and-parse on a fresh conversation.
    chat <- .resolve_chat(llm, system_prompt)
  }

  .chat_json_with_retry(chat, user_prompt)
}

#' Call the Anthropic API (deprecated)
#'
#' `call_claude()` is deprecated and will be removed in the next minor version.
#' Use [call_llm()] with any provider instead.
#'
#' @inheritParams call_llm
#' @param max_tokens Ignored.
#' @param temperature Ignored.
#' @keywords internal
#' @export
call_claude <- function(system_prompt, user_prompt,
                        max_tokens   = 4096L,
                        temperature  = 0.1,
                        response_format = c("json", "text")) {
  response_format <- match.arg(response_format)
  rlang::warn(
    c("`call_claude()` is deprecated and will be removed in the next minor version.",
      i = "Use `call_llm(llm = <any ellmer chat>, ...)` instead."),
    class = "lifecycle_warning_deprecated"
  )
  .require_ellmer()
  # The one deliberate provider reference outside the setup-help message: this
  # shim exists only for backward compatibility and goes in the next minor
  # version.
  call_llm(ellmer::chat_anthropic(), system_prompt, user_prompt, response_format)
}

# ---------------------------------------------------------------------------
# Resolving the provider
# ---------------------------------------------------------------------------

# Turn any accepted `llm` value into a fresh Chat with `system_prompt` set.
# Order: explicit `llm` -> option ontologyDiscoverR.chat -> env var
# ONTOLOGYDISCOVERR_CHAT -> error.
.resolve_chat <- function(llm, system_prompt) {
  if (is.null(llm)) llm <- getOption("ontologyDiscoverR.chat")
  if (is.null(llm)) {
    env <- Sys.getenv("ONTOLOGYDISCOVERR_CHAT", unset = "")
    if (nzchar(env)) llm <- env
  }
  if (is.null(llm)) .abort_no_provider()

  if (.is_chat(llm)) {
    chat <- llm$clone()
    # Never carry conversation history from the template Chat into a request.
    if (is.function(chat$set_turns)) chat$set_turns(list())
    chat$set_system_prompt(system_prompt)
    return(chat)
  }
  if (is.function(llm)) {
    chat <- llm(system_prompt)
    if (!.is_chat(chat)) {
      rlang::abort(
        "The `llm` function must return an ellmer Chat object (with `$chat()`).",
        call = NULL
      )
    }
    return(chat)
  }
  if (is.character(llm) && length(llm) == 1L && nzchar(llm)) {
    .require_ellmer()
    if (!.ellmer_has_chat_string()) {
      rlang::abort(
        c(paste0("This version of ellmer has no `chat()` function, so `llm = \"",
                 llm, "\"` cannot be used."),
          i = "Pass a Chat object instead, e.g. `llm = ellmer::chat_openai(model = \"...\")`,",
          i = "or update ellmer: install.packages('ellmer')."),
        call = NULL
      )
    }
    chat_fn <- getExportedValue("ellmer", "chat")
    return(chat_fn(llm, system_prompt = system_prompt))
  }

  rlang::abort(
    paste0("`llm` must be an ellmer Chat object, a function(system_prompt) ",
           "returning one, a \"provider/model\" string, or NULL."),
    call = NULL
  )
}

.is_chat <- function(x) {
  (is.environment(x) || is.list(x)) && !is.null(x$chat) && is.function(x$chat) &&
    !is.function(x)
}

.ellmer_has_chat_string <- function() {
  requireNamespace("ellmer", quietly = TRUE) &&
    "chat" %in% getNamespaceExports("ellmer")
}

.require_ellmer <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    rlang::abort(
      c("Package 'ellmer' is required for LLM calls.",
        i = "Install it with: install.packages('ellmer')"),
      call = NULL
    )
  }
  invisible(TRUE)
}

.abort_no_provider <- function() {
  rlang::abort(
    c("No LLM provider is configured for ontologyDiscoverR.",
      i = "Pass one to `discover_session(llm = ...)`, for example:",
      " " = "  discover_session(llm = ellmer::chat_anthropic())",
      " " = "  discover_session(llm = ellmer::chat_openai(model = \"<model>\"))",
      " " = "  discover_session(llm = ellmer::chat_ollama(model = \"<model>\"))",
      i = "Or set a default for every session:",
      " " = "  options(ontologyDiscoverR.chat = \"openai/<model>\")",
      " " = "  Sys.setenv(ONTOLOGYDISCOVERR_CHAT = \"ollama/<model>\")"),
    class = "ontologyDiscoverR_no_provider",
    call  = NULL
  )
}

# Abort early (with setup help) when no provider is configured anywhere.
.check_provider <- function(llm) {
  if (is.null(llm) && is.null(getOption("ontologyDiscoverR.chat")) &&
      !nzchar(Sys.getenv("ONTOLOGYDISCOVERR_CHAT", unset = ""))) {
    .abort_no_provider()
  }
  invisible(TRUE)
}

# "provider/model" label for provenance, e.g. "openai/<model>".
.llm_info <- function(llm) {
  if (is.null(llm)) llm <- getOption("ontologyDiscoverR.chat")
  if (is.null(llm)) {
    env <- Sys.getenv("ONTOLOGYDISCOVERR_CHAT", unset = "")
    if (nzchar(env)) llm <- env
  }
  if (is.null(llm)) return(list(provider = NA_character_, model = NA_character_, label = NA_character_))

  provider <- NA_character_
  model    <- NA_character_
  if (is.character(llm)) {
    parts    <- strsplit(llm, "/", fixed = TRUE)[[1]]
    provider <- parts[1]
    model    <- if (length(parts) > 1) paste(parts[-1], collapse = "/") else NA_character_
  } else {
    chat <- if (.is_chat(llm)) llm else tryCatch(llm(""), error = function(e) NULL)
    if (!is.null(chat)) {
      p <- tryCatch(chat$get_provider(), error = function(e) NULL)
      provider <- if (!is.null(p)) .prop_or_na(p, "name") else NA_character_
      model    <- .chr_or_na(tryCatch(chat$get_model(), error = function(e) NULL))
      if (is.na(model) && !is.null(p)) model <- .prop_or_na(p, "model")
    }
  }
  label <- if (is.na(provider)) NA_character_
           else if (is.na(model)) provider
           else paste0(tolower(provider), "/", model)
  list(provider = provider, model = model, label = label)
}

# Read a property from an ellmer Provider (an S7 object, whose properties are
# stored as attributes) or from a plain list/environment.
.prop_or_na <- function(obj, name) {
  v <- tryCatch(attr(obj, name, exact = TRUE), error = function(e) NULL)
  if (is.null(v)) v <- tryCatch(obj[[name]], error = function(e) NULL)
  .chr_or_na(v)
}

.chr_or_na <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) x else NA_character_
}

# ---------------------------------------------------------------------------
# Sending and parsing
# ---------------------------------------------------------------------------

.chat_text <- function(chat, prompt) {
  out <- chat$chat(prompt, echo = "none")
  paste(as.character(out), collapse = "\n")
}

.chat_json_with_retry <- function(chat, user_prompt) {
  prompt <- paste0(user_prompt, .json_only_footer())
  text   <- .chat_text(chat, prompt)
  first  <- .try_parse_json(text)
  if (first$ok) return(first$value)

  retry_prompt <- paste0(
    "Your previous answer could not be parsed as JSON (", first$error, "). ",
    "Reply again with ONLY the JSON object: no prose, no Markdown fences."
  )
  text2  <- .chat_text(chat, retry_prompt)
  second <- .try_parse_json(text2)
  if (second$ok) return(second$value)

  rlang::abort(
    c("The LLM did not return valid JSON, even after one retry.",
      x = paste0("Parse error: ", second$error),
      i = paste0("Raw output (trimmed): ", .trim_output(text2))),
    class = "ontologyDiscoverR_bad_llm_json",
    call  = NULL
  )
}

.json_only_footer <- function() {
  "\n\nRespond ONLY with a valid JSON object. No preamble, no Markdown fences."
}

.try_parse_json <- function(text) {
  text <- trimws(text)
  text <- sub("^```[A-Za-z]*\\s*\\n?", "", text)
  text <- sub("\\n?```\\s*$",          "", text)
  # Tolerate prose around a single JSON object.
  if (!grepl("^[\\[{]", text)) {
    start <- regexpr("[\\[{]", text)
    if (start > 0) text <- substring(text, start)
  }
  tryCatch(
    list(ok = TRUE, value = jsonlite::fromJSON(text, simplifyVector = FALSE)),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )
}

.trim_output <- function(text, n = 500L) {
  text <- paste(text, collapse = "\n")
  if (nchar(text) > n) paste0(substr(text, 1, n), " [...]") else text
}

# Make chat_structured() output look like jsonlite::fromJSON(simplifyVector =
# FALSE) output, which is what all callers expect: lists of records, not
# data frames.
.normalise_structured <- function(x) {
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      .normalise_structured(lapply(as.list(x[i, , drop = FALSE]), function(col) {
        if (is.list(col) && length(col) == 1L) col[[1]] else col
      }))
    })
    return(rows)
  }
  if (is.list(x)) return(lapply(x, .normalise_structured))
  if (is.atomic(x) && length(x) > 1L) return(as.list(x))
  x
}
