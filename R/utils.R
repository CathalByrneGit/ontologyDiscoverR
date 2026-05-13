# Generate a UUID v4
new_uuid <- function() {
  paste(
    paste0(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = ""),
    paste0(sample(c(0:9, letters[1:6]), 4, replace = TRUE), collapse = ""),
    paste0(c("4", sample(c(0:9, letters[1:6]), 3, replace = TRUE)), collapse = ""),
    paste0(c(sample(c("8", "9", "a", "b"), 1), sample(c(0:9, letters[1:6]), 3, replace = TRUE)), collapse = ""),
    paste0(sample(c(0:9, letters[1:6]), 12, replace = TRUE), collapse = ""),
    sep = "-"
  )
}

# Check if a string is a valid UUID
is_uuid <- function(x) {
  grepl("^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$", x, ignore.case = TRUE)
}

# Truncate text to max_chars with a warning
truncate_text <- function(text, max_chars = 50000L, source_label = "") {
  if (nchar(text) > max_chars) {
    rlang::warn(paste0("Source '", source_label, "' truncated to ", max_chars, " characters"))
    substr(text, 1, max_chars)
  } else {
    text
  }
}

# Safe JSON parse that returns NULL on failure
safe_json_parse <- function(text) {
  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# Coerce value to named list, handling NULL
as_list_or_null <- function(x) {
  if (is.null(x)) NULL else as.list(x)
}
