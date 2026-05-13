#' Parse a SQL DDL file containing CREATE TABLE statements
#'
#' @param path Path to the .sql file
#' @return A DiscoverySource object with structured table information
#' @export
parse_sql_ddl <- function(path) {
  if (!file.exists(path)) rlang::abort(paste0("File not found: ", path))

  raw_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  tables <- .parse_ddl_tables(raw_text)

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = "sql_ddl",
      source_label = basename(path),
      raw_text     = raw_text,
      structured   = list(tables = tables),
      metadata     = list(path = path, n_tables = length(tables))
    ),
    class = c("DiscoverySource", "list")
  )
}

# Internal: parse CREATE TABLE blocks from DDL text
.parse_ddl_tables <- function(ddl_text) {
  # Match CREATE TABLE blocks (handle IF NOT EXISTS, schema prefixes)
  pattern <- "CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?([`\"\\[\\]\\w\\.]+)\\s*\\(([^;]+?)\\)\\s*;"
  matches <- gregexpr(pattern, ddl_text, ignore.case = TRUE, perl = TRUE)

  table_blocks <- regmatches(ddl_text, matches)[[1]]

  if (length(table_blocks) == 0) {
    # Try without trailing semicolon (some DDLs use GO or nothing)
    pattern2 <- "CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?([`\"\\[\\]\\w\\.]+)\\s*\\(([\\s\\S]+?)\\)(?=\\s*(?:;|CREATE|$))"
    matches2 <- gregexpr(pattern2, ddl_text, ignore.case = TRUE, perl = TRUE)
    table_blocks <- regmatches(ddl_text, matches2)[[1]]
  }

  lapply(table_blocks, .parse_table_block)
}

.parse_table_block <- function(block) {
  # Extract table name
  name_match <- regmatches(block, regexpr(
    "CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?([`\"\\[\\]\\w\\.]+)",
    block, ignore.case = TRUE, perl = TRUE
  ))
  table_name <- if (length(name_match) > 0) {
    nm <- sub("CREATE\\s+TABLE\\s+(?:IF\\s+NOT\\s+EXISTS\\s+)?", "", name_match[1], ignore.case = TRUE, perl = TRUE)
    gsub("[`\"\\[\\]]", "", nm)
    # Remove schema prefix if present
    sub("^[^.]+\\.", "", gsub("[`\"\\[\\]]", "", nm))
  } else "unknown"

  # Extract body between outer parens
  body_match <- regmatches(block, regexpr("\\(([\\s\\S]+)\\)", block, perl = TRUE))
  body <- if (length(body_match) > 0) {
    sub("^\\(", "", sub("\\)$", "", body_match[1]))
  } else ""

  columns <- .parse_columns(body, table_name)

  list(
    name    = table_name,
    columns = columns
  )
}

.parse_columns <- function(body, table_name) {
  # Split by commas not inside parens
  lines <- .split_column_defs(body)

  pk_cols <- character(0)
  fk_map  <- list()
  col_defs <- list()

  for (line in lines) {
    line <- trimws(line)
    if (!nzchar(line)) next

    upper <- toupper(line)

    # PRIMARY KEY constraint
    if (grepl("^PRIMARY\\s+KEY", upper)) {
      pk_match <- regmatches(line, regexpr("\\(([^)]+)\\)", line, perl = TRUE))
      if (length(pk_match) > 0) {
        pk_cols <- c(pk_cols, trimws(strsplit(gsub("[`\"\\[\\]]", "", sub("^\\(", "", sub("\\)$", "", pk_match[1]))), ",")[[1]]))
      }
      next
    }

    # FOREIGN KEY constraint
    if (grepl("^(?:CONSTRAINT\\s+\\w+\\s+)?FOREIGN\\s+KEY", upper, perl = TRUE)) {
      fk_cols <- regmatches(line, regexpr("FOREIGN\\s+KEY\\s*\\(([^)]+)\\)", line, ignore.case = TRUE, perl = TRUE))
      ref_tbl  <- regmatches(line, regexpr("REFERENCES\\s+([`\"\\[\\]\\w\\.]+)", line, ignore.case = TRUE, perl = TRUE))
      if (length(fk_cols) > 0 && length(ref_tbl) > 0) {
        col_name <- trimws(gsub("[`\"\\[\\]]", "",
          sub("FOREIGN\\s+KEY\\s*\\(", "", sub("\\)$", "", fk_cols[1]), ignore.case = TRUE, perl = TRUE)
        ))
        ref <- trimws(gsub("[`\"\\[\\]]", "",
          sub("REFERENCES\\s+", "", ref_tbl[1], ignore.case = TRUE, perl = TRUE)
        ))
        ref <- sub("^[^.]+\\.", "", ref)
        fk_map[[col_name]] <- ref
      }
      next
    }

    # Skip other constraints
    if (grepl("^(?:CONSTRAINT|UNIQUE|CHECK|INDEX|KEY)\\b", upper, perl = TRUE)) next

    # Column definition
    col <- .parse_column_def(line)
    if (!is.null(col)) col_defs <- c(col_defs, list(col))
  }

  # Apply PK and FK info
  lapply(col_defs, function(col) {
    col$pk <- col$name %in% pk_cols || col$pk
    col$fk_to <- fk_map[[col$name]]
    col
  })
}

.parse_column_def <- function(line) {
  # col_name TYPE [constraints...]
  tokens <- strsplit(trimws(line), "\\s+")[[1]]
  if (length(tokens) < 2) return(NULL)

  col_name <- gsub("[`\"\\[\\]]", "", tokens[1])
  col_type  <- tokens[2]
  # Handle type with size like VARCHAR(255) — reconnect if split
  if (length(tokens) >= 3 && grepl("\\($", col_type) && grepl("^\\d", tokens[3])) {
    col_type <- paste0(col_type, tokens[3])
  }

  rest <- paste(tokens[-(1:2)], collapse = " ")
  upper_rest <- toupper(rest)
  upper_line  <- toupper(line)

  nullable <- !grepl("NOT\\s+NULL", upper_line)
  is_pk    <- grepl("PRIMARY\\s+KEY", upper_line)

  # Inline FK
  fk_to <- NULL
  fk_match <- regmatches(line, regexpr("REFERENCES\\s+([`\"\\[\\]\\w\\.]+)", line, ignore.case = TRUE, perl = TRUE))
  if (length(fk_match) > 0) {
    fk_to <- trimws(gsub("[`\"\\[\\]]", "",
      sub("^[^.]+\\.", "", sub("REFERENCES\\s+", "", fk_match[1], ignore.case = TRUE, perl = TRUE))
    ))
  }

  list(
    name     = col_name,
    type     = normalise_type(col_type),
    raw_type = col_type,
    nullable = nullable,
    pk       = is_pk,
    fk_to    = fk_to
  )
}

# Split column definitions by top-level commas (respecting parentheses nesting)
.split_column_defs <- function(text) {
  chars  <- strsplit(text, "")[[1]]
  depth  <- 0L
  start  <- 1L
  parts  <- character(0)

  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- depth - 1L
    else if (ch == "," && depth == 0L) {
      parts <- c(parts, paste(chars[start:(i-1)], collapse = ""))
      start <- i + 1L
    }
  }
  if (start <= length(chars)) {
    parts <- c(parts, paste(chars[start:length(chars)], collapse = ""))
  }
  parts
}
