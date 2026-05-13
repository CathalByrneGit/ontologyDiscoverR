#' Parse a CSV file as a discovery source
#'
#' @param path Path to the CSV file
#' @param n_sample Number of rows to sample for type inference
#' @return A DiscoverySource object
#' @export
parse_csv <- function(path, n_sample = 100L) {
  if (!file.exists(path)) rlang::abort(paste0("File not found: ", path))

  df <- utils::read.csv(path, nrows = n_sample, stringsAsFactors = FALSE, check.names = FALSE)
  object_name <- tools::file_path_sans_ext(basename(path))

  .df_to_source(df, name = object_name, source_type = "csv",
                source_label = basename(path),
                metadata = list(path = path, n_rows_sampled = nrow(df)))
}

#' Parse an in-memory R data frame as a discovery source
#'
#' @param df An R data frame
#' @param name Name for the candidate object type (used as source_label)
#' @return A DiscoverySource object
#' @export
parse_r_dataframe <- function(df, name) {
  if (!is.data.frame(df)) rlang::abort("'df' must be a data.frame")
  .df_to_source(df, name = name, source_type = "r_dataframe",
                source_label = name,
                metadata = list(n_rows = nrow(df), n_cols = ncol(df)))
}

.df_to_source <- function(df, name, source_type, source_label, metadata = list()) {
  columns <- lapply(names(df), function(col_name) {
    vals <- df[[col_name]]
    raw_type <- .infer_r_type(vals)
    n_distinct <- length(unique(vals[!is.na(vals)]))
    sample_vals <- head(unique(vals[!is.na(vals)]), 5)

    list(
      name          = col_name,
      type          = normalise_type(raw_type),
      raw_type      = raw_type,
      nullable      = anyNA(vals),
      pk            = FALSE,
      fk_to         = NULL,
      n_distinct    = n_distinct,
      sample_values = as.character(sample_vals)
    )
  })

  str_capture <- utils::capture.output(utils::str(df))

  raw_text <- paste0(
    "Table: ", name, "\n",
    "Columns (", ncol(df), "):\n",
    paste(sapply(columns, function(col) {
      sprintf("  %s: %s (n_distinct=%d)", col$name, col$type, col$n_distinct)
    }), collapse = "\n"),
    "\n\nR structure:\n",
    paste(str_capture, collapse = "\n")
  )

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = source_type,
      source_label = source_label,
      raw_text     = raw_text,
      structured   = list(tables = list(list(name = name, columns = columns))),
      metadata     = metadata
    ),
    class = c("DiscoverySource", "list")
  )
}

.infer_r_type <- function(vals) {
  if (inherits(vals, "POSIXct") || inherits(vals, "POSIXlt")) return("datetime")
  if (inherits(vals, "Date")) return("date")
  if (is.logical(vals)) return("boolean")
  if (is.integer(vals)) return("integer")
  if (is.numeric(vals)) return("number")
  "string"
}
