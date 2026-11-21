#' Parse schema from a live DBI database connection
#'
#' @param connection A DBI connection object
#' @param schemas Character vector of schema names to filter (NULL = all)
#' @param tables Character vector of table names to filter (NULL = all)
#' @return A DiscoverySource object
#' @export
parse_db_schema <- function(connection, schemas = NULL, tables = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    rlang::abort("Package 'DBI' is required for parse_db_schema(). Install it with install.packages('DBI').")
  }

  all_tables <- DBI::dbListTables(connection)

  if (!is.null(tables)) {
    all_tables <- intersect(all_tables, tables)
  }

  parsed_tables <- lapply(all_tables, function(tbl_name) {
    fields <- tryCatch(
      DBI::dbListFields(connection, tbl_name),
      error = function(e) character(0)
    )

    # Try to get column type info from information_schema
    col_info <- tryCatch({
      query <- sprintf(
        "SELECT column_name, data_type, is_nullable FROM information_schema.columns WHERE table_name = '%s'",
        tbl_name
      )
      DBI::dbGetQuery(connection, query)
    }, error = function(e) NULL)

    columns <- lapply(fields, function(field_name) {
      raw_type <- if (!is.null(col_info) && field_name %in% col_info$column_name) {
        col_info$data_type[col_info$column_name == field_name][1]
      } else "string"

      nullable <- if (!is.null(col_info) && field_name %in% col_info$column_name) {
        col_info$is_nullable[col_info$column_name == field_name][1] != "NO"
      } else TRUE

      list(
        name     = field_name,
        type     = normalise_type(raw_type),
        raw_type = raw_type,
        nullable = nullable,
        pk       = FALSE,
        fk_to    = NULL
      )
    })

    list(name = tbl_name, columns = columns)
  })

  # Build raw text description for LLM
  raw_text <- paste(
    lapply(parsed_tables, function(tbl) {
      col_strs <- sapply(tbl$columns, function(col) {
        sprintf("  %s %s%s", col$name, col$raw_type, if (col$nullable) "" else " NOT NULL")
      })
      sprintf("TABLE %s (\n%s\n)", tbl$name, paste(col_strs, collapse = ",\n"))
    }),
    collapse = "\n\n"
  )

  db_info <- tryCatch(DBI::dbGetInfo(connection), error = function(e) list())

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = "db_schema",
      source_label = paste0("db:", db_info$dbname %||% "database"),
      raw_text     = raw_text,
      structured   = list(tables = parsed_tables),
      metadata     = list(
        dbname   = db_info$dbname %||% NA_character_,
        n_tables = length(parsed_tables)
      )
    ),
    class = c("DiscoverySource", "list")
  )
}
