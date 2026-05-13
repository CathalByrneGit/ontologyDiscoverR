#' Parse an OpenAPI 3.x or Swagger 2.x spec
#'
#' @param path_or_url File path or URL to a YAML or JSON OpenAPI spec
#' @return A DiscoverySource object
#' @export
parse_openapi <- function(path_or_url) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    rlang::abort("Package 'yaml' is required for parse_openapi(). Install it with install.packages('yaml').")
  }

  is_url <- grepl("^https?://", path_or_url)

  spec <- if (is_url) {
    resp <- httr2::req_perform(httr2::request(path_or_url))
    content_type <- httr2::resp_content_type(resp)
    text <- httr2::resp_body_string(resp)
    if (grepl("json", content_type, ignore.case = TRUE) || grepl("\\.json$", path_or_url)) {
      jsonlite::fromJSON(text, simplifyVector = FALSE)
    } else {
      yaml::read_yaml(text = text)
    }
  } else {
    if (!file.exists(path_or_url)) rlang::abort(paste0("File not found: ", path_or_url))
    ext <- tolower(tools::file_ext(path_or_url))
    if (ext == "json") {
      jsonlite::read_json(path_or_url, simplifyVector = FALSE)
    } else {
      yaml::read_yaml(path_or_url)
    }
  }

  # Extract components/schemas → object types
  schemas <- spec$components$schemas %||% spec$definitions %||% list()

  schema_items <- lapply(names(schemas), function(schema_name) {
    s <- schemas[[schema_name]]
    props <- s$properties %||% list()
    required_fields <- s$required %||% character(0)

    columns <- lapply(names(props), function(prop_name) {
      p <- props[[prop_name]]
      list(
        name     = prop_name,
        type     = normalise_type(p$type %||% "string"),
        raw_type = p$type %||% "string",
        nullable = !(prop_name %in% required_fields),
        pk       = FALSE,
        fk_to    = if (!is.null(p[["$ref"]])) {
          sub("^.*/", "", p[["$ref"]])
        } else NULL
      )
    })

    list(name = schema_name, columns = columns)
  })

  # Extract paths → actions
  paths <- spec$paths %||% list()
  action_hints <- lapply(names(paths), function(path_str) {
    methods <- paths[[path_str]]
    lapply(intersect(names(methods), c("post", "put", "delete", "patch")), function(method) {
      op <- methods[[method]]
      list(
        path        = path_str,
        method      = toupper(method),
        operation_id = op$operationId %||% paste(method, gsub("/", "_", path_str)),
        summary     = op$summary %||% ""
      )
    })
  })

  raw_text <- paste0(
    "OpenAPI Spec: ", spec$info$title %||% "Unknown", " v", spec$info$version %||% "?", "\n\n",
    "Schemas:\n",
    paste(sapply(names(schemas), function(n) {
      s <- schemas[[n]]
      props <- names(s$properties %||% list())
      sprintf("  %s: {%s}", n, paste(props, collapse = ", "))
    }), collapse = "\n"),
    "\n\nPaths:\n",
    paste(names(paths), collapse = "\n")
  )

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = "openapi",
      source_label = basename(path_or_url),
      raw_text     = raw_text,
      structured   = list(tables = schema_items, action_hints = unlist(action_hints, recursive = FALSE)),
      metadata     = list(
        title   = spec$info$title %||% NA_character_,
        version = spec$info$version %||% NA_character_
      )
    ),
    class = c("DiscoverySource", "list")
  )
}
