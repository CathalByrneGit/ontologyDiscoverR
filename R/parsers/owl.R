#' Parse an OWL/RDF ontology file
#'
#' @param path Path to the OWL/RDF file (XML or Turtle format)
#' @return A DiscoverySource object
#' @export
parse_owl <- function(path) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    rlang::abort("Package 'xml2' is required for parse_owl(). Install it with install.packages('xml2').")
  }
  if (!file.exists(path)) rlang::abort(paste0("File not found: ", path))

  raw_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  ext <- tolower(tools::file_ext(path))

  structured <- if (ext %in% c("owl", "rdf", "xml")) {
    tryCatch(.parse_owl_xml(path), error = function(e) {
      rlang::warn(paste0("Could not parse OWL XML: ", conditionMessage(e)))
      NULL
    })
  } else {
    # Turtle or other format — pass raw text to LLM
    NULL
  }

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = "owl_rdf",
      source_label = basename(path),
      raw_text     = raw_text,
      structured   = structured,
      metadata     = list(path = path, format = ext)
    ),
    class = c("DiscoverySource", "list")
  )
}

.parse_owl_xml <- function(path) {
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)

  # OWL classes → object types
  classes <- xml2::xml_find_all(doc, "//owl:Class", ns)
  class_items <- lapply(classes, function(node) {
    about <- xml2::xml_attr(node, "about") %||% xml2::xml_attr(node, "ID")
    label_node <- xml2::xml_find_first(node, ".//rdfs:label", ns)
    label <- if (!inherits(label_node, "xml_missing")) xml2::xml_text(label_node) else basename(about %||% "")
    list(name = label, about = about %||% "", columns = list())
  })

  # Object properties → link types
  obj_props <- xml2::xml_find_all(doc, "//owl:ObjectProperty", ns)
  link_items <- lapply(obj_props, function(node) {
    about <- xml2::xml_attr(node, "about") %||% ""
    domain_node <- xml2::xml_find_first(node, ".//rdfs:domain", ns)
    range_node  <- xml2::xml_find_first(node, ".//rdfs:range", ns)
    list(
      name   = basename(about),
      about  = about,
      domain = if (!inherits(domain_node, "xml_missing")) xml2::xml_attr(domain_node, "resource") else NULL,
      range  = if (!inherits(range_node,  "xml_missing")) xml2::xml_attr(range_node,  "resource") else NULL
    )
  })

  # Data properties → property hints
  data_props <- xml2::xml_find_all(doc, "//owl:DatatypeProperty", ns)
  data_items <- lapply(data_props, function(node) {
    about <- xml2::xml_attr(node, "about") %||% ""
    list(name = basename(about), about = about)
  })

  list(
    tables      = class_items,
    link_hints  = link_items,
    data_props  = data_items
  )
}
