#' Parse a Markdown file or URL as a discovery source
#'
#' @param path_or_url File path or URL
#' @return A DiscoverySource object
#' @export
parse_markdown <- function(path_or_url) {
  text <- .read_text_source(path_or_url)
  headings <- .extract_md_headings(text)

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = "markdown",
      source_label = basename(path_or_url),
      raw_text     = text,
      structured   = list(headings = headings),
      metadata     = list(source = path_or_url, n_headings = length(headings))
    ),
    class = c("DiscoverySource", "list")
  )
}

#' Parse an HTML file or URL as a discovery source
#'
#' @param path_or_url File path or URL
#' @return A DiscoverySource object
#' @export
parse_html <- function(path_or_url) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    rlang::abort("Package 'xml2' is required for parse_html(). Install it with install.packages('xml2').")
  }

  is_url <- grepl("^https?://", path_or_url)
  doc <- if (is_url) {
    xml2::read_html(path_or_url)
  } else {
    if (!file.exists(path_or_url)) rlang::abort(paste0("File not found: ", path_or_url))
    xml2::read_html(path_or_url)
  }

  # Extract text from all text nodes
  text_nodes <- xml2::xml_find_all(doc, "//body//text()")
  text <- paste(trimws(xml2::xml_text(text_nodes)), collapse = " ")
  text <- gsub("\\s{2,}", " ", text)

  # Extract headings
  heading_nodes <- xml2::xml_find_all(doc, "//*[self::h1 or self::h2 or self::h3 or self::h4]")
  headings <- lapply(heading_nodes, function(node) {
    level <- as.integer(sub("h", "", xml2::xml_name(node)))
    list(level = level, text = trimws(xml2::xml_text(node)), content_preview = "")
  })

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = "html",
      source_label = basename(path_or_url),
      raw_text     = text,
      structured   = list(headings = headings),
      metadata     = list(source = path_or_url, n_headings = length(headings))
    ),
    class = c("DiscoverySource", "list")
  )
}

.read_text_source <- function(path_or_url) {
  if (grepl("^https?://", path_or_url)) {
    resp <- httr2::req_perform(httr2::request(path_or_url))
    httr2::resp_body_string(resp)
  } else {
    if (!file.exists(path_or_url)) rlang::abort(paste0("File not found: ", path_or_url))
    paste(readLines(path_or_url, warn = FALSE), collapse = "\n")
  }
}

.extract_md_headings <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  headings <- list()

  for (i in seq_along(lines)) {
    line <- lines[i]
    m <- regmatches(line, regexpr("^(#{1,6})\\s+(.+)$", line, perl = TRUE))
    if (length(m) > 0) {
      hashes <- regmatches(m[1], regexpr("^#{1,6}", m[1]))
      heading_text <- sub("^#{1,6}\\s+", "", m[1])
      # Preview: next few lines of content
      preview_lines <- lines[seq(min(i+1, length(lines)), min(i+3, length(lines)))]
      headings <- c(headings, list(list(
        level           = nchar(hashes),
        text            = heading_text,
        content_preview = paste(trimws(preview_lines), collapse = " ")
      )))
    }
  }
  headings
}
