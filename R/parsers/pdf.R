#' Parse a PDF file as a discovery source
#'
#' @param path Path to the PDF file
#' @return A DiscoverySource object
#' @export
parse_pdf <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    rlang::abort("Package 'pdftools' is required for parse_pdf(). Install it with install.packages('pdftools').")
  }
  if (!file.exists(path)) rlang::abort(paste0("File not found: ", path))

  pages <- pdftools::pdf_text(path)
  n_pages <- length(pages)

  raw_text <- paste(
    mapply(function(page_text, i) {
      paste0("--- Page ", i, " ---\n", page_text)
    }, pages, seq_along(pages)),
    collapse = "\n"
  )

  raw_text <- truncate_text(raw_text, max_chars = 50000L, source_label = basename(path))

  structure(
    list(
      source_id    = new_uuid(),
      source_type  = "pdf",
      source_label = basename(path),
      raw_text     = raw_text,
      structured   = NULL,
      metadata     = list(path = path, n_pages = n_pages)
    ),
    class = c("DiscoverySource", "list")
  )
}
