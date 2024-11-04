#' Get total number of pages from website
#'
#' @return Total number of pages
#' @export
get_total_pages <- function() {
  # TODO: Implement real page count logic
  100
}

#' Format currency values
#'
#' @param x Numeric value
#' @return Formatted currency string
#' @export
format_currency <- function(x) {
  paste0("$", format(x, big.mark = ",", scientific = FALSE))
}

#' Format date values
#'
#' @param x Date or datetime value
#' @return Formatted date string
#' @export
format_date <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%S")
}

#' Clean text by removing extra whitespace
#'
#' @param x Character string
#' @return Cleaned string
#' @export
clean_text <- function(x) {
  gsub("\\s+", " ", trimws(x))
}

#' Safely extract text from HTML elements
#'
#' @param node HTML node
#' @param selector CSS selector
#' @return Extracted text or NA
#' @export
safe_extract <- function(node, selector) {
  tryCatch(
    {
      text <- node %>%
        html_nodes(selector) %>%
        html_text()
      
      if (length(text) == 0) NA else clean_text(text[1])
    },
    error = function(e) NA
  )
}