################################################################################
#
#' get_publishers
#'
#' Function to get publishers endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI publishers, this can be
#'     `id`, `publisher_iati_id`, `display_name`, `name`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on publishers from IATI-compliant datasets.
#'
#' @examples
#' get_publishers(url = "https://devtracker.dfid.gov.uk",
#'                query = "publisher_iati_id",
#'                value = "075004")
#'
#' @export
#'
#
################################################################################

get_publishers <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/publishers/",
                                query = query),
               paste("=", value, sep = ""), sep = "")

  resp <- httr::GET(url)

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(x = resp,
                                             as = "text",
                                             encoding = "UTF-8"),
                               simplifyVector = FALSE)

  structure(
    list(
      content = parsed,
      path = "/api/publishers/",
      response = resp
    ),
    class = "oipa_api"
  )
}
