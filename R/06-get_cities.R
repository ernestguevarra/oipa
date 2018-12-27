################################################################################
#
#' get_cities
#'
#' Function to get cities endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI activites, this can be
#'     specified as `city_id`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on cities from IATI-compliant datasets.
#'
#' @examples
#' get_cities(url = "https://devtracker.dfid.gov.uk",
#'            query = "city_id",
#'            value = "3443013")
#'
#' @export
#'
#
################################################################################

get_cities <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/cities/",
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
      path = "/api/cities/",
      response = resp
    ),
    class = "oipa_api"
  )
}
