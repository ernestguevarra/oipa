################################################################################
#
#' get_countries
#'
#' Function to get activities endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI countries, this can be
#'     specified as `country_id`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on countries from IATI-compliant datasets.
#'
#' @examples
#' get_countries(url = "https://devtracker.dfid.gov.uk",
#'               query = "country_id",
#'               value = "AC")
#'
#' @export
#'
#
################################################################################

get_countries <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/countries/",
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
      path = "/api/countries/",
      response = resp
    ),
    class = "oipa_api"
  )
}



