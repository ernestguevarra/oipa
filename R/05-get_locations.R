################################################################################
#
#' get_locations
#'
#' Function to get locations endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI activites, this can be
#'     specified as `activity_id`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on locations from IATI-compliant datasets.
#'
#' @examples
#' get_locations(url = "https://devtracker.dfid.gov.uk",
#'               query = "activity_id",
#'               value = "883116")
#'
#' @export
#'
#
################################################################################

get_locations <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/locations/",
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
      path = "/api/locations/",
      response = resp
    ),
    class = "oipa_api"
  )
}



