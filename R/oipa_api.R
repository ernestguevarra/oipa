################################################################################
#
#' get_oipa_activities
#'
#' Function to get activities from IATI-compliant sites.
#'
#' @param base Base URL
#' @param iati_identifier IATI unique identifier
#' @param query Query to pass to API
#'
#' @return Parsed content
#'
#' @examples
#' #
#'
#' @export
#'
#
################################################################################

get_oipa_activities <- function(base, iati_identifier, query = NULL) {
  url <- httr::modify_url(url = base, path = paste("/api/activities/", iati_identifier, sep = ""), query = query)

  resp <- httr::GET(url)

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  structure(
    list(
      content = parsed,
      path = "/api/activities/",
      response = resp
    ),
    class = "oipa_api"
  )
}



