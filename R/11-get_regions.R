################################################################################
#
#' get_regions
#'
#' Function to get regions endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI regions, this can be
#'     specified as `region_id`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on regions from IATI-compliant datasets.
#'
#' @examples
#' get_regions(url = "https://devtracker.dfid.gov.uk",
#'             query = "region_id",
#'             value = "189")
#'
#' @export
#'
#
################################################################################

get_regions <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/regions/",
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
      path = "/api/regions/",
      response = resp
    ),
    class = "oipa_api"
  )
}



