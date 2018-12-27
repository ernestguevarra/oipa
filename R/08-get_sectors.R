################################################################################
#
#' get_sectors
#'
#' Function to get sectors endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI sectors, this can be
#'     specified as `sector_id`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on sectors from IATI-compliant datasets.
#'
#' @examples
#' get_sectors(url = "https://devtracker.dfid.gov.uk",
#'             query = "sector_id",
#'             value = "11110")
#'
#' @export
#'
#
################################################################################

get_sectors <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/sectors/",
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
      path = "/api/sectors/",
      response = resp
    ),
    class = "oipa_api"
  )
}



