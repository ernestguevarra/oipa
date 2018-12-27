################################################################################
#
#' get_datasets
#'
#' Function to get datasets endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI datasets, this can be
#'     specified as `name`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on datasets from IATI-compliant datasets.
#'
#' @examples
#' get_datasets(url = "https://devtracker.dfid.gov.uk",
#'              query = "name",
#'              value = "6236")
#'
#' @export
#'
#
################################################################################

get_datasets <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/datasets/",
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
      path = "/api/datasets/",
      response = resp
    ),
    class = "oipa_api"
  )
}



