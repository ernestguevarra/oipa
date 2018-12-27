################################################################################
#
#' get_budgets
#'
#' Function to get budgets endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI budgets, this can be
#'     `iati_identifier`, `title`, `description`, `recipient_country`,
#'     `recipient_region`, `recipient_org`, `sector`, `document_link`,
#'     `participating_org`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on budgets from IATI-compliant datasets.
#'
#' @examples
#' get_budgets(url = "https://devtracker.dfid.gov.uk",
#'             query = "iati_identifier",
#'             value = "075004")
#'
#' @export
#'
#
################################################################################

get_budgets <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/budgets/aggregations/",
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
      path = "/api/budgets/aggregations",
      response = resp
    ),
    class = "oipa_api"
  )
}



