################################################################################
#
#' get_transactions
#'
#' Function to get transactions endpoint from IATI-compliant datasets.
#'
#' @param url URL for websites containing IATI-compliant datasets.
#' @param query Search/query parameter. For IATI activites, this can be
#'     `description`, `provider_organisation_name`, `receiver_organisation_name`.
#' @param value Value to pass to specified `query`.
#'
#' @return Parsed content on transactions from IATI-compliant datasets.
#'
#' @examples
#' get_transactions(url = "https://devtracker.dfid.gov.uk",
#'                  query = "transaction_id",
#'                  value = "270304")
#'
#' @export
#'
#
################################################################################

get_transactions <- function(url, query, value) {
  url <- paste(httr::modify_url(url = url,
                                path = "/api/transactions/",
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
      path = "/api/transactions/",
      response = resp
    ),
    class = "oipa_api"
  )
}



