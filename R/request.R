#' Retrieve data from the BillTrack50 API
#'
#' @param path The specific endpoint path to request data from
#' @param format The format in which the data should be returned (default is "json")
#' @param api_key The API key required to access the BillTrack50 API (default is retrieved from environment variable 'BILLTRACK50_API_KEY')
#'
#' @return A list containing the data retrieved from the API
#'
#' @examples
#' billtrack_req("sessions/CA")
#' billtrack_req("bills/?searchText=education&stateCodes=CA&sessionID=12345")
#' billtrack_req("legislators?legislatorName=John%20Doe&stateCodes=CA")
#'
#' @export
billtrack_req <- function(
  path,
  format = "json",
  api_key = Sys.getenv("BILLTRACK50_API_KEY")
) {
  if (stringr::str_length(api_key) < 1) {
    rlang::abort("No BillTrack50 API key found in environment variable 'BILLTRACK50_API_KEY'")
  }

  str_to_url <- function(str) {
    str <- gsub(" ", "%20", str)
    str <- gsub(",", "%2C", str)
    return(str)
  }

  req <- httr2::request(glue::glue("https://www.billtrack50.com/bt50api/2.1/{format}")) |>
    httr2::req_url_path_append(str_to_url(path)) |>
    httr2::req_headers(Authorization = glue::glue("apikey {api_key}"))

  logger::log_debug("{req}")

  res <- httr2::req_perform(req)

  body <- res |>
    httr2::resp_body_json(simplifyVector = TRUE)

  return(body)
}
