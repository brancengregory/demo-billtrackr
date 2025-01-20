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

  req <- httr2::request(glue::glue("https://www.billtrack50.com/bt50api/2.1/{format}")) |>
    httr2::req_url_path_append(path) |>
    httr2::req_headers(Authorization = glue::glue("apikey {api_key}"))

  logger::log_debug("{req}")

  res <- httr2::req_perform(req)

  body <- res |>
    httr2::resp_body_json(simplifyVector = TRUE)

  return(body)
}

#' Get information about sessions
#'
#' This function retrieves information about sessions based on the specified state.
#'
#' @param state The state for which to retrieve session information
#' @return A list of sessions
#'
#' @examples
#' sessions("NY")
#'
#' @export
sessions <- function(state = NULL) {
  sessions <- billtrack_req(glue::glue("sessions/{state}")) |>
    purrr::pluck("sessions") |>
    tibble::as_tibble()

  return(sessions)
}

#' Get information about bills
#'
#' This function retrieves information about bills based on the specified search text, state codes, and session ID.
#'
#' @param search_text The text to search for in bill titles
#' @param state_codes The state codes to filter bills by
#' @param session_id The session ID to filter bills by
#' @return A list of bills
#'
#' @examples
#' bills(search_text = "education", state_codes = "NY", session_id = 12345)
#'
#' @export
bills <- function(search_text = NULL, state_codes = NULL, session_id = NULL) {
  bills <- billtrack_req(glue::glue("bills/?searchText={search_text}&stateCodes={state_codes}&sessionID={session_id}")) |>
    purrr::pluck("bills") |>
    tibble::as_tibble()

  return(bills)
}

#' Get information about legislators
#'
#' This function retrieves information about legislators based on the specified legislator name and state codes.
#'
#' @param legislator_name The name of the legislator to search for
#' @param state_codes The state codes to filter legislators by
#' @return A tibble of legislators
#'
#' @examples
#' legislators("John Doe", "NY")
#'
#' @export
legislators <- function(legislator_name, state_codes) {
  legislators <- billtrack_req(glue::glue("legislators?legislatorName={legislator_name}&stateCodes={state_codes}")) |>
    purrr::pluck("legislators") |>
    tibble::as_tibble()

  return(legislators)
}

#' Get information about bill sheets
#'
#' This function retrieves information about all bill sheets for the authenticated user.
#'
#' @return A tibble of bill sheets containing information like billSheetID, name, summary, counts, etc.
#'
#' @examples
#' bill_sheets()
#'
#' @export
bill_sheets <- function() {
  sheets <- billtrack_req("billSheets") |>
    purrr::pluck("billSheets") |>
    tibble::as_tibble()

  return(sheets)
}

#' Get bills from a specific bill sheet
#'
#' This function retrieves all bills contained in a specified bill sheet.
#'
#' @param bill_sheet_id The unique identifier of the bill sheet
#' @return A tibble of bills contained in the specified bill sheet
#'
#' @examples
#' bill_sheet_bills(12345)
#'
#' @export
bill_sheet_bills <- function(bill_sheet_id) {
  bills <- billtrack_req(glue::glue("billSheets/{bill_sheet_id}/bills")) |>
    purrr::pluck("bills") |>
    tibble::as_tibble()

  return(bills)
}

