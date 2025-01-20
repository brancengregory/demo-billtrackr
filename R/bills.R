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

#' Get detailed information about a specific bill
#'
#' This function retrieves detailed information about a bill by its ID.
#'
#' @param bill_id The unique identifier of the bill
#' @return A tibble containing detailed bill information
#'
#' @examples
#' bill_details(12345)
#'
#' @export
bill_details <- function(bill_id) {
  bill <- billtrack_req(glue::glue("bills/{bill_id}")) |>
    purrr::pluck("bill") |>
    tibble::as_tibble()

  return(bill)
}

#' Get sponsors of a specific bill
#'
#' This function retrieves all sponsors for a specific bill.
#'
#' @param bill_id The unique identifier of the bill
#' @return A tibble containing sponsor information
#'
#' @examples
#' bill_sponsors(12345)
#'
#' @export
bill_sponsors <- function(bill_id) {
  sponsors <- billtrack_req(glue::glue("bills/{bill_id}/sponsors")) |>
    purrr::pluck("sponsors") |>
    tibble::as_tibble()

  return(sponsors)
}

#' Get votes for a specific bill
#'
#' This function retrieves all votes for a specific bill.
#'
#' @param bill_id The unique identifier of the bill
#' @return A tibble containing voting information
#'
#' @examples
#' bill_votes(12345)
#'
#' @export
bill_votes <- function(bill_id) {
  votes <- billtrack_req(glue::glue("bills/{bill_id}/votes")) |>
    purrr::pluck("votes") |>
    tibble::as_tibble()

  return(votes)
}

#' Get action history for a specific bill
#'
#' This function retrieves the action history for a specific bill.
#'
#' @param bill_id The unique identifier of the bill
#' @return A tibble containing action history
#'
#' @examples
#' bill_action_history(12345)
#'
#' @export
bill_action_history <- function(bill_id) {
  actions <- billtrack_req(glue::glue("bills/{bill_id}/action-history")) |>
    purrr::pluck("actions") |>
    tibble::as_tibble()

  return(actions)
}

#' Get events for a specific bill
#'
#' This function retrieves all events for a specific bill.
#'
#' @param bill_id The unique identifier of the bill
#' @return A tibble containing event information
#'
#' @examples
#' bill_events(12345)
#'
#' @export
bill_events <- function(bill_id) {
  events <- billtrack_req(glue::glue("bills/{bill_id}/events")) |>
    purrr::pluck("events") |>
    tibble::as_tibble()

  return(events)
}

#' Get detailed information about a specific vote
#'
#' This function retrieves detailed information about a vote by its ID.
#'
#' @param vote_id The unique identifier of the vote
#' @return A tibble containing detailed vote information including legislator votes
#'
#' @examples
#' vote_details(67890)
#'
#' @export
vote_details <- function(vote_id) {
  vote <- billtrack_req(glue::glue("votes/{vote_id}")) |>
    purrr::pluck("vote") |>
    tibble::as_tibble()

  return(vote)
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

#' Get bill progress information by state
#'
#' This function retrieves information about bill progress in specified states.
#'
#' @param state_codes Optional state codes to filter progress information
#' @return A tibble containing bill progress information
#'
#' @examples
#' state_bill_progress()
#' state_bill_progress("CA")
#'
#' @export
state_bill_progress <- function(state_codes = NULL) {
  path <- if (is.null(state_codes)) {
    "state/bill-progress"
  } else {
    glue::glue("state/bill-progress?stateCode={state_codes}")
  }

  progress <- billtrack_req(path) |>
    purrr::pluck("billProgress") |>
    tibble::as_tibble()

  return(progress)
}
