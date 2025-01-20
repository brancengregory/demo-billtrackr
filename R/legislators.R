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

#' Get detailed information about a specific legislator
#'
#' This function retrieves detailed information about a legislator by their ID.
#'
#' @param legislator_id The unique identifier of the legislator
#' @return A tibble containing detailed legislator information
#'
#' @examples
#' legislator_details(18718)
#'
#' @export
legislator_details <- function(legislator_id) {
  legislator <- billtrack_req(glue::glue("legislators/{legislator_id}")) |>
    purrr::pluck("legislator") |>
    tibble::as_tibble()

  return(legislator)
}

#' Get bills sponsored by a specific legislator
#'
#' This function retrieves all bills sponsored by a specific legislator.
#'
#' @param legislator_id The unique identifier of the legislator
#' @return A tibble containing bills sponsored by the legislator
#'
#' @examples
#' legislator_bills(18718)
#'
#' @export
legislator_bills <- function(legislator_id) {
  bills <- billtrack_req(glue::glue("legislators/{legislator_id}/bills")) |>
    purrr::pluck("sponsoredBill") |>
    tibble::as_tibble()

  return(bills)
}

#' Get voting history for a specific legislator
#'
#' This function retrieves all votes cast by a specific legislator.
#'
#' @param legislator_id The unique identifier of the legislator
#' @return A tibble containing the legislator's voting history
#'
#' @examples
#' legislator_votes(18718)
#'
#' @export
legislator_votes <- function(legislator_id) {
  votes <- billtrack_req(glue::glue("legislators/{legislator_id}/votes")) |>
    purrr::pluck("legislatorVotes") |>
    tibble::as_tibble()

  return(votes)
}
