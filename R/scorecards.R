#' Get information about scorecards
#'
#' This function retrieves information about all scorecards for the authenticated user.
#'
#' @return A tibble of scorecards containing information like scorecardID, name, categories, etc.
#'
#' @examples
#' scorecards()
#'
#' @export
scorecards <- function() {
  scorecards <- billtrack_req("scorecards") |>
    purrr::pluck("scorecards") |>
    tibble::as_tibble()

  return(scorecards)
}

#' Get bills from a specific scorecard
#'
#' This function retrieves all bills contained in a specified scorecard.
#'
#' @param scorecard_id The unique identifier of the scorecard
#' @return A tibble of bills contained in the specified scorecard
#'
#' @examples
#' scorecard_bills(12345)
#'
#' @export
scorecard_bills <- function(scorecard_id) {
  bills <- billtrack_req(glue::glue("scorecards/{scorecard_id}/bills")) |>
    purrr::pluck("bills") |>
    tibble::as_tibble()

  return(bills)
}

#' Get legislator information for a specific scorecard
#'
#' This function retrieves information about a specific legislator in a scorecard.
#'
#' @param scorecard_id The unique identifier of the scorecard
#' @param legislator_id The unique identifier of the legislator
#' @return A tibble containing legislator information and category scores
#'
#' @examples
#' scorecard_legislator(12345, 18718)
#'
#' @export
scorecard_legislator <- function(scorecard_id, legislator_id) {
  legislator <- billtrack_req(glue::glue("scorecards/{scorecard_id}/legislators/{legislator_id}")) |>
    purrr::pluck("legislator") |>
    tibble::as_tibble()

  return(legislator)
}

#' Get legislator votes for a specific scorecard
#'
#' This function retrieves voting information for a specific legislator in a scorecard.
#'
#' @param scorecard_id The unique identifier of the scorecard
#' @param legislator_id The unique identifier of the legislator
#' @return A tibble containing voting information
#'
#' @examples
#' scorecard_legislator_votes(12345, 18718)
#'
#' @export
scorecard_legislator_votes <- function(scorecard_id, legislator_id) {
  votes <- billtrack_req(glue::glue("scorecards/{scorecard_id}/legislators/{legislator_id}/votes")) |>
    purrr::pluck("billVotes") |>
    tibble::as_tibble()

  return(votes)
}

#' Get all legislators for a specific scorecard
#'
#' This function retrieves information about all legislators in a scorecard.
#'
#' @param scorecard_id The unique identifier of the scorecard
#' @param category_id Optional category ID to filter legislation
#' @return A tibble containing legislator information
#'
#' @examples
#' scorecard_legislators(12345)
#' scorecard_legislators(12345, category_id = 67)
#'
#' @export

scorecard_legislators <- function(scorecard_id, category_id = NULL) {
  path <- if (is.null(category_id)) {
    glue::glue("scorecards/{scorecard_id}/legislators")
  } else {
    glue::glue("scorecards/{scorecard_id}/legislators?category={category_id}")
  }

  legislators <- billtrack_req(path) |>
    purrr::pluck("legislators") |>
    tibble::as_tibble()

  return(legislators)
}
