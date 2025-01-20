
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
