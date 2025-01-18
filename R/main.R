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

sessions <- function(state = NULL) {
  sessions <- billtrack_req(glue::glue("sessions/{state}")) |>
    purrr::pluck("sessions")

  return(sessions)
}

bills <- function(search_text = NULL, state_codes = NULL, session_id = NULL) {
  bills <- billtrack_req(glue::glue("bills/?searchText={search_text}&stateCodes={state_codes}&sessionID={session_id}"))

  return(bills)
}

legislators <- function(legislator_name, state_codes) {
  legislators <- billtrack_req(glue::glue("legislators?legislatorName={legislator_name}&stateCodes={state_codes}")) |>
    purrr::pluck("legislators") |>
    tibble::as_tibble()

  return(legislators)
}

