skip_if_no_api_key <- function() {
  if (Sys.getenv("BILLTRACK50_API_KEY") == "") {
    skip("No API key available")
  }
}
