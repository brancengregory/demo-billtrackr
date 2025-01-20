test_that("vote_details() returns correct structure", {
  skip_if_no_api_key()

  # Get a real vote ID from a bill's votes first
  bills_result <- bills(state_codes = "NY", search_text = "education")
  test_bill_id <- bills_result$billID[1]
  votes <- bill_votes(test_bill_id)
  test_vote_id <- votes$voteID[1]

  result <- vote_details(test_vote_id)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c(
    "voteID", "chamber", "motion",
    "yesVotes", "noVotes", "voteDate"
  ) %in% names(result)))
})
