
<<<<<<< HEAD
=======
test_that("legislator_details() returns correct structure", {
  skip_if_no_api_key()

  # Get a real legislator ID from a bill's sponsors first
  bills_result <- bills(state_codes = "NY", search_text = "education")
  test_bill_id <- bills_result$billID[1]
  sponsors <- bill_sponsors(test_bill_id)
  test_legislator_id <- sponsors$legislatorID[1]

  result <- legislator_details(test_legislator_id)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c(
    "legislatorID", "name", "party",
    "stateCode", "role", "district"
  ) %in% names(result)))
})

test_that("legislator_bills() returns correct structure", {
  skip_if_no_api_key()

  # Get a real legislator ID from a bill's sponsors first
  bills_result <- bills(state_codes = "NY", search_text = "education")
  test_bill_id <- bills_result$billID[1]
  sponsors <- bill_sponsors(test_bill_id)
  test_legislator_id <- sponsors$legislatorID[1]

  result <- legislator_bills(test_legislator_id)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c(
    "legislatorID", "legislatorName",
    "sponsorType", "billID", "stateBillID"
  ) %in% names(result)))
})

test_that("legislator_votes() returns correct structure", {
  skip_if_no_api_key()

  # Get a real legislator ID from a bill's sponsors first
  bills_result <- bills(state_codes = "NY", search_text = "education")
  test_bill_id <- bills_result$billID[1]
  sponsors <- bill_sponsors(test_bill_id)
  test_legislator_id <- sponsors$legislatorID[1]

  result <- legislator_votes(test_legislator_id)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c(
    "billID", "stateCode", "stateBillID",
    "voteID", "motion", "vote"
  ) %in% names(result)))
})
>>>>>>> Snippet
