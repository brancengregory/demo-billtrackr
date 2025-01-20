
test_that("bills() returns expected structure", {
  skip_if_no_api_key()

  result <- bills(search_text = "education", state_codes = "OK", session_id = 855)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("billID", "stateBillID", "stateCode", "billName") %in% names(result)))
})

test_that("bill_details() returns expected structure", {
  skip_if_no_api_key()

  # Assuming a valid bill ID - you'll need to replace with a real one
  test_bill_id <- 1656104
  result <- bill_details(test_bill_id)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("billID", "stateBillID", "stateCode", "billName", "summary") %in% names(result)))
})

test_that("bill_sponsors() returns expected structure", {
  skip_if_no_api_key()

  test_bill_id <- 1656104
  result <- bill_sponsors(test_bill_id)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("legislatorID", "name", "legislatorParty", "state") %in% names(result)))
})

test_that("bill_votes() returns expected structure", {
  skip_if_no_api_key()

  test_bill_id <- 1656104
  result <- bill_votes(test_bill_id)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("voteID", "chamber", "motion", "yesVotes", "noVotes") %in% names(result)))
})

test_that("bill_action_history() returns expected structure", {
  skip_if_no_api_key()

  test_bill_id <- 1656104
  result <- bill_action_history(test_bill_id)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("chamberCode", "actionDescription", "actionDate") %in% names(result)))
})

test_that("bill_events() returns expected structure", {
  skip_if_no_api_key()

  test_bill_id <- 1656104
  result <- bill_events(test_bill_id)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("date", "time", "type", "location", "description") %in% names(result)))
})

test_that("vote_details() returns expected structure", {
  skip_if_no_api_key()

  test_vote_id <- 67890
  result <- vote_details(test_vote_id)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("voteID", "chamber", "motion", "yesVotes", "noVotes") %in% names(result)))
})

test_that("bill_sheets() returns expected structure", {
  skip_if_no_api_key()

  result <- bill_sheets()

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("billSheetID", "billSheetName") %in% names(result)))
})

test_that("bill_sheet_bills() returns expected structure", {
  skip_if_no_api_key()

  # This is highly dependent on a specific bill sheet so if it's deleted find another we won't delete.
  test_sheet_id <- 45564
  result <- bill_sheet_bills(test_sheet_id)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("billID", "stateBillID", "stateCode", "billName") %in% names(result)))
})

test_that("state_bill_progress() returns expected structure", {
  skip_if_no_api_key()

  result <- state_bill_progress("OK")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("stateCode", "totalBills", "introduced", "inCommittee") %in% names(result)))
})
