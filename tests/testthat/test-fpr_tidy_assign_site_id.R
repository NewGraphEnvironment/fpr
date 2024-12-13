dat_pass <- data.frame(
  pscis_crossing_id = c(1, 2, NA, 4),
  my_crossing_reference = c(NA, NA, 3, NA),
  date_time_start = Sys.time() + 0:3
)

dat_fail_dupes <- data.frame(
  pscis_crossing_id = c(100, NA, 100, NA),
  my_crossing_reference = c(NA, 4, NA, 4),
  date_time_start = Sys.time() + 0:3
)

test_that("fpr_tidy_assign_site_id passes with valid input", {


  expect_silent(fpr_tidy_assign_site_id(dat_pass))
})

test_that("fpr_tidy_assign_site_id fails with invalid input", {

  dat_fail <- data.frame(
    pscis_crossing_id = c(1, 2, NA, 4),
    my_crossing_reference = c(1, NA, 3, 4),
    date_time_start = Sys.time() + 0:3
  )
  expect_error(fpr_tidy_assign_site_id(dat_fail))
})


testthat::test_that("fpr_chk_dupes detects duplicates correctly", {
  # Case: Duplicates exist in the specified column
  expect_warning(
    fpr_chk_dupes(dat_fail_dupes, pscis_crossing_id)
    )
})


testthat::test_that("fpr_chk_dupes throws an error for an invalid column", {
  # Case: Invalid column name
  expect_error(
    fpr_chk_dupes(dat_fail_dupes, pscis_crossing_i)
  )
})

testthat::test_that("fpr_chk_dupes is silent when no ishy", {
  # Case: No duplicates in the specified column
  expect_silent(
    fpr_chk_dupes(dat_pass, pscis_crossing_id)
  )
})


