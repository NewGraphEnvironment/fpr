testthat::test_that("connection is successful", {
  testthat::expect_equal(
    class(fpr_db_conn())[1],
    "PqConnection"
  )
})
