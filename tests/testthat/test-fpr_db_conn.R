# this on allows some writing so
# testthat::test_that("connection is successful", {
#   testthat::expect_equal(
#     class(fpr_db_conn())[1],
#     "PqConnection"
#   )
# })

testthat::test_that("connection is successful", {
  testthat::expect_equal(
    class(fpr_db_conn(user_var = Sys.getenv('PG_USER_SHARE'),
                      password_var = Sys.getenv('PG_PASS_SHARE')))[1],
    "PqConnection"
  )
})

test_that("connection unsuccessful fails", {
  expect_error(fpr_db_conn(db_var = 'wrong'))
})
