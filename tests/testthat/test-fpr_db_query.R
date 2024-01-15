test_that("query returns table", {
  expect_false(
    identical(
      dim(
        fpr_db_query()),
      as.integer(c(0,0))
    )
  )
})
