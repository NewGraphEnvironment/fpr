  test_that("fpr_t_site_id passes with valid input", {

    dat_pass <- data.frame(
      pscis_crossing_id = c(1, 2, NA, 4),
      my_crossing_reference = c(NA, NA, 3, NA),
      date_time_start = Sys.time() + 0:3
    )
    expect_silent(fpr_t_site_id(dat_pass))
  })

  test_that("fpr_t_site_id fails with invalid input", {

    dat_fail <- data.frame(
      pscis_crossing_id = c(1, 2, NA, 4),
      my_crossing_reference = c(1, NA, 3, 4),
      date_time_start = Sys.time() + 0:3
    )
    expect_error(fpr_t_site_id(dat_fail))
  })
