testthat::test_that("derives UTMs equivalent", {
  testthat::expect_equal(nrow({

    d <- fpr_db_query(
      query = fpr::fpr_dbq_subset(
        num_rows = 100,
        randomize = FALSE))

    d2 <- fpr::fpr_sp_assign_utm(dat = d,
                                 col_zone = 'utm_zone2',
                                 col_easting = 'utm_easting2',
                                 col_northing = 'utm_northing2')
    d3 <-  d2 %>%
      dplyr::select(contains(c('zone', 'easting', 'northing'))) %>%
      dplyr::mutate(chk_east = abs(utm_easting - utm_easting2),
                    chk_north = abs(utm_northing - utm_northing2),
                    chk_zone = abs(utm_zone - utm_zone2)) %>%
      dplyr::filter(dplyr::if_any(c(chk_east, chk_north), ~ .x >= 3) | chk_zone >= 1)

    d3}),
    0)
})
