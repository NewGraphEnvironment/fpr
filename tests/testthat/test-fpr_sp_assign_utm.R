# this is actually also testing if the bcfishpass.crossings_vw layer utms are correct in the database
testthat::test_that("derives UTMs equivalent", {
# this has warnings suppressed so we only test if the utms derived match those in the database
  suppressWarnings(
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
    0))
})

sf_df <- data.frame(
   lon = c(-128.5, -123.5, -118.5),  # Longitudes for UTM zones 9, 10, and 11
   lat = c(45.5, 45.5, 45.5)  # Same latitude for simplicity
 ) %>%
   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

testthat::test_that("warning is thrown if col_easting and col_northing are not present in the dataframe", {
  testthat::expect_warning(fpr::fpr_sp_assign_utm(dat = sf_df))
})
