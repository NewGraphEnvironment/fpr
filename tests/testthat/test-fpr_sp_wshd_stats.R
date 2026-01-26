testthat::test_that("fpr_sp_wshd_stats returns expected columns", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  lon <- c(
    -126.851692669,
    -126.800268492,
    -126.784708983,
    -126.782603083,
    -126.80687253,
    -126.851692669
  )
  lat <- c(
    54.093073557,
    54.125492344,
    54.121174443,
    54.109790451,
    54.095312235,
    54.093073557
  )
  coords <- cbind(lon, lat)
  wshd <- sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)

  dat <- sf::st_sf(
    area_ha = 792.42,
    stream_crossing_id = 1,
    geometry = wshd
  )

  out <- fpr::fpr_sp_wshd_stats(dat)

  testthat::expect_s3_class(out, "sf")
  testthat::expect_true(
    all(
      c(
        "elev_min",
        "elev_max",
        "elev_median",
        "elev_p60",
        "aspect",
        "area_km"
      ) %in% names(out)
    )
  )
  testthat::expect_equal(out$stream_crossing_id, 1)
  testthat::expect_false(is.na(out$elev_median))
})
