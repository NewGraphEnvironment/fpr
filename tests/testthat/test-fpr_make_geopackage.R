# test_that("fpr_make_geopackage writes a .gpkg file", {
#   test_dir <- fs::path(tempdir(), "test_gpkg_dir")
#   if (!fs::dir_exists(test_dir)) fs::dir_create(test_dir)
#
  # dat <- data.frame(
  #   utm_zone = 10,
  #   utm_easting = c(610000, 611000),
  #   utm_northing = c(5600000, 5601000)
  # )
  #
  # fpr_make_geopackage(
  #   dat = dat,
  #   dir = test_dir,
  #   gpkg_name = "test_layer",
  #   col_easting = "utm_easting",
  #   col_northing = "utm_northing",
  #   crs_return = 4326
  # )
#
#   file_out <- fs::path(test_dir, "test_layer.gpkg")
#   expect_true(fs::file_exists(file_out))
# })


