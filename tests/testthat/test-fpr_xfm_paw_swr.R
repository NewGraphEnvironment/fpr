path <- system.file("extdata", "pscis_phase1.xlsm", package = "fpr")

xl_raw <- fpr::fpr_import_pscis(dir_root = fs::path_dir(path)) |>
  dplyr::mutate(swr = stream_width_ratio)

# all these df passed the "fpr_xfm_paw_swr calculates stream_width_ratio correctly" test
# xl_raw <- fpr::fpr_import_pscis(dir_root = "~/Projects/repo/fish_passage_peace_2023_reporting/data/") |>
#   dplyr::mutate(swr = stream_width_ratio)
#
# xl_raw <- fpr::fpr_import_pscis(dir_root = "~/Projects/repo/fish_passage_skeena_2021_reporting/data/") |>
#   dplyr::mutate(swr = stream_width_ratio)


result <- fpr_xfm_paw_swr(xl_raw) |>
  dplyr::mutate(chk = swr - stream_width_ratio)

test_that("fpr_xfm_paw_swr calculates stream_width_ratio correctly", {

  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk == 0))
})

test_that("fpr_xfm_paw_swr throws an error for missing required columns", {
  dat_no_col <- xl_raw |>
    dplyr::select(-diameter_or_span_meters)
  # Expect error if required columns are missing
  expect_error(
    fpr_xfm_paw_swr(dat_no_col),
    "The following required columns are missing from the dataframe: diameter_or_span_meters"
  )
})
