# path <- system.file("extdata", "pscis_phase1.xlsm", package = "fpr")

paths <- c(
  "~/Projects/repo/fpr/inst/extdata/pscis_phase1.xlsm",
  "~/Projects/repo/fpr/inst/extdata/pscis_phase1_peace_2023.xlsm",
  "~/Projects/repo/fpr/inst/extdata/pscis_phase1_skeena_2021.xlsm",
  "~/Projects/repo/fpr/inst/extdata/pscis_phase2_skeena_2021.xlsm"
)

xl_raw <- paths |>
  purrr::map(
    ~ fpr::fpr_import_pscis(
      dir_root = fs::path_dir(.x)
    ) |>
      dplyr::mutate(filename = basename(.x)) # Add a source column with file name
  ) |>
  dplyr::bind_rows()


result_prep <- xl_raw |>
  dplyr::mutate(
    stream_width_ratio_raw = stream_width_ratio,
    culvert_length_score_raw = culvert_length_score,
    embed_score_raw = embed_score,
    outlet_drop_score_raw = outlet_drop_score,
    culvert_slope_score_raw = culvert_slope_score,
    stream_width_ratio_score_raw = stream_width_ratio_score,
    final_score_raw = final_score,
    barrier_result_raw = barrier_result
  )

result <- result_prep |>
  fpr_xfm_paw_score_cv_len() |>
  fpr_xfm_paw_swr() |>
  fpr_xfm_paw_score_embed() |>
  dplyr::mutate(
    chk_stream_width_ratio = stream_width_ratio_raw - stream_width_ratio,
    chk_culvert_length_score = culvert_length_score_raw - culvert_length_score,
    chk_embed_score = embed_score_raw - embed_score
  )


test_that("fpr_xfm_paw_score_cv_len calculates stream_width_ratio correctly", {

  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_culvert_length_score == 0))
})

test_that("fpr_xfm_paw_swr calculates stream_width_ratio correctly", {

  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_stream_width_ratio == 0))
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

test_that("fpr_xfm_paw_score_embed calculates stream_width_ratio correctly", {

  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_embed_score == 0))
})
