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
  )

result <- result_prep |>
  fpr_xfm_paw_score_cv_len() |>
  fpr_xfm_paw_swr() |>
  fpr_xfm_paw_score_embed(col_embed_score = "col_embed_score_r") |>
  dplyr::mutate(
    chk_stream_width_ratio = stream_width_ratio_raw - stream_width_ratio,
    chk_culvert_length_score = culvert_length_score_raw - culvert_length_score,
    chk_embed_score = col_embed_score_r - embed_score
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
  result <- result_prep |>
    fpr_xfm_paw_score_embed(col_embed_score = "col_embed_score_fpr") |>
    dplyr::mutate(
      chk_score = col_embed_score_fpr - embed_score
    )
  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_score == 0))
})

test_that("fpr_xfm_paw_score_outlet_drop calculates stream_width_ratio correctly", {
  result <- result_prep |>
    fpr_xfm_paw_score_outlet_drop(col_outlet_drop_score = outlet_drop_score_fpr) |>
    dplyr::mutate(
      chk_score = outlet_drop_score_fpr - outlet_drop_score
    )
  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_score == 0))
})

test_that("fpr_xfm_paw_score_cv_slope calculates stream_width_ratio correctly", {
  result <- result_prep |>
    fpr_xfm_paw_score_cv_slope(col_culvert_slope_score = culvert_slope_score_fpr) |>
    dplyr::mutate(
      chk_score = culvert_slope_score_fpr - culvert_slope_score
    )
  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_score == 0))
})

test_that("fpr_xfm_paw_score_swr calculates stream_width_ratio correctly", {
  result <- result_prep |>
    fpr_xfm_paw_score_swr(col_stream_width_ratio_score = stream_width_ratio_score_fpr) |>
    dplyr::mutate(
      chk_score = stream_width_ratio_score_fpr - stream_width_ratio_score
    )
  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_score == 0))
})

test_that("fpr_xfm_paw_score_final calculates stream_width_ratio correctly", {
  result <- result_prep |>
    fpr_xfm_paw_score_final(col_final_score = final_score_fpr) |>
    dplyr::mutate(
      chk_score = final_score_fpr - final_score
    )
  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk_score == 0))
})


