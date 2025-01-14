path <- system.file("extdata", "pscis_phase1.xlsm", package = "fpr")

xl_raw <- fpr::fpr_import_pscis(dir_root = fs::path_dir(path)) |>
  dplyr::mutate(culvert_length_score_raw = culvert_length_score)

# # all these df passed the "fpr_xfm_paw_swr calculates stream_width_ratio correctly" test
# xl_raw <- fpr::fpr_import_pscis(dir_root = "~/Projects/repo/fish_passage_peace_2023_reporting/data/") |>
#   dplyr::mutate(culvert_length_score_raw = culvert_length_score)
#
# xl_raw <- fpr::fpr_import_pscis(dir_root = "~/Projects/repo/fish_passage_skeena_2021_reporting/data/") |>
#   dplyr::mutate(culvert_length_score_raw = culvert_length_score)

result <- fpr_xfm_paw_score_cv_len(xl_raw) |>
  dplyr::mutate(chk = culvert_length_score_raw - culvert_length_score)

test_that("fpr_xfm_paw_score_cv_len calculates stream_width_ratio correctly", {

  # Check if all calculated values match expected `swr`
  expect_true(all(result$chk == 0))
})
