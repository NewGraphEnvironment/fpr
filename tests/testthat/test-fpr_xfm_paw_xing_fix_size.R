
testthat::test_that("fpr_xfm_paw_xing_fix_size errors when required columns are missing", {
  dat <- base::data.frame(
    barrier_result = c("Barrier", "Potential"),
    # downstream_channel_width_meters missing
    fill_depth_meters = c(6, 2)
  )

  testthat::expect_error(
    fpr_xfm_paw_xing_fix_size(dat),
    "The following required columns are missing from the dataframe"
  )
})


testthat::test_that("fpr_xfm_paw_xing_fix_size errors when required non-numeric bridge_width is supplied", {
  dat <- base::data.frame(
    barrier_result = c("Barrier", "Potential"),
    downstream_channel_width_meters = c(3, 1),
    fill_depth_meters = c(6, 2)
  )

  testthat::expect_error(
    fpr_xfm_paw_xing_fix_size(dat, brdg_wdth = "hi"),
    "`brdg_wdth` must be a number (non-missing numeric scalar)."
  )
})




testthat::test_that("fpr_xfm_paw_xing_fix_size returns expected fixes for basic barrier cases", {
  dat <- base::data.frame(
    barrier_result = c("Barrier", "Potential", "Passable", "Unknown", "Other"),
    downstream_channel_width_meters = c(2.5, 3.0, 2.2, 1.8, 2.0),
    fill_depth_meters = c(4, 4, 4, 4, 4),
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(dat)

  testthat::expect_true("crossing_fix" %in% base::names(out))

  # Barrier/Potential with width ≥ 2m → open bottom structure
  testthat::expect_identical(
    out$crossing_fix[1:2],
    base::rep("Replace with New Open Bottom Structure", 2)
  )

  # Passable / Unknown → NA_character_
  testthat::expect_true(base::is.na(out$crossing_fix[3]))
  testthat::expect_true(base::is.na(out$crossing_fix[4]))

  # Other → streambed simulation
  testthat::expect_identical(
    out$crossing_fix[5],
    "Replace Structure with Streambed Simulation CBS"
  )
})

testthat::test_that("fpr_xfm_paw_xing_fix_size applies special streambed simulation case for narrow streams with deep fill", {
  dat <- base::data.frame(
    barrier_result = c(
      "Barrier",    # triggers special case
      "Potential",  # triggers special case
      "Barrier",    # width too small → base rule
      "Barrier"     # fill too shallow → base rule
    ),
    downstream_channel_width_meters = c(
      3.0,  # >2 and <=3.5
      2.1,  # >2 and <=3.5
      1.9,  # <=2
      3.0   # >2 and <=3.5
    ),
    fill_depth_meters = c(
      6,   # >5
      10,  # >5
      6,   # >5 but width <=2
      4    # <=5
    ),
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(dat)

  # rows 1 & 2 → special case
  testthat::expect_identical(
    out$crossing_fix[1:2],
    base::rep("Replace Structure with Streambed Simulation CBS", 2)
  )

  # row 3 → base rule (width <= 2)
  testthat::expect_identical(
    out$crossing_fix[3],
    "Replace Structure with Streambed Simulation CBS"
  )

  # row 4 → base rule (fill <= 5) → open bottom
  testthat::expect_identical(
    out$crossing_fix[4],
    "Replace with New Open Bottom Structure"
  )
})

testthat::test_that("fpr_xfm_paw_xing_fix_size works with custom column names", {
  dat <- base::data.frame(
    my_barrier = c("Barrier", "Passable"),
    my_chan = c(2.5, 3.0),
    my_fill = c(4, 7),
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(
    dat = dat,
    col_barrier_result = my_barrier,
    col_downstream_channel_width_meters = my_chan,
    col_fill_depth_meters = my_fill,
    col_crossing_fix = my_fix
  )

  testthat::expect_true("my_fix" %in% base::names(out))

  # row 1 → open bottom structure
  testthat::expect_identical(
    out$my_fix[1],
    "Replace with New Open Bottom Structure"
  )

  # row 2 → NA (Passable)
  testthat::expect_true(base::is.na(out$my_fix[2]))
})

testthat::test_that("fpr_xfm_paw_xing_fix_size preserves existing columns", {
  dat <- base::data.frame(
    barrier_result = c("Barrier", "Passable"),
    downstream_channel_width_meters = c(2.5, 2.5),
    fill_depth_meters = c(4, 4),
    extra_col = c("a", "b"),
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(dat)

  testthat::expect_true(
    base::all(
      c(
        "barrier_result",
        "downstream_channel_width_meters",
        "fill_depth_meters",
        "extra_col"
      ) %in% base::names(out)
    )
  )

  testthat::expect_identical(out$extra_col, dat$extra_col)
})

testthat::test_that("fpr_xfm_paw_xing_fix_size sets recommended size for bridge, streambed, and passable cases", {
  dat <- base::data.frame(
    barrier_result = c("Barrier", "Other", "Passable", "Unknown"),
    downstream_channel_width_meters = c(3, 3, 3, 3),
    fill_depth_meters = c(3, 4, 4, 4),
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(dat)

  testthat::expect_true("recommended_diameter_or_span_meters" %in% base::names(out))

  # Barrier with width >= chan_wdth_min_brdg (2) → bridge with default brdg_wdth (15)
  testthat::expect_identical(
    out$recommended_diameter_or_span_meters[1],
    15
  )

  # "Other" → streambed simulation with minimum span (default ss_span_min = 3)
  testthat::expect_identical(
    out$recommended_diameter_or_span_meters[2],
    3
  )

  # Passable / Unknown → NA_real_
  testthat::expect_true(base::is.na(out$recommended_diameter_or_span_meters[3]))
  testthat::expect_true(base::is.na(out$recommended_diameter_or_span_meters[4]))
})


testthat::test_that("fpr_xfm_paw_xing_fix_size increases recommended size with additional fill depth", {
  dat <- base::data.frame(
    barrier_result = c("Barrier", "Barrier"),
    downstream_channel_width_meters = c(4, 4),  # <= chn_wdth_max so wide-channel rule not triggered
    fill_depth_meters = c(3, 7),                # second row has 4m extra fill
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(dat)

  # Row 1: fill_depth = 3 → no extra fill above fill_dpth (3), so base span = brdg_wdth = 15
  testthat::expect_identical(
    out$recommended_diameter_or_span_meters[1],
    15
  )

  # Row 2:
  # fill_dpth_over = 7 - 3 = 4
  # extra span = fill_dpth_mult (3) * 4 = 12
  # base span = 15 → total = 27
  testthat::expect_identical(
    out$recommended_diameter_or_span_meters[2],
    27
  )
})


testthat::test_that("fpr_xfm_paw_xing_fix_size extends size when channel is very wide", {
  dat <- base::data.frame(
    barrier_result = "Barrier",
    downstream_channel_width_meters = 10,  # > chn_wdth_max (5)
    fill_depth_meters = 3,                 # no extra fill
    stringsAsFactors = FALSE
  )

  # Use a smaller base bridge width to make the math simple
  out <- fpr_xfm_paw_xing_fix_size(dat, brdg_wdth = 8)

  # Logic:
  # - Initial recommended span = brdg_wdth = 8
  # - Check condition: 8 < (10 + chan_wdth_to_span_ok(4)) → 8 < 14 → TRUE
  #   and 10 > chn_wdth_max(5) → TRUE
  # - New span = (10 - 5) + 8 = 13
  testthat::expect_identical(
    out$recommended_diameter_or_span_meters[1],
    13
  )
})


testthat::test_that("fpr_xfm_paw_xing_fix_size applies special streambed simulation case and max span", {
  dat <- base::data.frame(
    barrier_result = c("Barrier", "Potential"),
    downstream_channel_width_meters = c(3, 3.2),  # > chan_wdth_min_brdg (2) and <= stream_wdth_max_ss (3.5)
    fill_depth_meters = c(6, 10),                 # > fill_dpth_max (5)
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(dat)

  # Both rows meet the special case criteria → streambed simulation with ss_span_max (4.5)
  testthat::expect_identical(
    out$crossing_fix,
    base::rep("Replace Structure with Streambed Simulation CBS", 2)
  )

  testthat::expect_identical(
    out$recommended_diameter_or_span_meters,
    base::rep(4.5, 2)
  )
})


testthat::test_that("fpr_xfm_paw_xing_fix_size rounds recommended size to nearest 0.5m", {
  dat <- base::data.frame(
    barrier_result = "Barrier",
    downstream_channel_width_meters = 3,  # <= chn_wdth_max so no wide-channel extension
    fill_depth_meters = 3,                # no extra fill
    stringsAsFactors = FALSE
  )

  # Set a non-0.5-multiple base width
  out <- fpr_xfm_paw_xing_fix_size(dat, brdg_wdth = 8.3)

  # Before rounding: span = 8.3
  # After rounding to nearest 0.5: 8.5
  testthat::expect_equal(
    out$recommended_diameter_or_span_meters[1],
    8.5
  )
})


testthat::test_that("fpr_xfm_paw_xing_fix_size supports custom recommended size column name", {
  dat <- base::data.frame(
    my_barrier = c("Barrier", "Other"),
    my_chan = c(3, 3),
    my_fill = c(3, 4),
    stringsAsFactors = FALSE
  )

  out <- fpr_xfm_paw_xing_fix_size(
    dat = dat,
    col_barrier_result = my_barrier,
    col_downstream_channel_width_meters = my_chan,
    col_fill_depth_meters = my_fill,
    col_crossing_fix = my_fix,
    col_recommended_diameter_or_span_meters = my_span
  )

  # New column name exists
  testthat::expect_true("my_span" %in% base::names(out))

  # For Barrier with width >= 2, span should be default brdg_wdth = 15
  testthat::expect_identical(
    out$my_span[1],
    15
  )

  # "Other" → streambed simulation min span = 3
  testthat::expect_identical(
    out$my_span[2],
    3
  )
})

