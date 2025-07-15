#' Summary table of culvert info with photo to place in memos.
#'
#' @param dat Dataframe. Defaults to pscis_phase2
#' @param site Integer. Name of PSCIS site to filter pscis_phase2. Defaults to my_site
#' @param site_photo_id Integer. Name of PSCIS site to source photo. Defaults to my_site
#' @param font Integer. Size of font.
#' @param col_filter String unquoted.  Name of column to filter site from. Defaults to pscis_crossing_id
#'
#' @return html object
#' @export
#'
#' @examples
fpr_table_cv_summary_memo <- function(dat = pscis_phase2,
                                      site = my_site,
                                      site_photo_id = my_site,
                                      font = 11,
                                      col_filter = pscis_crossing_id) {

  dat_site <- dat |> dplyr::filter({{ col_filter }} == site)
  comments <- dat_site |> dplyr::pull(assessment_comment)

  comment_label <- paste0("Comments: ", comments)

  photo_label <- "Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet."
  photos_inserted <- paste0("![](data/photos/", site_photo_id, "/crossing_all.JPG)")

  fpr::fpr_kable(
    fpr::fpr_table_cv_detailed(dat = dat_site),
    caption_text = paste0("Summary of fish passage assessment for PSCIS crossing ", site, "."),
    booktabs = TRUE,
    scroll = FALSE,
    font = font
  ) |>
    kableExtra::add_footnote(
      label = c(comment_label, photo_label, photos_inserted),
      notation = "none"
    )
}

