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
                                      col_filter = pscis_crossing_id){
  fpr_table_cv_detailed(dat = dat %>% filter({{ col_filter }} == site)) |>
    fpr_kable(caption_text = paste0("Summary of fish passage assessment for PSCIS crossing ",
                                    site, "."), booktabs = T, scroll = FALSE, font = font) |>
    #this grabs the comments out
    kableExtra::add_footnote(label = paste0('Comments: ', dat %>% filter({{ col_filter }} == site) |>
                                              pull(assessment_comment)), notation = 'none') %>%
    kableExtra::add_footnote(label = paste0('Photos: From top left clockwise: Road/Site Card, Barrel, Outlet, Downstream, Upstream, Inlet.',
                                            paste0('![](data/photos/', site_photo_id, '/crossing_all.JPG)')), notation = 'none')
}
