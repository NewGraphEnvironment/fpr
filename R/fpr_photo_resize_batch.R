#' Resize batch of images and convert JPG
#'
#'
#' @param dir_source String full path name of directory where photos are at
#' @param dir_target  String full path name of directory where photos are to be copied to
#' @param ...  Not used. For passing alternative destination photo size as string to \link{fpr_photo_resize_convert}
#' @importFrom fs dir_ls
#'
#' @return Photos resized with `.JPG` extension
#' @export
#'
#' @examples
fpr_photo_resize_batch <- function(dir_source = NULL,
                                   dir_target = NULL,
                                   ...){
  filestoconvert <- fs::dir_ls(
    path = dir_source,
    recurse = FALSE,
    regexp = "(?i)\\.(jpe?g|png)$"
  )

  filestoconvert |>
    purrr::map(fpr::fpr_photo_resize_convert,
               path = dir_target
    )
}
