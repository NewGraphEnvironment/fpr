#' Resize images and convert JPG
#'
#'
#' @param photo String of full path name of photo
#' @param size  String image width and height of photo you want to resize to
#' @param path  String of path to the directory where you want to burn the converted img
#'
#' @return
#' @export
#'
#' @examples
fpr_photo_resize_convert <- function(photo,
                                     size = "1036.8x777.6!",
                                     path = NULL){
  image <- magick::image_read(photo)
  image_scaled <- magick::image_scale(image, size)
  magick::image_write(image_scaled, path = paste0(path, '/', tools::file_path_sans_ext(basename(photo)), '.JPG'), format = 'jpg')
}
