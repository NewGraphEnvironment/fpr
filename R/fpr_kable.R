#' Custom kable settings
#'
#' @param dat dataframe to make a table with
#' @param caption_text string to insert as caption
#' @param font size of font.  Defaults to font_set usually set in setup chunks of index.Rmd based on output type ie. gitbook vs pagedown
#' @param footnote_text string to insert as footnote
#' @param scroll TRUE or FALSE about whether to have scroll
#' @param scroll_box_height string. pixel height of scroll box.  Defaults to "500px"
#' @param col_width_min Number or vector of numbers. Defaults to NULL
#' @param width_min string with units of inches (in). Defaults to 1.0in
#' @param ... Not used. Open for passing arguments to `knitr::kable`
#'
#' @return
#' @export
#'
#' @examples
fpr_kable <- function(dat,
                      caption_text = '',
                      font = font_set,
                      footnote_text = NULL,
                      scroll = TRUE,
                      scroll_box_height = "500px",
                      col_width_min = NULL,
                      width_min = '1.0in',
                      ...){
  dat2 <- dat %>%
    knitr::kable(caption = caption_text, booktabs = T, label = NA, ...) %>%
    kableExtra::kable_styling(c("condensed", "responsive"),
                              full_width = T,
                              font_size = font)
  if(!is.null(footnote_text)){
    dat2 <- dat2 %>%
      kableExtra::footnote(symbol = footnote_text)
  }
  if(!is.null(col_width_min)){
    dat2 <- dat2 %>%
      kableExtra::column_spec(column = col_width_min, width_min = width_min)
  }
  if(identical(scroll,TRUE)){
    dat2 <- dat2 %>%
      kableExtra::scroll_box(width = "100%", height = scroll_box_height)
  }

  dat2
}
