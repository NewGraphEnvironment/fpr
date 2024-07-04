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
      kableExtra::scroll_box(width = "90%", height = scroll_box_height)
  }

  dat2
}
