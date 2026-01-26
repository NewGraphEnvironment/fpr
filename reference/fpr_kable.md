# Custom kable settings

Custom kable settings

## Usage

``` r
fpr_kable(
  dat,
  caption_text = "",
  font = font_set,
  footnote_text = NULL,
  scroll = TRUE,
  scroll_box_height = "500px",
  col_width_min = NULL,
  width_min = "1.0in",
  ...
)
```

## Arguments

- dat:

  dataframe to make a table with

- caption_text:

  string to insert as caption

- font:

  size of font. Defaults to font_set usually set in setup chunks of
  index.Rmd based on output type ie. gitbook vs pagedown

- footnote_text:

  string to insert as footnote

- scroll:

  TRUE or FALSE about whether to have scroll

- scroll_box_height:

  string. pixel height of scroll box. Defaults to "500px"

- col_width_min:

  Number or vector of numbers. Defaults to NULL

- width_min:

  string with units of inches (in). Defaults to 1.0in

- ...:

  Not used. Open for passing arguments to \`knitr::kable\`
