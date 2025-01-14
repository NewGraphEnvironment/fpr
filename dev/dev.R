# Step 2: clean then build and install the package
processx::run(
  command = "R",
  args = c("CMD", "INSTALL", "--preclean", "--no-multiarch", "--with-keep.source", "."),
  echo = TRUE
)


staticimports::import(
  dir = here::here("R/"),
  outfile = here::here("R/staticimports.R")
)
