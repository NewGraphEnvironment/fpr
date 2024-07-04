# script to copy over files to the inst/staticexports directory

# copy over the files

fs::file_copy("R/fpr_kable.R", "inst/staticexports/fpr_kable.R", overwrite = TRUE)
