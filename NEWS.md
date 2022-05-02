# fpr (development version)

# add `fpr_make_geopackage()`

# fpr 0.1.0.9002

* `fpr_table_bcfp()` now includes `fpr_kable` with cookie cutter caption and footnotes.  Should move caption and footnotes to params for more flexibility.
*  Tweaked individual photot in `fpr_phtot_amalg_cv()` from "x420"" to "560x420!" to deal with photos that start out as wrong size
* add `fpr_plot_fish_box()` 
* add `fpr_table_fish_density()` 
* add `fpr_table_fish_site()` 
* tweak photo params in `fpr_photo_amalg_cv()` so originals are resized
* `fpr_sheet_trim()` (helper) now has an option to remove all rows without an input in the second (or any) column.
* ignore more species in `fpr_my_fish_sp()` by default
* tweaks to `fpr_my_channel_sentence()`
* change page break hack `fpr_table_cv_summary_memo()` so that it works with pagedown

# fpr 0.1.0.9001

* `fpr_import_hab_con()` now creates directory `data/backup` and backs up all sheets in csv format (#1)
* `fpr_sheet_trim()` (helper) now removes all rows without an input into the first column.  
