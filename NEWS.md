# fpr v.20221220 or  1.0.0.0

* BREAKING CHANGE.  `fpr_import_hab_con` now requires `row_empty_remove = T` (pushes through to `fpr_sheet_trim`) in order to function correctly. Reason is that `fpr_import_pscis` now explicity drops the first 3 rows on import in order to call column types from `fpr_xref_pscis`.
* `fpr_xref_pscis` built from `xref_names_pscis` with column types appended.  Better way to do this may be found at https://stackoverflow.com/questions/24067782/assign-data-types-to-each-column-of-a-data-frame-in-r as it allows fpr_imp
* `fpr_tab_habvalue`
* Add param for directory in `fpr_import_pscis` to allow more flexibility
* cmd badge addition
* add switch to 

# fpr v.20221215 or  0.1.0.9003

* add `fpr_make_geopackage()`
* add backup switch for `fpr_import_pscis_all`
* add `fpr_data_tab_habvalue` data object

# fpr 0.1.0.9002

* `fpr_table_bcfp()` now includes `fpr_kable` with cookie cutter caption and footnotes.  Should move caption and footnotes to params for more flexibility.
*  Tweaked individual photo in `fpr_phtot_amalg_cv()` from "x420"" to "560x420!" to deal with photos that start out as wrong size
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
