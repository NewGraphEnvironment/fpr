# fpr 1.0.2
# Development Version


# fpr 1.0.2
* fix bug where `fpr_my_habitat_paragraph` was putting commas between sentences

# fpr 1.0.1
* fix [#17](https://github.com/NewGraphEnvironment/fpr/issues/17) in `my_channel_sentence` so we actually specify the site (vs defaut my_site) in the `my_habitat_info` request for the average gradient


# fpr 1.0.0
* v.20221221
* BREAKING CHANGE.  `fpr_import_hab_con` now requires `row_empty_remove = T` (pushes through to `fpr_sheet_trim`) in order to correctly choose the first complete row of the template speadsheets to be the column names. Reason is that `fpr_import_pscis` now explicitly drops the first 3 rows on import in order to call column types from `fpr_xref_pscis` before the call to `fpr_sheet_trim`.
* `fpr_xref_pscis` built from `xref_names_pscis` with column types appended.  Better way to do this may be found at https://stackoverflow.com/questions/24067782/assign-data-types-to-each-column-of-a-data-frame-in-r as it allows fpr_imp
* `fpr_tab_habvalue`
* Add param for directory in `fpr_import_pscis` to allow more flexibility
* cmd badge addition
* add objects 
  + fpr_tab_barrier_result 
  + fpr_tab_barrier_scoring 
  + fpr_tab_habvalue 
  + fpr_xref_fix 
  + fpr_xref_obstacles 
  + fpr_xref_pscis 
  
  

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
