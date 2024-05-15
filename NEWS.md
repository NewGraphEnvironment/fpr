# Development version
* remove `poisutils` dependency as per [#83](https://github.com/NewGraphEnvironment/fpr/issues/83)
* `fpr_sp_assign_utm` alerts user if the default values for col_easting and col_northing are not present in the dat sf object
as per [#85](https://github.com/NewGraphEnvironment/fpr/issues/85)
* `fpr_my_bcfishpass` move from `{{}}` for param columns to `ensym` and `!!` to provide flexibility for `dat` param objects other than `bcfishpass` - close [#86](https://github.com/NewGraphEnvironment/fpr/issues/86)
* `fpr_photo_amalg_cv` - close [#46](https://github.com/NewGraphEnvironment/fpr/issues/46)
* close [#75](https://github.com/NewGraphEnvironment/fpr/issues/75) and [#76](https://github.com/NewGraphEnvironment/fpr/issues/76) by using `fpr_kable` in `fpr_table_cv_summary_memo`
* add fpr_xref_crossings as per https://github.com/NewGraphEnvironment/fish_passage_template_reporting/issues/1
* add pass through `...` to `fpr_kable` to allow for more flexibility
* Update `fpr_kable` to deal with duplicated Table captions as per [#68](https://github.com/NewGraphEnvironment/fpr/issues/68)
* Update fpr_photo_amalg_cv to include full calls to functions, importFrom and move to stand alone script. replace calls to %like% with str_detect. add to photo family
* Added fpr_dbq_clip which clips a point layer by a polygon layer.
* Added fpr_create_hydrograph which creates hydrographs of flow data for a given station
* change name of `fpr_t_site_id` to `fpr_tidy_assign_site_id` to clarify use and avoid future conflicts if/when 
fpr_test_* functions are introduced
* in `fpr_sp_gpkg_backup` split out write_backup params into write_csv and write_rdata to provide more flexibility. change to new name for fpr_tidy_assign_site_id


# fpr 1.1.0
* add `fpr_sp_gpkg_backup`
* add `fpr_t_site_id`
* add `fpr_sp_assign_sf_from_utm`
* add `fpr_make_xref_pscis_modelled`
* add `fpr_dbq_lstable`
* add `fpr_dbq_subset`
* add `fpr_db_query`
* add `fpr_db_conn`
* add `fpr_sp_utm_assign`
* close [#34](https://github.com/NewGraphEnvironment/fpr/issues/34)
* add `fpr_photo_qa_missing_all`
* add `fpr_photo_qa_df`
* add `fpr_remove_dupes` to close [#42](https://github.com/NewGraphEnvironment/fpr/issues/42)
* add `fpr_photo_rename`
* add `fpr_photo_resize_batch`
* fix [#29](https://github.com/NewGraphEnvironment/fpr/issues/29)
* fix [#26](https://github.com/NewGraphEnvironment/fpr/issues/26)

# fpr 1.0.2.9
* change names of 
  + fpr_table_barrier_result 
  + fpr_table_barrier_scoring 
  + fpr_table_habvalue 

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
