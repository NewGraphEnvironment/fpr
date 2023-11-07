#' This is a table that cross references the names PSCIS submission template to the bc data catalog names to names used in reporting.  Also has columns that reference untidy table ids for join for two column table.
#'
#' @name fpr_xref_pscis
#' @docType data
#' @title xref_pscis
#' @description This is a table that cross references the names PSCIS submission template to the bc data catalog names to names used in reporting.  Also has columns that reference untidy table ids for join for two column table.
#' @details
#' Latest version created with script in https://github.com/NewGraphEnvironment/fpr/issues/6
#' Needed to bring in date as numeric then convert later
#' @eval  rdScript("inst/createCrimeData.R")
#' @author Al Irvine \email{al@newgraphenvironment.com}
#' @references \url{https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/aquatic-habitat-management/fish-passage/fish-passage-technical/assessment-projects}
#' @keywords data
fpr_xref_pscis <- tibble::tribble(
                          ~bcdata,                               ~spdsht,                 ~report, ~id_join, ~id_side, ~type_readxl, ~type_on_import,
                             "id",                                    NA,                      NA,       NA,       NA,           NA,           FALSE,
         "funding_project_number",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                "funding_project",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                     "project_id",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                 "funding_source",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
         "responsible_party_name",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                "consultant_name",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                "assessment_date",                                "date",                  "Date",       1L,       1L,    "numeric",            TRUE,
             "stream_crossing_id",                   "pscis_crossing_id",              "PSCIS ID",       2L,       1L,    "numeric",            TRUE,
                  "assessment_id",                                    NA,                      NA,       NA,       NA,           NA,           FALSE,
    "external_crossing_reference",               "my_crossing_reference",           "External ID",       3L,       1L,    "numeric",            TRUE,
                   "crew_members",                        "crew_members",                  "Crew",       5L,       1L,       "text",            TRUE,
                       "utm_zone",                            "utm_zone",              "UTM Zone",       6L,       1L,    "numeric",            TRUE,
                    "utm_easting",                             "easting",               "Easting",       7L,       1L,    "numeric",            TRUE,
                   "utm_northing",                            "northing",              "Northing",       8L,       1L,    "numeric",            TRUE,
        "location_confidence_ind",                                    NA,                      NA,       NA,       NA,           NA,           FALSE,
                    "stream_name",                         "stream_name",                "Stream",       9L,       1L,       "text",            TRUE,
                      "road_name",                           "road_name",                  "Road",      10L,       1L,       "text",            TRUE,
                   "road_km_mark",                        "road_km_mark",                      NA,       NA,       NA,    "numeric",            TRUE,
                    "road_tenure",                         "road_tenure",           "Road Tenure",      11L,       1L,       "text",            TRUE,
             "crossing_type_code",                       "crossing_type",         "Crossing Type",       NA,       NA,       "text",            TRUE,
             "crossing_type_desc",                                    NA,                      NA,       NA,       NA,           NA,           FALSE,
          "crossing_subtype_code",                    "crossing_subtype",     "Crossing Sub Type",       1L,       2L,       "text",            TRUE,
          "crossing_subtype_desc",                                    NA,                      NA,       NA,       NA,           NA,           FALSE,
               "diameter_or_span",             "diameter_or_span_meters",          "Diameter (m)",       2L,       2L,    "numeric",            TRUE,
                "length_or_width",              "length_or_width_meters",            "Length (m)",       3L,       2L,    "numeric",            TRUE,
    "continuous_embeddedment_ind",      "continuous_embeddedment_yes_no",              "Embedded",       5L,       2L,       "text",            TRUE,
     "average_depth_embededdment",   "average_depth_embededdment_meters",    "Depth Embedded (m)",       6L,       2L,    "numeric",            TRUE,
           "resemble_channel_ind",             "resemble_channel_yes_no",      "Resemble Channel",       7L,       2L,       "text",            TRUE,
                "backwatered_ind",                  "backwatered_yes_no",           "Backwatered",       8L,       2L,       "text",            TRUE,
         "percentage_backwatered",              "percentage_backwatered",   "Percent Backwatered",       9L,       2L,    "numeric",            TRUE,
                     "fill_depth",                   "fill_depth_meters",        "Fill Depth (m)",      10L,       2L,    "numeric",            TRUE,
                    "outlet_drop",                  "outlet_drop_meters",       "Outlet Drop (m)",      11L,       2L,    "numeric",            TRUE,
              "outlet_pool_depth",             "outlet_pool_depth_0_01m", "Outlet Pool Depth (m)",      12L,       2L,    "numeric",            TRUE,
                 "inlet_drop_ind",                   "inlet_drop_yes_no",            "Inlet Drop",      13L,       2L,       "text",            TRUE,
                  "culvert_slope",               "culvert_slope_percent",             "Slope (%)",      14L,       2L,    "numeric",            TRUE,
       "downstream_channel_width",     "downstream_channel_width_meters",     "Channel Width (m)",      12L,       1L,    "numeric",            TRUE,
                   "stream_slope",                        "stream_slope",      "Stream Slope (%)",      13L,       1L,    "numeric",            TRUE,
            "beaver_activity_ind",              "beaver_activity_yes_no",       "Beaver Activity",      14L,       1L,       "text",            TRUE,
              "fish_observed_ind",                "fish_observed_yes_no",          "Fish Sighted",       NA,       NA,       "text",            TRUE,
               "valley_fill_code",                         "valley_fill",           "Valley Fill",      15L,       2L,       "text",            TRUE,
          "valley_fill_code_desc",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
             "habitat_value_code",                       "habitat_value",         "Habitat Value",      15L,       1L,       "text",            TRUE,
             "habitat_value_desc",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
             "stream_width_ratio",                  "stream_width_ratio",                   "SWR",       NA,       NA,    "numeric",            TRUE,
       "stream_width_ratio_score",                                    NA,                 "Score",       NA,       NA,    "numeric",            TRUE,
           "culvert_length_score",                "culvert_length_score",                 "Score",       NA,       NA,    "numeric",            TRUE,
                    "embed_score",                         "embed_score",                 "Score",       NA,       NA,    "numeric",            TRUE,
              "outlet_drop_score",                   "outlet_drop_score",                 "Score",       NA,       NA,    "numeric",            TRUE,
            "culvert_slope_score",                 "culvert_slope_score",                 "Score",       NA,       NA,    "numeric",            TRUE,
                    "final_score",                         "final_score",           "Final score",      16L,       1L,    "numeric",            TRUE,
            "barrier_result_code",                      "barrier_result",        "Barrier Result",      16L,       2L,       "text",            TRUE,
     "barrier_result_description",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
              "crossing_fix_code",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
              "crossing_fix_desc",                        "crossing_fix",              "Fix type",      17L,       1L,       "text",            TRUE,
   "recommended_diameter_or_span", "recommended_diameter_or_span_meters",   "Fix Span / Diameter",      17L,       2L,    "numeric",            TRUE,
             "assessment_comment",                  "assessment_comment",               "Comment",       NA,       NA,       "text",            TRUE,
                     "ecocat_url",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                 "image_view_url",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
           "current_pscis_status",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
     "current_crossing_type_code",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
     "current_crossing_type_desc",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
  "current_crossing_subtype_code",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
  "current_crossing_subtype_desc",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
    "current_barrier_result_code",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
    "current_barrier_description",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                   "feature_code",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                       "objectid",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
               "se_anno_cad_data",                                    NA,                      NA,       NA,       NA,       "text",            FALSE,
                       "geometry",                                    NA,                      NA,       NA,       NA,           NA,           FALSE
  )

usethis::use_data(fpr_xref_pscis, overwrite = TRUE)


fpr_xref_fix <- tibble::tribble(
  ~crossing_fix_code,                                ~crossing_fix_desc,                                     ~crossing_fix,
                "RM",                    "Remove / Deactivate Crossing",                      "Remove/Deactivate Crossing",
               "OBS",          "Replace with new open bottom structure",          "Replace with New Open Bottom Structure",
            "SS-CBS", "Replace structure with streambed simulation CBS", "Replace Structure with Streambed Simulation CBS",
                "EM",          "Add substrate to further imbed the CBS",          "Add Substrate to Further embed the CBS",
                "BW",     "Install downstream weir(s) to backwater CBS",     "Install Downstream Weir(s) to Backwater CBS"
  )

usethis::use_data(fpr_xref_fix, overwrite = TRUE)

fpr_xref_obstacles <- tibble::tribble(
                                                   ~fiss_obstacle_name, ~spreadsheet_feature_type,
                                                         "Beaver Dams",                        NA,
                                                    "Velocity barrier",                        NA,
                                                               "Wedge",                        NA,
                                                              "Bridge",                        NA,
                                                           "Hydro Dam",                        NA,
                                        "Water Management Storage Dam",                        NA,
                                                       "Not Specified",                        NA,
                                                                "Bars",                        NA,
                                                             "LWD Jam",                 "LWD Jam",
                                                               "OTHER",                        NA,
                                             "Irrigation District Dam",                        NA,
                                                "Dam - Unknown Origin",                        NA,
                                                             "Cascade",                        NA,
                                                                "Pump",                        NA,
                                                             "Log jam",                        NA,
                                                    "Cascade or Chute",                        NA,
                                                   "Persistent Debris",                        NA,
                                                           "Hydro dam",                        NA,
                                                               "Rocks",                        NA,
                        "Persistent debris; present for several years",                        NA,
                                                                "Weir",                        NA,
                                                               "Falls",                   "falls",
                                                                "Logs",                        NA,
                                                             "Log Jam",                        NA,
                                                             "Culvert",                        NA,
                                                                "Rock",                        NA,
                                                              "Canyon",                        NA,
                                                          "Beaver Dam",                        NA,
                                               "Regional District Dam",                        NA,
                                                         "Underground",                        NA,
                                                        "Woody Debris",                        NA,
                                                       "Cascade/Chute",        "cascade or Chute",
                                                         "Private Dam",                        NA,
                                                            "Gradient",                        NA,
                                            "Fisheries Management Dam",                        NA,
                                                          "BEAVER DAM",                        NA,
                                         "Landslide or bank sloughing",                        NA,
                                                    "Velocity Barrier",                        NA,
                                                                 "Dam",                        NA
                        )

usethis::use_data(fpr_xref_obstacles, overwrite = TRUE)

fpr_xref_photo_order <- tibble::tribble(
          ~photo, ~sort,
     "upstream",      1,
   "downstream",      2,
        "inlet",      3,
       "outlet",      4,
       "barrel",      5,
         "road",      6,
    "condition",      7,
         "Fill",      8,
     "Blockage",      9,
  "us_typical1",     10,
  "ds_typical1",     11,
    "us_gravel",     12,
    "ds_gravel",     13,
      "us_pool",     14,
      "ds_pool",     15,
  "us_typical2",     16,
  "ds_typical2",     17,
       "us_top",     18,
    "ds_bottom",     19,
          "_k_",     20
  )


usethis::use_data(fpr_xref_photo_order, overwrite = TRUE)

