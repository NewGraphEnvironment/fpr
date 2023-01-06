fpr_table_habvalue <- tibble::tibble(`Habitat Value` = c('High', 'Medium', 'Low'),
                               `Fish Habitat Criteria` = c(
                                 'The presence of high value spawning or rearing habitat (e.g., locations with abundance of suitably sized gravels, deep pools, undercut banks, or stable debris) which are critical to the fish population.',
                                 'Important migration corridor. Presence of suitable spawning habitat. Habitat with moderate rearing potential for the fish species present.', 'No suitable spawning habitat, and habitat with low rearing potential (e.g., locations without deep pools, undercut banks, or stable debris, and with little or no suitably sized spawning gravels for the fish species present).'
                               )
)

usethis::use_data(fpr_table_habvalue, overwrite = TRUE)


fpr_table_barrier_scoring <- tibble::tribble(
  ~Risk,   ~Embedded,                                 ~Value,  ~`Outlet Drop (cm)`, ~Value, ~SWR,    ~Value, ~`Slope (%)`, ~Value, ~`Length (m)`, ~Value,
  "LOW",  ">30cm or >20% of diameter and continuous",  "0",       "<15",              '0',  '<1.0',    '0',      '<1',      '0',     '<15',         '0',
  "MOD",  "<30cm or 20% of diameter but continuous",   "5",       "15-30",            '5',  '1.0-1.3', '3',      '1-3',     '5',     '15-30',       '3',
  "HIGH", "No embedment or discontinuous",             "10",      ">30",             '10',  '>1.3',    '6',       '>3',     '10',    '>30',         '6',
)

usethis::use_data(fpr_table_barrier_scoring, overwrite = TRUE)

fpr_table_barrier_result <- tibble::tribble(
  ~`Cumlative Score`, ~Result,
  '0-14',             'passable',
  '15-19',            'potential barrier',
  '>20',              'barrier'
)

usethis::use_data(fpr_table_barrier_result, overwrite = TRUE)
