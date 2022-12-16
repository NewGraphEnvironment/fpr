fpr_data_tab_habvalue <- tibble::tibble(`Habitat Value` = c('High', 'Medium', 'Low'),
                               `Fish Habitat Criteria` = c(
                                 'The presence of high value spawning or rearing habitat (e.g., locations with abundance of suitably sized gravels, deep pools, undercut banks, or stable debris) which are critical to the fish population.',
                                 'Important migration corridor. Presence of suitable spawning habitat. Habitat with moderate rearing potential for the fish species present.', 'No suitable spawning habitat, and habitat with low rearing potential (e.g., locations without deep pools, undercut banks, or stable debris, and with little or no suitably sized spawning gravels for the fish species present).'
                               )
)

usethis::use_data(fpr_data_tab_habvalue, overwrite = TRUE)
