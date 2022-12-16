
#' Generate boxplot for individual site showing densities of fish captured from electrofishing
#'
#' @param dat Dataframe. Defaults to fish_abund.  Must contian the columns site, species_code,
#' life_stage, density_100m2
#' @param sit Integer. ID for the site. Defaults to my_site.
#'
#' @return Boxplot
#' @export
#'
#' @examples
fpr_plot_fish_box <- function(dat = fish_abund, sit = my_site){
  dat %>%
    filter(
      site  == sit
      &
        species_code != 'NFC'
    ) %>%
    ggplot(., aes(x = location, y =density_100m2)) +
    geom_boxplot()+
    facet_grid(species_code ~ life_stage, scales ="fixed",
               as.table = T)+
    dark_theme_bw()+
    theme(legend.position = "none", axis.title.x=element_blank()) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
    ylab(expression(Density ~ (fish/100 ~  m^2)))
}

