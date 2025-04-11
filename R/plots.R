
#' Generate boxplot for individual site showing densities of fish captured from electrofishing
#'
#' @param dat Dataframe. Defaults to fish_abund.  Must contian the columns site, species_code,
#' life_stage, density_100m2
#' @param sit Integer. ID for the site. Defaults to my_site.
#' @param theme Function. Theme used for the plot. Defaults to `ggplot2::theme_bw()`.
#'
#' @return Boxplot
#' @export
#'
#' @examples
fpr_plot_fish_box <- function(dat = fish_abund, sit = my_site, theme = ggplot2::theme_bw()){
  dat |>
    dplyr::filter(
      site == sit &
        species_code != 'NFC'
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = location, y = density_100m2)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_grid(species_code ~ life_stage, scales = "fixed", as.table = TRUE) +
    theme +
    ggplot2::theme(legend.position = "none", axis.title.x = ggplot2::element_blank()) +
    ggplot2::geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1) +
    ggplot2::ylab(expression(Density ~ (fish / 100 ~ m^2)))
}

