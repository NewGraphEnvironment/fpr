% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpr_create_hydrograph.R
\name{fpr_create_hydrograph}
\alias{fpr_create_hydrograph}
\title{Creates hydrograph and hydrology stats figures for a given station.}
\usage{
fpr_create_hydrograph(
  station = NULL,
  pane_hydat = TRUE,
  single_hydat = TRUE,
  start_year = NULL,
  end_year = NULL
)
}
\arguments{
\item{station}{String (quoted) number of station}

\item{pane_hydat}{Boolean TRUE if you want a pane layout of all hydrographs}

\item{single_hydat}{Boolean TRUE if you want a single hydrograph with mean flows}

\item{start_year}{Specific start year, if not specified, will use the first year of the data}

\item{end_year}{Specific end year, if not specified, will use the first year of the data}

\item{fig/hydrology_stats_}{hydrology stats figure saved to the fig folder}

\item{fig/hydrograph_}{hydrograph figure saved to the fig folder}
}
\description{
Creates hydrograph and hydrology stats figures for a given station.
}
\examples{
\dontrun{fpr_create_hydrograph('08EE004', pane_hydat = FALSE)}
}
