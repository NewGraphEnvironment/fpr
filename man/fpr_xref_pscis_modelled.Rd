% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpr_xref_pscis_modelled.R
\name{fpr_xref_pscis_modelled}
\alias{fpr_xref_pscis_modelled}
\title{Create a Cross-Reference for PSCIS Modelled Data}
\usage{
fpr_xref_pscis_modelled(project_id = NULL)
}
\arguments{
\item{project_id}{A character string (quoted) specifying the project ID (or list of project IDS separated by commas)
to filter the data on. Default is NULL.}
}
\value{
A data frame with cleaned names and numeric columns, serving as a cross-reference for PSCIS modeled data.
}
\description{
Used after our data is submitted to the province so that we can cross reference our modeled IDs to their new PSCIS identifiers.
This function queries geospatial data from the BC Data Catalogue, filters it based on the provided project ID,
selects specific columns, cleans the column names, drops unnecessary columns and geometry data, and converts all
remaining columns to numeric. The resulting data frame serves as a cross-reference for PSCIS modeled data.
}
\examples{
\dontrun{
fpr_xref_pscis_modelled(project_id = 'peace_2023_Phase1')
}
}
