% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/photo.R
\name{fpr_photo_qa_df}
\alias{fpr_photo_qa_df}
\title{QA photos}
\usage{
fpr_photo_qa_df(dat = NULL, ...)
}
\arguments{
\item{dat}{Dataframe that contains a column with the numeric site ids. Defaults to `pscis_all` created using
\link{fpr_import_pscis_all}
@family photo}

\item{...}{Not used. Use to pass string path defined as `dir_photos = 'quoted_filepath'` to run
QA outside of defualt filepath of root of helper function  \link{fpr_photo_qa}. Incorperates outputs
of \link{fpr_photo_qa_missing_all}}
}
\value{
Tibble with 7 columns showing missing phots.
}
\description{
See if all required PSCIS photos are named and look for duplicates of those names.
}
