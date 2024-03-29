% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_make_unique_ids.R
\name{cdr_make_unique_ids}
\alias{cdr_make_unique_ids}
\title{Attach unique IDs as the key column}
\usage{
cdr_make_unique_ids(db_tbl, key_field = "UID", use_guid = FALSE)
}
\arguments{
\item{db_tbl}{a dataframe}

\item{key_field}{the name of the column with the Unique ID}

\item{use_guid}{use a 12 digit GUID instead of the default format}
}
\value{
a table with a column of Unique IDs
}
\description{
Attach unique IDs as the key column
}
\examples{
cdr_make_unique_ids(utils::head(iris,11))
cdr_make_unique_ids(utils::head(iris,11), key_field = 'GUID', use_guid = TRUE)
}
