% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_check_unique_id.R
\name{cdr_check_unique_id}
\alias{cdr_check_unique_id}
\title{Checks availability of unique ID in DB}
\usage{
cdr_check_unique_id(db_tbl, input_uid, uid_column_name)
}
\arguments{
\item{db_tbl}{primary table in server memory}

\item{input_uid}{char string: the new unique ID you'd like to use for the
new observation you're creating}

\item{uid_column_name}{character string describing the unique ID column}
}
\value{
text to display in the UI
}
\description{
For a user to create a new row observation, the ID for the unique ID column
must not already be used. Function checks user requested unique ID against
what's already in the DB
}
