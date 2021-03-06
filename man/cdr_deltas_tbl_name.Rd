% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_deltas_tbl_name.R
\name{cdr_deltas_tbl_name}
\alias{cdr_deltas_tbl_name}
\title{creates the name of the change tracking table so it's set in one location}
\usage{
cdr_deltas_tbl_name(db_tbl_name)
}
\arguments{
\item{db_tbl_name}{char string: name of the primary table the deltas table will track}
}
\value{
char string: name of the table to track changes to the primary table
}
\description{
creates the name of the change tracking table so it's set in one location
}
\examples{
\dontrun{
deltas_tbl_name('some_table_name')
}
}
