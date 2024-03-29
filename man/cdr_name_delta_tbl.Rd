% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_name_delta_tbl.R
\name{cdr_name_delta_tbl}
\alias{cdr_name_delta_tbl}
\title{creates the name of the change tracking table so it's set in one location}
\usage{
cdr_name_delta_tbl(db_tbl_name, chg_log_suffix = "_DELTAS")
}
\arguments{
\item{db_tbl_name}{char string: name of the primary table the deltas table will track}

\item{chg_log_suffix}{if specified, appends your string to your chg_log DB table name
rather than "_DELTAS"}
}
\value{
char string: name of the table to track changes to the primary table
}
\description{
creates the name of the change tracking table so it's set in one location
}
\examples{
  crudr:::cdr_name_delta_tbl('some_table_name')
  crudr:::cdr_name_delta_tbl('some_table_name', chg_log_suffix = '_deltas')
  crudr:::cdr_name_delta_tbl('some_table_name', chg_log_suffix = '_chglog')

}
