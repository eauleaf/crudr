% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_pull_db_tbls.R
\name{cdr_pull_db_tbls}
\alias{cdr_pull_db_tbls}
\title{Collect both tables (admin & chg-log) from the DB, join them in a user view,
and return all 3 tables in a list}
\usage{
cdr_pull_db_tbls(
  conn_pool,
  db_tbl_name,
  chg_log_suffix = "_DELTAS",
  key_field = "UID",
  ...
)
}
\arguments{
\item{conn_pool}{connection object (preferably a \code{\link[pool:dbPool]{pool::dbPool()}} connection)}

\item{db_tbl_name}{name of your primary table}

\item{chg_log_suffix}{name of your change-log table suffix}

\item{key_field}{column in the primary table that acts as the unique row identifier}

\item{...}{other database pointer params like \code{schema}}
}
\value{
a list with primary (admin), change-log, and end-user view tables as list elements
}
\description{
Collect both tables (admin & chg-log) from the DB, join them in a user view,
and return all 3 tables in a list
}
\examples{
\dontrun{
con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "test",
  host = "localhost",
  user = Sys.getenv('postgres_username'),
  password = Sys.getenv('postgres_password')
  )

as_of <- lubridate::now()-lubridate::years(1)
DBI::dbListTables(con)
DBI::dbListObjects(con)
# both_tbls <- cdr_pull_db_tbls(con, db_tbl_name = 'iris', key_field = 'UID', schema = "test")
DBI::dbDisconnect(con)
}

}
