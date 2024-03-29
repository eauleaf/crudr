% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_make_db_tbls.R
\name{cdr_make_db_tbls}
\alias{cdr_make_db_tbls}
\title{Creates primary and deltas tables in a DB}
\usage{
cdr_make_db_tbls(
  conn_pool,
  db_tbl,
  key_field = NULL,
  make_deltas_tbl = TRUE,
  chg_log_suffix = "_DELTAS",
  ...
)
}
\arguments{
\item{conn_pool}{a database connection of class 'pool'}

\item{db_tbl}{a dataframe; the primary table to place into the database}

\item{key_field}{string: a unique ID column name in the data. If NULL,
creates a key field called \code{UID}}

\item{make_deltas_tbl}{set to FALSE if you only want to write a primary table
without its corresponding change log table}

\item{chg_log_suffix}{to provide a suffix other than the default '_DELTAS'
when creating your database change-log table}

\item{...}{additional table name parameters passed to \code{\link[=cdr_id]{cdr_id()}}, e.g. table =
"mytable", cluster = "mycluster", catalog = "mycatalog", schema = "myschema".
Passing \verb{table = }, specifies the table name.}
}
\value{
invisibly returns NULL
}
\description{
Writes an initial primary table into the database specified by conn_pool, as
well as the corresponding deltas metadata table required for change tracking.
}
\details{
Note: if you want to remove your table, these functions can be helpful:
\itemize{
\item pool::dbListTables(conn_pool)
\item pool::dbRemoveTable(conn_pool, cdr_id(table = 'some_table'))
}
}
\examples{
\dontrun{
 con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
 cdr_make_db_tbls(con, iris)
 pool::dbListTables(conn_pool)
 pool::dbRemoveTable(con, 'iris')
 pool::dbRemoveTable(con, 'iris_deltas')

 con <- DBI::dbConnect(
 RPostgres::Postgres(),
 host = "localhost",
 dbname = "PG_DEV",
 user = Sys.getenv("postgres_username"),
 password = Sys.getenv("postgres_password")
 )

 cdr_make_db_tbls(con, iris, schema = 'test')

}

}
