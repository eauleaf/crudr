#' Given a `DBI` or `pool` connection, provides DB type
#'
#' @param conn pool or DBI connection to a db
#'
#' @return string representing db type
#'
#' @examples \dontrun{
#' # create some connections -------------------------------------------------
#' pg_dbi_conn <- DBI::dbConnect( RPostgreSQL::PostgreSQL(),
#'                         dbname = "test",
#'                         host = "localhost",
#'                         port = "5432",
#'                         user = Sys.getenv('postgres_username'),
#'                         password = Sys.getenv('postgres_password')
#' )
#' pg_dbi_conn |> cdr_which_db()
#' pg_dbi_conn |> pool::dbGetQuery('SELECT CURRENT_TIMESTAMP') #In Client TIMEZONE
#' pg_dbi_conn |> pool::dbGetQuery('SELECT LOCALTIMESTAMP') #In DB TIMEZONE
#' pg_dbi_conn |> DBI::dbDisconnect()
#'
#' pg_pool_conn <- pool::dbPool(pg_dbi_conn)
#' pg_pool_conn |> cdr_which_db()
#' pg_pool_conn |> pool::dbGetQuery('SELECT CURRENT_TIMESTAMP') #In Client TIMEZONE
#' pg_pool_conn |> pool::dbGetQuery('SELECT LOCALTIMESTAMP') #In DB TIMEZONE
#' pg_pool_conn |> pool::poolClose()
#'
#' sqlite_dbi_mem_conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' sqlite_dbi_mem_conn |> cdr_which_db()
#' sqlite_dbi_mem_conn |> DBI::dbGetQuery('SELECT CURRENT_TIMESTAMP') #UTC
#' sqlite_dbi_mem_conn |> DBI::dbGetQuery('SELECT LOCALTIMESTAMP') # no go
#' sqlite_dbi_mem_conn |> DBI::dbDisconnect()
#'
#' sqlite_dbi_conn <- DBI::dbConnect(RSQLite::SQLite(), 'test.db')
#' sqlite_dbi_conn |> cdr_which_db()
#' sqlite_dbi_conn |> DBI::dbGetQuery('SELECT CURRENT_TIMESTAMP') #UTC
#' sqlite_dbi_conn |> DBI::dbGetQuery('SELECT LOCALTIMESTAMP') # no go
#' sqlite_dbi_conn |> DBI::dbDisconnect()
#'
#' sqlite_pool_conn <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'test.db'))
#' sqlite_pool_conn |> cdr_which_db()
#' sqlite_pool_conn |> DBI::dbDisconnect()
#' sqlite_pool_conn |> pool::poolClose()
#'
#' Sys.time() |> lubridate::with_tz(tzone = 'UTC')
#'
#' }
#'
cdr_which_db <- function(conn){

  # take care of pool objects
  if(methods::is(conn)[[1]] == "Pool") {
    db <- conn$objClass[[1]]
    db <- c(
      conn$objClass[[1]],
      attr(conn$objClass,'package')
    )
  } else {
    db <-   c(
    # this is for DBI using odbc
    class(conn)[[1]],
    # this is using rpostgres on local machine
    attr(class(conn), 'package')
    )
  }

  return(db)
  # db <- db[stringr::str_detect(db, "(?i)\\.GlobalEnv|PqConnection", negate = TRUE)]
  # pos_options[str_detect(pos_options, "(?i)postgres|snowflake|sqlite|ssms")]

}


