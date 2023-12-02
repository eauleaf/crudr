#' Collect both tables (admin & chg-log) from the DB
#'
#' @param conn_pool connection object (preferably a [pool::dbPool()] connection)
#' @param db_tbl_name name of your primary table
#' @param chg_log_suffix name of your change-log table suffix
#' @param key_field column in the primary table that acts as the unique row identifier
#' @param ... other DB params like `schema`
#'
#' @return a list with primary and change-log tables as elements
#' @export
#'
#' @examples \dontrun{
#' con <- DBI::dbConnect(
#'   drv = RPostgres::Postgres(),
#'   dbname = "test",
#'   host = "localhost",
#'   user = Sys.getenv('postgres_username'),
#'   password = Sys.getenv('postgres_password')
#'   )
#'
#' as_of <- lubridate::now()-lubridate::years(1)
#' DBI::dbListTables(con)
#' DBI::dbListObjects(con)
#' # both_tbls <- cdr_pull_db_tbls(con, db_tbl_name = 'iris', key_field = 'UID', schema = "test")
#' DBI::dbDisconnect(con)
#' }
#'
cdr_pull_db_tbls <- function(conn_pool, db_tbl_name, chg_log_suffix = '_DELTAS', key_field = 'UID', ...){

  cat('\n   - Collecting Primary DB table ... ')
  db_tbl <- cdr_DB2RT_primary(conn_pool, db_tbl_name = cdr_id(table = db_tbl_name, ...), key_col = key_field)

  cat('\n   - Collecting Deltas DB table ... ')
  deltas_tbl_name <- cdr_name_delta_tbl(db_tbl_name, chg_log_suffix = chg_log_suffix)
  chg_log_tbl <- cdr_DB2RT_chg_log(conn_pool, chg_log_tbl_name = cdr_id(table = deltas_tbl_name, ...))

  list(db_tbl, chg_log_tbl) |> rlang::set_names(c('primary', 'chg_log'))

}


