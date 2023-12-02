#' Download database change log and send it to the module server
#'
#' @param conn_pool pool connection object from package 'pool'
#' @param chg_log_tbl_name string: name of the database table you're managing with suffix "_DELTAS"
#'
#' @return a DT object to present the change log table
#'
#' @examples \dontrun{
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' cdr_make_db_tbls(con, iris)
#' cdr_DB2RT_primary(con, 'iris_DELTAS')
#' pool::dbRemoveTable(con,'iris')
#' pool::dbRemoveTable(con,'iris_DELTAS')
#' pool::poolClose(con)
#' }
#'
cdr_DB2RT_chg_log <-function(
    conn_pool,
    chg_log_tbl_name
) {
  cat('\n--Running: cdr_DB2RT_chg_log()', fill = TRUE)

  WHEN_EDITED <- NULL
  chg_log_tbl_id <- chg_log_tbl_name
  chg_log_tbl_name <- cdr_id2sql(chg_log_tbl_name)


  cat(paste0('\n\tDownloading the deltas table ',chg_log_tbl_name,
             ' from the database ... \n'))
  chg_log_tbl <- dplyr::tbl(conn_pool, chg_log_tbl_id) |>
    dplyr::collect()  |> dplyr::arrange(dplyr::desc(WHEN_EDITED))
  if ( !lubridate::is.POSIXct(chg_log_tbl$WHEN_EDITED) ) {
    chg_log_tbl <- dplyr::mutate(
      chg_log_tbl,
      WHEN_EDITED = lubridate::ymd_hms(WHEN_EDITED, tz=Sys.timezone()))
  }

  cat('\tChange-log table pulled from DB:\n')
  print(utils::head(chg_log_tbl,3))
  cat('\n\n')

  return(chg_log_tbl)


}
