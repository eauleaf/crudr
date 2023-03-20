#' Download database change log and sync it to user interface
#'
#' @param conn_pool pool connection object from package 'pool'
#' @param chg_log_tbl_name string: name of the database table you're managing with suffix "_DELTAS"
#'
#' @return a DT object to present the change log table
#' @export
#'
#' @examples \dontrun{cdr_impart_primary_tbl(con, 'iris_DELTAS')}
cdr_impart_chg_log_tbl <-function(
    conn_pool,
    chg_log_tbl_name = crudr::cdr_name_delta_tbl(db_tbl_name)
) {
  cat('\n--Running: crudr::cdr_impart_chg_log_tbl()')

  cat('\n Downloading Deltas table from the database and presenting it in the UI\n')
  chg_log_tbl <<- dplyr::tbl(conn_pool, chg_log_tbl_name) %>%
    dplyr::collect() %>% dplyr::arrange(dplyr::desc(WHEN_EDITED))


  if ( !lubridate::is.POSIXct(chg_log_tbl$WHEN_EDITED) ) {
    chg_log_tbl <<- dplyr::mutate(chg_log_tbl, WHEN_EDITED = lubridate::ymd_hms(WHEN_EDITED, tz=Sys.timezone()))
  }

  proxy_chg_log_tbl <<- DT::dataTableProxy('chg_log_tbl')

  DT::renderDT(
    DT::datatable(chg_log_tbl, selection = 'none') %>%
      DT::formatDate('WHEN_EDITED', method = 'toLocaleString')
  )

}
