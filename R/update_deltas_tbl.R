#' Connects to and posts who-what-when data to a change tracking table
#'  Function is called when a user updates the data in a primary table
#'
#' @param db_conn_pool see update_primary_tbl() definitions
#' @param db_tbl_name see update_primary_tbl() definitions
#' @param old_value the value that was overwritten in the primary table
#' @param update_value the value that overwrote 'old_value' in the primary table
#' @param value_colname see update_primary_tbl() definitions
#' @param value_rowuid see update_primary_tbl() definitions
#'
#' @return an in-memory copy of the data just appended to the tracking table
#' @export
#'

update_deltas_tbl <- function(db_conn_pool,
                              db_tbl_name,
                              old_value,
                              update_value,
                              value_colname,
                              value_rowuid,
                              who = NULL
){

  to_deltas_tbl <- tibble::tibble(uid = value_rowuid,
                                  field = value_colname,
                                  to = as.character(update_value),
                                  from = as.character(old_value),
                                  who = ifelse(is.null(who), "test_user", Sys.info()[['user']]),
                                  when = lubridate::now())
  pool::dbAppendTable(db_conn_pool, db_tbl_name, to_deltas_tbl)

  return(to_deltas_tbl)

}
