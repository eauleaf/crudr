#' Removes specified row from db table
#'
#' Deletes a row or rows by specifying the unique ids for the rows to delete
#'  Note: not implemented in a UI button, but here for administrator use
#'
#' @param db_conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table the value to update is located in
#' @param value_rowuid number or string: the specific the unique ID that corresponds to the row to delete
#' @param key_column string: the name of the column with the unique ID
#'
#' @return TRUE for successful deletion
#' @export
#'
#' @examples
#' \dontrun{
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' iris <- dplyr::mutate(iris, unique_id = paste0('uid_',dplyr::row_number()))
#' crudr::cdr_create_tbls_in_db(db_conn_pool = con, db_tbl = iris)
#' print(here::here('iris.db'))
#' crudr::cdr_delete_row_in_db(
#'   db_conn_pool = con,
#'   db_tbl_name = 'iris', #'iris_deltas'
#'   value_rowuid = 'uid_1',
#'   key_column = 'unique_id'
#'   )
#' pool::poolClose(con)
#'}
#'

cdr_delete_row_in_db <- function(db_conn_pool = db_conn_pool,
                                  db_tbl_name  = NULL,
                                  value_rowuid = NULL,
                                  key_column   = NULL){
  cat('\n--Running: crudr::cdr_delete_row_in_db()\n')

  cat('\n\nUsing this SQL statement to delete a row observation from the DB primary table:\n')
  sql_stmt <- pool::sqlInterpolate(
    conn = db_conn_pool,
    sql  = glue::glue('
    DELETE FROM "{db_tbl_name}"
    WHERE "{key_column}" = ?value_rowuid '),
    .dots = list(
      value_rowuid = value_rowuid
    ))
  print(sql_stmt)

  success <- pool::dbExecute(db_conn_pool, sql_stmt)

  if(success){
    cat(glue::glue("\n\nDeleted observations identified by '{value_rowuid}' from '{db_tbl_name}'\n\n"))
  } else {
    message(glue::glue("Unable to delete observations identified by '{value_rowuid}' from '{db_tbl_name}'. "))
  }


  return(success)

}
