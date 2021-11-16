#' Removes specified row from db table
#'
#' Deletes a row or rows by specifying the unique ids for the rows to delete
#'  Note: not implemented in a UI button, but here for administrator use
#'
#' @param db_conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table the value to update is located in
#' @param value_rowuid number or string: the specific the unique ID that corresponds to the row to delete
#' @param value_rowuid_colname string: the name of the column with the unique ID (key column)
#'
#' @return TRUE for successful deletion
#' @export
#'
#' @examples
#' \dontrun{
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' iris <- dplyr::mutate(iris, unique_id = paste0('uid_',dplyr::row_number()))
#' cdr_create_new_db_tbls(db_conn_pool = con, db_tbl = iris)
#' print(here::here('iris.db'))
#' cdr_delete_db_tbl_row(
#'   db_conn_pool = con,
#'   db_tbl_name = 'iris', #'iris_deltas'
#'   value_rowuid = 'uid_1',
#'   value_rowuid_colname = 'unique_id'
#'   )
#' pool::poolClose(con)
#'}
#'

cdr_delete_db_tbl_row <- function(db_conn_pool         = db_conn_pool,
                                  db_tbl_name          = NULL,
                                  value_rowuid         = NULL,
                                  value_rowuid_colname = NULL){


  # construct sql statement to update table
  sql_stmt <-
    pool::sqlInterpolate(
      conn = db_conn_pool,
      sql  = glue::glue('
    DELETE FROM "{db_tbl_name}"
    WHERE "{value_rowuid_colname}" = ?value_rowuid '),
      .dots = list(
        value_rowuid = value_rowuid
      ))

  success <- pool::dbExecute(db_conn_pool, sql_stmt)

  if(success){
    cat(glue::glue("\n\nDeleted observations identified by '{value_rowuid}' from '{db_tbl_name}'\n\n"))
  } else {
    message(glue::glue("Unable to delete observations identified by '{value_rowuid}' from '{db_tbl_name}'. "))
  }

  success

}
