#' removes the tables written to a db and closes the connection
#'
#' generally for testing
#'
#' @param conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table the value to update is located in
#' @return NULL
#' @export
#'
#' @examples \dontrun{ reset_db(con, iris) }
cdr_reset_db <- function(conn_pool, db_tbl_name = NULL){
  cat('\n--Running: crudr::cdr_reset_db()\n')

  pool::dbRemoveTable(conn_pool,db_tbl_name)
  pool::dbRemoveTable(conn_pool,crudr::cdr_name_delta_tbl(db_tbl_name))
  pool::poolClose(conn_pool)

}
