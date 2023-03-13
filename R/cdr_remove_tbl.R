#' removes a specified table by deleting the table or by removing just the table data (truncating)
#'
#' @param db_conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table the value to update is located in
#' @param removal string: one of 'delete' or 'truncate'
#'
#' @return Boolean describing success
#' @export
#'
#' @examples \dontrun{cdr_remove_tbl(db_conn_pool = pool_connection_object,
#' db_tbl_name = iris, removal = 'truncate')}
#'
cdr_remove_tbl<- function(db_conn_pool, db_tbl_name = NULL, removal = c('delete', 'truncate')){
  print('cdr_remove_tbl')

  if( tolower(removal[1])=='delete' ){

    pool::dbRemoveTable(db_conn_pool, name = db_tbl_name)

    cat(glue::glue("\n\nDeleted table {db_tbl_name}:\n\n"))
    return(TRUE)

  } else if ( tolower(removal[1])=='truncate' ){

    sql_stmt <- pool::sqlInterpolate(
      conn = db_conn_pool,
      sql  = glue::glue('
        TRUNCATE TABLE "{db_tbl_name}"
        '))

    pool::dbExecute(db_conn_pool, sql_stmt)

    cat(glue::glue("\n\nTruncated table {db_tbl_name}:\n\n"))
    return(TRUE)

  } else {

    cat("\n\nFAILED TO REMOVE TABLE\n\n")
    return(FALSE)

  }


}
