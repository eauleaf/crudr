#' Removes a specified table by deleting the table or by removing just the table data (truncating)
#'
#' @param conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table, or cdr_id() object
#' @param removal string: one of 'truncate' or 'delete'
#' @param ... other args specifying a DB table such as `schema = 'my_schema'`
#'
#' @return TRUE for success; FALSE for removal fail
#' @export
#'
#' @examples \dontrun{
#'
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'test.db'))
#' example_tbl <- dplyr::mutate(iris, bool = Species == 'setosa', day = Sys.Date(), test = Sys.time())
#' cdr_make_db_tbls(con, example_tbl, chg_log_suffix = '_deltas')
#' con |> DBI::dbListTables()
#' cdr_trunc_tbl(conn_pool = con, db_tbl_name = 'example_tbl', removal = 'truncate')
#' con |> DBI::dbListTables()
#' DBI::dbListTables(con) |> purrr::map(\(.) cdr_trunc_tbl(con, ., 'delete'))
#' con |> DBI::dbListTables()
#' pool::poolClose(con)
#'
#' }
#'
cdr_trunc_tbl<- function(conn_pool, db_tbl_name = NULL, removal = c('truncate','delete'), ...){
  cat('\n--Running: cdr_trunc_tbl()\n')

  if(inherits(db_tbl_name, 'Id')){
    db_table_id <- db_tbl_name
  } else {
    db_table_id <- cdr_id(table=db_tbl_name, ...)
  }
  db_tbl_name <- cdr_id2sql(db_table_id)


  if( !DBI::dbExistsTable(conn = conn_pool, name = db_table_id) ){

    cat("\n\nTable does not exist in database.\n\n")
    return(invisible(FALSE))

  } else if( tolower(removal[1])=='delete' ){

    pool::dbRemoveTable(conn_pool, name = db_table_id)

    cat(glue::glue("\n\nDeleted table {db_tbl_name}:\n\n"))
    return(invisible(TRUE))

  } else if ( tolower(removal[1])=='truncate' ){

    if(any(stringr::str_detect(cdr_which_db(conn_pool), '(?i)sqlite'))){
      trunc_stmt <- glue::glue('DELETE FROM {db_tbl_name}')
    } else {
      trunc_stmt <- glue::glue('TRUNCATE TABLE {db_tbl_name}')
      }

    sql_stmt <- pool::sqlInterpolate(
      conn = conn_pool,
      sql  = trunc_stmt)

    pool::dbExecute(conn_pool, sql_stmt)

    cat(glue::glue("\n\nTruncated table {db_tbl_name} with SQL:\n\n"))
    print(sql_stmt)

    cat('\n')
    return(invisible(TRUE))

  }

}
