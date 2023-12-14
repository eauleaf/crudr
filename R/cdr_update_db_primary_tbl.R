#' Updates the primary db table by specifying value to change,
#'   corresponding db table, unique row ID, and column
#'
#' @param conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table to update, or [cdr_id()] object
#' @param update_value number or string: the value to update already in the correct data type
#' @param value_colname string: the specific column name where the value to update is located
#' @param value_rowuid number or string: the specific row unique ID that corresponds to the row
#'     where the value to update is located (key)
#' @param key_column string: the name of the column with the unique ID (key column)
#'
#' @return TRUE
#'
cdr_update_db_primary_tbl <- function(conn_pool  = conn_pool,
                                      db_tbl_name   = NULL,
                                      update_value  = NULL,
                                      value_colname = NULL,
                                      value_rowuid  = NULL,
                                      key_column    = NULL
){
  cat('\n   --Running: cdr_update_db_primary_tbl()\n')

  db_tbl_name <- cdr_id2sql(db_tbl_name)

  if (!is.na(update_value) && lubridate::is.POSIXct(update_value)){
    update_value <- paste(lubridate::ymd_hms(update_value)) # already UTC
  } else if( rlang::is_logical(update_value) ){
    update_value <- as.character(update_value)
  }


  cat('\nUsing this SQL statement to overwrite data in the DB primary table:\n')
  sql_stmt <- pool::sqlInterpolate(
    conn = conn_pool,
    sql  = glue::glue('
    UPDATE {db_tbl_name}
    SET "{value_colname}" = ?update_value
    WHERE "{key_column}" = ?value_rowuid '
    ),
    .dots = list(
      update_value = update_value,
      value_rowuid = value_rowuid
    ))

  print(sql_stmt)

  success <- pool::dbExecute(conn_pool, sql_stmt)

  if(success){
    cat(
      paste0("\nOverwrote row '",value_rowuid,"' field '",value_colname,"' with '",
             update_value,"' in '",db_tbl_name,"' database\n")
    )
  } else {
    message(glue::glue("Unable to overwrite values in the DB primary table. "))
  }


  return(success)

}
