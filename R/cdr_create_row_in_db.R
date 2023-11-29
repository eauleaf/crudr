#' Appends new row to primary table
#'
#' Create new observation line in primary database table and insert a unique
#'  ID in the unique observation identifier line.
#'  Note: This function is only for the primary table. It is not used for the chg_log table.
#'  Note2: Function does not clean or control 'new_uid' inputs. Run controls code before passing in 'new_uid'
#'
#' @param conn_pool pool object for the pool of connections to the specific db
#' @param db_tbl_name string: name of the specific table to update, or [cdr_id()] object
#' @param key_col name of the unique ID column in the db table (table must have a unique ID column with unique IDs)
#' @param input_uid char string: the new unique ID you'd like to use for the new observation you're creating
#'
#' @return NULL
#'
cdr_create_row_in_db <- function(conn_pool, db_tbl_name, key_col, input_uid){

  db_tbl_name <- cdr_id2sql(db_tbl_name)

  cat('\n--Running: cdr_create_row_in_db()\n')
  cat(glue::glue("\nCreating new record {input_uid} in {key_col} for table {db_tbl_name}:\n"))


  sql_stmt <-
    pool::sqlInterpolate(
      conn = conn_pool,
      sql  = glue::glue('
        INSERT INTO {db_tbl_name}
        ( "{key_col}" )
        VALUES ( ?input_uid )
                        '),
      .dots = list(
        input_uid = input_uid
      ))

  cat(sql_stmt)
  pool::dbExecute(conn_pool, sql_stmt)

}
