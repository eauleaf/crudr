#' Appends new row to primary table
#'
#' create new observation line in primary production database table and insert a unique
#'  ID in the unique observation identifier line
#'  Note: This function is only for the primary table. It is not used on the deltas table.
#'  Note2: Function does not clean or control 'new_uid' inputs. Run your cleaning code before passing in 'new_uid'
#'
#' @param db_conn_pool pool object for the pool of connections to the specific db
#' @param db_tbl_name char string describing the specific table the new line is located in
#' @param key_col name of the unique ID column in the db table (table must have a unique ID column with unique IDs)
#' @param input_uid char string: the new unique ID you'd like to use for the new observation you're creating
#'
#' @export
#'

cdr_create_row_in_db <- function(db_conn_pool, db_tbl_name, key_col, input_uid){

  cat('\n--Running: crudr::cdr_create_row_in_db()\n')
  cat(glue::glue("\nCreating new record {input_uid} in {key_col} for table '{db_tbl_name}':\n"))

  sql_stmt <-
    pool::sqlInterpolate(
      conn = db_conn_pool,
      sql  = glue::glue('
        INSERT INTO "{db_tbl_name}"
        ( "{key_col}" )
        VALUES ( ?input_uid )
                        '),
      .dots = list(
        input_uid = input_uid
      ))

  cat(sql_stmt)
  pool::dbExecute(db_conn_pool, sql_stmt)

}
