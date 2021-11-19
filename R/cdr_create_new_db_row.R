#' Appends new row to primary table
#'
#' create new observation line in primary production database table and insert a unique
#'  ID in the unique observation identifier line
#'  Note: This function is only for the primary table. It is not used on the deltas table.
#'  Note2: Function does not clean or control 'new_uid' inputs. Run your cleaning code before passing in 'new_uid'
#'
#' @param db_conn_pool pool object for the pool of connections to the specific db
#' @param db_row tibble of the row to append
#' @param db_tbl_name char string describing the specific table the new line is located in
#'
#' @return the uid created from the custom uid function
#' @export
#'

cdr_create_new_db_row <- function(db_conn_pool, db_tbl_name, db_row){
  print('cdr_create_new_db_row')

  #prep delta table inputs
  # table_struc <-
  #   dplyr::tbl(db_conn_pool, db_tbl_name) %>%
  #   dplyr::filter(FALSE) %>%
  #   dplyr::collect()
  # to_append <- table_struc %>% dplyr::bind_rows(tibble::tibble({{uid_column_name}} := new_uid))

  pool::dbAppendTable(conn = db_conn_pool, name = db_tbl_name, value = db_row)

  cat(glue::glue("\n\nCreated new record in table '{db_tbl_name}':\n\n"))
  print(db_row)
  return(db_row[[1]])

}
