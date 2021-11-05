#' create new observation line in primary production database table and insert a unique
#'  ID in the unique observation identifier line
#'  Note: This function is only for the primary table. It is not used on the deltas table.
#'  Note2: Function does not clean or control 'new_uid' inputs. Run your cleaning code before passing in 'new_uid'
#'
#' @param db_conn_pool pool object for the pool of connections to the specific db
#' @param db_tbl_name char string describing the specific table the new line is located in
#' @param new_uid numeric or string: the new unique ID you'd like to use for the new observation you're creating; input type must match the db type
#' @param uid_column_name character string describing the unique ID column
#'
#' @return the uid created from the custom uid function
#' @export
#'

create_new_db_row <- function(db_conn_pool, db_tbl_name, new_uid, uid_column_name){

  # check that the supplied unique ID is truly unique
  all_uids <-
    dplyr::tbl(db_conn_pool, db_tbl_name) %>%
    dplyr::pull(.data[[uid_column_name]]) %>%
    unique()

  if(any(new_uid %in% all_uids)){
    stop("The new unique ID is not unique. Have another go at creating a unique one.")
  }

  #prep delta table inputs
  table_struc <-
    dplyr::tbl(db_conn_pool, db_tbl_name) %>%
    dplyr::filter(FALSE) %>%
    dplyr::collect()


  # append the new ID onto the database table
  to_append <- table_struc %>% bind_rows(tibble::tibble({{uid_column_name}} := new_uid))
  pool::dbAppendTable(conn = db_conn_pool, name = db_tbl_name, value = to_append)

  cat(glue::glue("\n\nCreated new record in table '{db_tbl_name}' with '{new_uid}' in field '{uid_column_name}'\n"))

  return(new_uid)

}
