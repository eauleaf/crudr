#' Checks availability of unique ID in DB
#'
#' For a user to create a new row observation, the ID for the unique ID column
#' must not already be used. Function checks user requested unique ID against
#' what's already in the DB
#'
#' @param db_conn_pool pool object for the pool of connections to the specific db
#' @param db_tbl_name char string describing the specific table where the new line is located
#' @param new_uid numeric or string: the new unique ID you'd like to use for the
#'   new observation you're creating; input type must match the db type
#' @param uid_column_name character string describing the unique ID column
#'
#' @return the uid created from the custom uid function
#'
#' @export
#'
#' @importFrom rlang .data
#'
check_unique_id <- function(db_conn_pool, db_tbl_name, new_uid, uid_column_name){

  new_uid <- stringr::str_trim(new_uid)

  if(new_uid == ''){
    return(paste0("Your unique ID entry was blank. To create a new row, please enter a unique ID for field '", uid_column_name,"'."))
  }

  # check that the supplied unique ID is truly unique
  all_uids <-
    dplyr::tbl(db_conn_pool, db_tbl_name) %>%
    dplyr::pull(.data[[uid_column_name]]) %>%
    unique()

  if(new_uid %in% as.character(all_uids)){
    return(paste0("Your entry '",new_uid,"' is not unique. To create a new row, please enter a unique ID for field '", uid_column_name,"'."))
  }

  return(NULL)

}
