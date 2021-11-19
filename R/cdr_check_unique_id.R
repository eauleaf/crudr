#' Checks availability of unique ID in DB
#'
#' For a user to create a new row observation, the ID for the unique ID column
#' must not already be used. Function checks user requested unique ID against
#' what's already in the DB
#'
#' @param db_tbl primary table in server memory
#' @param uid_column_name name of the specific column the UID is in
#' @param input_uid char string: the new unique ID you'd like to use for the
#'   new observation you're creating
#' @param uid_column_name character string describing the unique ID column
#'
#' @return text to display in the UI
#'
#' @export
#'
#' @importFrom rlang .data
#'
cdr_check_unique_id <- function(db_tbl, input_uid, uid_column_name){
  print('cdr_check_unique_id')

  if(input_uid == ''){
    return(paste0("Your unique ID entry was blank. To create a new row, please enter a unique ID for field '", uid_column_name,"'."))
  }

  if(input_uid %in% db_tbl[[uid_column_name]]){
    return(paste0("Your entry '",input_uid,"' is not unique. To create a new row, please enter a unique ID for field '", uid_column_name,"'."))
  }

  return(NULL)

}
