#' Checks availability of unique ID in DB
#'
#' For a user to create or delete a new row observation, the ID for the unique ID column
#' must not already be used. Function checks user requested unique ID against
#' what's already in the DB.
#'
#'
#' @param db_tbl tibble object: for primary table in server memory
#' @param input_uid char string: the unique ID you'd like to to add/remove for the
#'   observation you're creating/deleting
#' @param key_column char string: that describes the column or field the UID is in
#' @param chk_for one of 'create' or 'delete': describing whether the user wants to
#'   make a new UID or delete a row denoted by a UID. Create checking is slightly more
#'   restrictive because it's capital insensitive.
#'
#' @return text to display in the UI
#'
cdr_chk_uniq_id <- function(db_tbl, input_uid, key_column, chk_for = c('create', 'delete')){
  cat('\n--Running: cdr_chk_uniq_id()\n')

  chk_for <- tolower(chk_for[1])

  if ( chk_for == 'create' & toupper(input_uid) %in% toupper(db_tbl[[key_column]]) ){
    return(glue::glue('Your entry "{input_uid}" is NOT unique. To CREATE a new row, please enter an ID unique to field \'{key_column}\'.'))
  } else if( chk_for == 'create' & input_uid == '' ){
    return(paste0("Your unique ID entry is blank. To CREATE a new row, please enter a unique ID into field '", key_column,"'."))
  } else if( chk_for == 'delete' & !(input_uid %in% db_tbl[[key_column]] )){
    return(glue::glue('Your entry "{input_uid}" is NOT an ID in field \'{key_column}\'. To delete a row, please type the unique identifier exactly as it is in \'{key_column}\'.'))
  } else {

    return(NULL)

  }

}


# #' @importFrom rlang .data
# #'
# #'
