#' Creates primary and deltas tables in a db
#'
#' creates an initial primary table into the database specified by
#' db_conn_pool; as well as the corresponding deltas metadata table for
#' the change tracking
#'
#' Note: if you want to remove your table, these functions can be helpful:
#' pool::dbListTables(db_conn_pool)
#' pool::dbRemoveTable(db_conn_pool, 'some_table')
#'
#' @param db_conn_pool a database connection of class 'pool'
#' @param db_tbl a dataframe; the primary table to place into the database
#' @param key_field string: a unique ID column name in the data. If Null, creates a proxy
#'
#' @export


cdr_create_tbls_in_db <- function(db_conn_pool, db_tbl, key_field = NULL){
  cat('\n--Running: crudr::cdr_create_tbls_in_db()\n')

  db_tbl_name <- rlang::as_name(rlang::enquo(db_tbl))


  if( pool::dbExistsTable(conn = db_conn_pool, name = db_tbl_name) ){

    cat(glue::glue("\n\nDid not create new table for '{db_tbl_name}'. Table already exists in the database.\n\n"))

  } else {

    cat(glue::glue("\nCreating and populating table '{db_tbl_name}'\n\n"))

    if( is.null(key_field) ) {
      message('\nNo key Unique Key field specified.')
      db_tbl <- crudr::cdr_make_unique_ids(db_tbl)
    } else if ( !key_field %in% names(db_tbl) ) {
      message(glue::glue('\nSpecified field \'{key_field}\' does not exist.'))
      db_tbl <- crudr::cdr_make_unique_ids(db_tbl)
    } else if ( anyDuplicated(db_tbl[[key_field]])) {
      message(glue::glue('\nKey values in field \'{key_field}\' are not unique.'))
      db_tbl <- crudr::cdr_make_unique_ids(db_tbl)
    } else if ( !is.character(db_tbl[[key_field]]) ) {
      message(glue::glue('\nKey field \'{key_field}\' is not a varchar. Casting Key_field as a character vector.'))
      db_tbl[[key_field]] <- as.character(db_tbl[[key_field]])
    }


    cat(glue::glue("\nCreating database table '{db_tbl_name}'.\n\n"))
    pool::dbCreateTable(conn = db_conn_pool, name = db_tbl_name, fields = db_tbl)

    # break the dataframe into chunks of 10,000 elements
    split_count <- round(10000/ncol(db_tbl))
    split_db_tbl <- split(
      x = db_tbl,
      f = rep(1:ceiling(nrow(db_tbl)/split_count), length.out = nrow(db_tbl), each = split_count)
    )

    # append tables
    cat(glue::glue("\nAppending data to table '{db_tbl_name}' with truncated query below.\n\n"))
    sql_queries <- split_db_tbl %>%
      purrr::map(., ~dplyr::mutate(., dplyr::across(tidyselect::where(lubridate::is.POSIXct),~as.character(lubridate::with_tz(.,'UTC'))))) %>%
      purrr::map(., ~pool::sqlAppendTable(DBI::ANSI(), db_tbl_name, .)) %>%
      suppressWarnings()
    cat(paste(stringr::str_extract(sql_queries, '(?:)(.*\\n){5}'),'... etc... \n\n'))
    purrr::map(sql_queries, ~pool::dbExecute(conn = db_conn_pool, statement = .))



  }


  # create change tracking table
  chg_log_tbl_name <- crudr::cdr_name_delta_tbl(db_tbl_name)

  if( pool::dbExistsTable(conn = db_conn_pool, name = chg_log_tbl_name) ){

    cat(glue::glue("\nDid not create new table for '{chg_log_tbl_name}'. Table already exists in the database.\n\n"))

  } else {

    cat(glue::glue("\nCreating new change tracking table '{chg_log_tbl_name} from: '\n\n"))

    delta_tbl <- tibble::tibble(
      OBS_ID = character(),
      FIELD = character(),
      CHG_FROM = character(),
      CHG_TO = character(),
      WHO_EDITED = character(),
      WHEN_EDITED = lubridate::with_tz(as.POSIXct(NA), 'UTC')
    )

    pool::dbCreateTable(db_conn_pool, chg_log_tbl_name, delta_tbl)
    print(delta_tbl)

  }


}


