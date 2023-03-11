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


  db_tbl_name <- rlang::as_name(rlang::enquo(db_tbl))


  if( pool::dbExistsTable(conn = db_conn_pool, name = db_tbl_name) ){

    cat(glue::glue("\n\nDid not create new table for '{db_tbl_name}'. Table already exists in the database.\n\n"))

  } else {

    cat(glue::glue("\nCreating and populating table '{db_tbl_name}'\n"))

    if( is.null(key_field) ) {
      message('\nNo key Unique Key field specified.')
      db_tbl <- cdr_make_unique_ids(db_tbl)
    } else if ( !key_field %in% names(db_tbl) ) {
      message(glue::glue('\nSpecified field \'{key_field}\' does not exist.'))
      db_tbl <- cdr_make_unique_ids(db_tbl)
    } else if ( anyDuplicated(db_tbl[[key_field]])) {
      message(glue::glue('\nKey values in field \'{key_field}\' are not unique.'))
      db_tbl <- cdr_make_unique_ids(db_tbl)
    }

    # create a database
    pool::dbCreateTable(conn = db_conn_pool, name = db_tbl_name, fields = db_tbl)

    # break the dataframe into chunks of 10,000 elements
    split_count <- round(10000/ncol(db_tbl))
    split_db_tbl <- split(
      x = db_tbl,
      f = rep(1:ceiling(nrow(db_tbl)/split_count), length.out = nrow(db_tbl), each = split_count)
    )

    # append tables
    cat(glue::glue("\nAppending data to table '{db_tbl_name}'.\n\n"))
      sql_queries <- split_db_tbl %>%
        purrr::map(., ~dplyr::mutate(., dplyr::across(dplyr::where(lubridate::is.POSIXct),as.character))) %>%
        purrr::map(., ~pool::sqlAppendTable(DBI::ANSI(), db_tbl_name, .))
      print(sql_queries)
      purrr::map(sql_queries, ~pool::dbExecute(conn = db_conn_pool, statement = .))

    # pool::dbAppendTable(conn = db_conn_pool, name = db_tbl_name, value = db_tbl)

  }


  # create change tracking table
  chg_log_tbl_name <- crudr::cdr_name_delta_tbl(db_tbl_name)

  if( pool::dbExistsTable(conn = db_conn_pool, name = chg_log_tbl_name) ){

    cat(glue::glue("\nDid not create new table for '{chg_log_tbl_name}'. Table already exists in the database.\n\n"))

  } else {

    cat(glue::glue("\nCreating new change tracking table '{chg_log_tbl_name}'\n\n"))

    delta_tbl <- tibble::tibble(
      OBS_ID = character(),
      FIELD = character(),
      CHG_FROM = character(),
      CHG_TO = character(),
      WHO_EDITED = character(),
      WHEN_EDITED = as.POSIXct(NA)
    )

    pool::dbCreateTable(db_conn_pool, chg_log_tbl_name, delta_tbl)

  }


}


# tests
