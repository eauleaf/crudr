#' Creates primary and deltas tables in a DB
#'
#' Writes an initial primary table into the database specified by conn_pool, as
#' well as the corresponding deltas metadata table required for change tracking.
#'
#' Note: if you want to remove your table, these functions can be helpful:
#' - pool::dbListTables(conn_pool)
#' - pool::dbRemoveTable(conn_pool, cdr_id(table = 'some_table'))
#'
#' @param db_tbl a dataframe; the primary table to place into the database
#' @param conn_pool a database connection of class 'pool'
#' @param key_field string: a unique ID column name in the data. If NULL,
#'   creates a key field called `UID`
#' @param make_deltas_tbl set to FALSE if you only want to write a primary table
#'   without its corresponding change log table
#' @param chg_log_suffix to provide a suffix other than the default '_DELTAS'
#'   when creating your database change-log table
#' @param ... additional table name parameters passed to [cdr_id()], e.g. table =
#'   "mytable", cluster = "mycluster", catalog = "mycatalog", schema = "myschema".
#'   Passing `table = `, specifies the table name.
#'
#' @return invisibly returns NULL
#' @export
#'
#' @examples \dontrun{
#'  con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#'  cdr_make_db_tbls(con, iris)
#'  pool::dbListTables(conn_pool)
#'  pool::dbRemoveTable(con, 'iris')
#'  pool::dbRemoveTable(con, 'iris_deltas')
#'}
#'
cdr_make_db_tbls <- function(
    conn_pool, db_tbl, key_field = NULL,
    make_deltas_tbl = TRUE,
    chg_log_suffix = '_DELTAS',
    ... ){
  cat('\n--Running: cdr_make_db_tbls()\n')

  stor_sym <- rlang::enquo(db_tbl)
  id_params_list <- list(...)

  db_tbl <- tibble::tibble(db_tbl)


  # capture & prep info for cdr_id() if exists
  if(!'table' %in% names(id_params_list)){
    id_params_list <- list(table = rlang::as_name(stor_sym)) |>
      utils::modifyList(id_params_list)
  }
  chg_log_id <-  id_params_list
  chg_log_id$table <- cdr_name_delta_tbl(id_params_list$table, chg_log_suffix = chg_log_suffix)

  primary_id <- cdr_id(!!!id_params_list)
  chg_log_id <- cdr_id(!!!chg_log_id)
  db_tbl_name <- DBI::dbQuoteIdentifier(DBI::ANSI(), primary_id)
  chg_log_tbl_name <- DBI::dbQuoteIdentifier(DBI::ANSI(), chg_log_id)


  if( pool::dbExistsTable(conn = conn_pool, name = primary_id) ){
    cat(glue::glue("\n\nDid not create new table for {db_tbl_name}. ",
                   "Table already exists in the database.\n\n"))
  } else {

    cat(glue::glue("\nTable to write {db_tbl_name} has format:\n\n"))
    print(utils::head(db_tbl,3))
    cat(glue::glue("\nCreating and populating table {db_tbl_name}\n\n"))
    if( is.null(key_field) ) {
      message('\nNo Unique-key field specified. Creating one.')
      db_tbl <- cdr_make_unique_ids(db_tbl)
    } else if ( !key_field %in% names(db_tbl) ) {
      cli::cli_abort(glue::glue('\nSpecified field \'{key_field}\' does not exist.'))
    } else if ( anyDuplicated(db_tbl[[key_field]])) {
      cli::cli_abort(glue::glue('\nKey values in field \'{key_field}\' are not unique.'))
      # message(glue::glue('\nKey values in field \'{key_field}\' are not unique.'))
      # db_tbl <- cdr_make_unique_ids(db_tbl)
    } else if ( !is.character(db_tbl[[key_field]]) ) {
      message(glue::glue('\nKey field \'{key_field}\' is not a varchar. Casting Key_field as a character vector.'))
      db_tbl[[key_field]] <- as.character(db_tbl[[key_field]])
    }


    cat(glue::glue("\nCreating database table {db_tbl_name}.\n\n"))
    pool::dbCreateTable(conn = conn_pool, name = primary_id, fields = db_tbl)
    cdr_append_tbl(db_tbl = db_tbl, conn_pool = conn_pool, db_tbl_name = primary_id)


  }

  # return early if user specified `make_deltas_tbl = F`
  if(!make_deltas_tbl){
    message('Parameter `make_deltas_tbl` is FALSE. No change-log table created.')
    return(invisible())
    }

  # create change tracking table
  if( pool::dbExistsTable(conn = conn_pool, name = chg_log_id) ){
    cat(glue::glue("\nDid not create new table for {chg_log_tbl_name}. ",
                   "Table already exists in the database.\n\n"))
  } else {

    cat(glue::glue("\nCreating change tracking table {chg_log_tbl_name} with structure: '\n\n"), fill = TRUE)

    delta_tbl <- tibble::tibble(
      OBS_ID = character(),
      FIELD = character(),
      CHG_FROM = character(),
      CHG_TO = character(),
      WHO_EDITED = character(),
      WHEN_EDITED = as.POSIXct(NA)
      # WHEN_EDITED = lubridate::with_tz(as.POSIXct(NA), cdr_db_tzone(conn_pool))
    )

    pool::dbCreateTable(conn = conn_pool, name = chg_log_id, fields = delta_tbl)
    print(delta_tbl)

  }

  return(invisible())


}


