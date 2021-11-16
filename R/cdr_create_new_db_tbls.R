#' Creates primary and deltas tables in a db
#'
#' instantiates an initial primary table into the database defined by
#' db_conn_pool; writes the initial primary table dataframe to the relational
#' database as well as instantiates the corresponding deltas metadata table for
#' change tracking Note: function should only be called once when setting up a
#' table to track within the db If you need to remove tables, these functions
#' can be helpful: pool::dbListTables(db_conn_pool)
#' pool::dbRemoveTable(db_conn_pool, 'some_table')
#'
#' @param db_conn_pool a connection of class 'pool'
#' @param db_tbl a dataframe; the primary table to place into the database
#'
#' @return NULL; and function cats statements to the console reporting success
#'   or failure
#' @export
#'

cdr_create_new_db_tbls <- function(db_conn_pool, db_tbl){

  db_tbl_name <- rlang::as_name(rlang::enquo(db_tbl))

  if (!pool::dbExistsTable(conn = db_conn_pool, name = db_tbl_name)){

    pool::dbCreateTable(conn = db_conn_pool, name = db_tbl_name, fields = db_tbl)
    pool::dbAppendTable(conn = db_conn_pool, name = db_tbl_name, value = db_tbl)
    reporting <- dplyr::tbl(db_conn_pool, db_tbl_name) %>% utils::head() %>% dplyr::collect()

    cat(glue::glue("\n\nCreated and populated table '{db_tbl_name}' with structure:\n\n"))
    cat(utils::str(reporting))

  } else {

    cat(glue::glue("\n\nUnable to create new table for '{db_tbl_name}'; table already exists in the database.\n"))

  }

  # create change tracking table
  deltas_db_tbl_name <- cdr_deltas_tbl_name(db_tbl_name)
  if(!pool::dbExistsTable(conn = db_conn_pool, name = deltas_db_tbl_name)){

    pool::dbCreateTable(conn = db_conn_pool,
                        name = deltas_db_tbl_name,
                        fields = tibble::tibble(uid = character(),
                                                field = character(),
                                                to = character(),
                                                from = character(),
                                                who = character(),
                                                when = as.POSIXct(NA)))

    reporting <- pool::dbReadTable(db_conn_pool, deltas_db_tbl_name)

    cat(glue::glue("\n\nCreated new change tracking table '{deltas_db_tbl_name}' with structure: \n\n"))
    cat(utils::str(reporting))

  } else {

    cat(glue::glue("\n\nUnable to create new table for '{deltas_db_tbl_name}'; table already exists in the database.\n\n"))

  }

}
