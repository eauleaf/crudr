#' updates the primary db table by specifying value to change, corresponding db table, unique row ID, and column
#'
#' @param db_conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table the value to update is located in
#' @param update_value number or string: the value to update already in the correct data type
#' @param value_colname string: the specific column name where the value to update is located
#' @param value_rowuid number or string: the specific row unique ID that corresponds to the row where the value to update is located (key)
#' @param value_rowuid_colname string: the name of the column with the unique ID (key column)
#'
#' @return 'old_value': number or strings previously held in the database prior to replacement
#' @export
#'

update_primary_tbl <- function(db_conn_pool         = db_conn_pool,
                               db_tbl_name          = NULL,
                               update_value         = NULL,
                               value_colname        = NULL,
                               value_rowuid         = NULL,
                               value_rowuid_colname = NULL
){


  #prep delta table inputs
  old_value <- dplyr::tbl(db_conn_pool, db_tbl_name) %>%
    filter(!!as.symbol(value_rowuid_colname) == value_rowuid) %>% #show_query()
    pull({{value_colname}})

  # some simple checks
  if (length(old_value) > 1){stop(glue::glue("The field {value_rowuid_colname} isn't a unique identifier in database table {db_tbl_name}"))}
  if (length(old_value) < 1){stop(glue::glue("Cannot find unique ID {value_rowuid} in field {value_rowuid_colname} within table {db_tbl_name}"))}

  # construct sql statement to update primary table
  sql_stmt <-
    pool::sqlInterpolate(
      conn = db_conn_pool,
      sql  = glue::glue('
    UPDATE "{db_tbl_name}"
    SET "{value_colname}" = ?update_value
    WHERE "{value_rowuid_colname}" = ?value_rowuid '),
      .dots = list(
        update_value = update_value,
        value_rowuid = value_rowuid
      ))

  success <- pool::dbExecute(db_conn_pool, sql_stmt)

  if(!success){stop("Not able to write the information to the database. Please try again.")}

  return(old_value)

}
