#' Download database table and sync it to user interface
#'
#' @param conn_pool pool connection object from package 'pool'
#' @param db_tbl_name string: name of the specific table to update or [cdr_id()] object
#' @param key_col string: the field that contains your unique ID key for each row
#'
#' @return a DT object to present the primary table
#'
#' @examples \dontrun{
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' cdr_make_db_tbls(con, iris)
#' cdr_DB2RT_primary(con, 'iris')
#' pool::dbRemoveTable(con,'iris')
#' pool::dbRemoveTable(con,'iris_DELTAS')
#' pool::poolClose(con)
#' }
#'
cdr_DB2RT_primary <- function(
    conn_pool,
    db_tbl_name,
    key_col
) {

  cat('\n--Running: cdr_DB2RT_primary()\n')

  db_tbl_id <- db_tbl_name
  db_tbl_name <- cdr_id2sql(db_tbl_name)

  cat(paste0('\tDownloading Primary table ',db_tbl_name,' from the database and presenting it in the UI.\n'))
  db_tbl <- dplyr::tbl(conn_pool, db_tbl_id) |> dplyr::collect() |>
    dplyr::relocate(dplyr::all_of({{key_col}})) |> dplyr::arrange(!!rlang::sym({{key_col}}))
  # assign("db_tbl", db_tbl, pos = parent.frame())
  cat(paste("\tUID for table",db_tbl_name,"is:", key_col))
  cat(paste0('\tPrimary DB table for ',db_tbl_name,' passed to server module.\n'))
  print(utils::head(db_tbl,3))
  cat('\n\n')

  return(db_tbl)

}

