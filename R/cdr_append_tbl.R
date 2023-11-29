#' Appends data from a dataframe into an existing database table
#'
#' Requires the dataframe and database table to have the same structure
#'
#' @param db_tbl the dataframe to append to the database table
#' @param conn_pool a database connection of class `pool` or `DBI`
#' @param db_tbl_name the name of the database table if different from the name
#'   of the dataframe passed to 'db_tbl'; can alternatively accept a [DBI::Id()] or object
#' @param chunk_size the maximum number of elements you want to pass to the DB in one go
#'  (i.e. if you have a huge dataframe with 100 columns and 10,000 rows, a
#'  chunk_size of 1000 elements would split the dataframe into 1000 groups and
#'  append each group successively.)
#' @param ... other args specifying a DB table such as `schema = 'my_schema'`
#'
#' @return invisibly returns the SQL INSERT statements
#' @export
#'
#' @examples \dontrun{
#'
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'test.db'))
#' example_tbl <- dplyr::mutate(iris, bool = Species == 'setosa', day = Sys.Date(), test = Sys.time())
#' pool::dbCreateTable(con, 'example_tbl', example_tbl)
#' cdr_append_tbl(example_tbl, con)
#' dplyr::tbl(con, 'example_tbl')
#' pool::dbRemoveTable(con,'example_tbl')
#' pool::poolClose(con)
#'
#' }
#'
cdr_append_tbl <- function(db_tbl, conn_pool, db_tbl_name = NULL, chunk_size = 1000, ...){


  if( is.null(db_tbl_name) ){
  # if no 'db_tbl_name' supplied, use the user's "db_tbl" name
    db_table_id <- cdr_id(table=rlang::as_name(rlang::enquo(db_tbl)), ...)
  } else if(inherits(db_tbl_name, 'Id')){
    # if already a cdr_id(), then use that
    db_table_id <- db_tbl_name
  } else {
    # otherwise, it's a string
    db_table_id <- cdr_id(table=db_tbl_name, ...)
  }
  # full table description name
  db_tbl_name <- DBI::dbQuoteIdentifier(DBI::ANSI(), db_table_id)



  # break up and prep for ANSI standard formatting
  chunked_df <- db_tbl |>
    cdr_tbl2ansi_format(tzone = cdr_adj_timezone(conn_pool)) |>
    cdr_chunk_tbl(chunk_size = chunk_size)


  # write SQL statements to append tables
  cat(glue::glue("Appending data to the database table {db_tbl_name} with the SQL statement truncated below:"),fill = TRUE)
  sql_queries <- chunked_df |>
    purrr::map(\(.) pool::sqlAppendTable(DBI::ANSI(), table=db_table_id, values = .)) |>
    suppressWarnings()
  cat(paste(stringr::str_extract(paste(sql_queries[[1]],'\n'), '(?:.*\\n){1,5}'),'... \n\n'))

  row_counts <- chunked_df |> purrr::map(nrow)
  purrr::map2(.x = sql_queries, .y = row_counts,
              .f = \(.x,.y) {
                cat('Sending data chunk to database ...', fill = TRUE)
                pool::dbExecute(conn = conn_pool, statement = .x)
                cat(glue::glue("Appended {.y} rows to database table {db_tbl_name}."), fill = TRUE)
              })


  return(invisible(sql_queries))

}
