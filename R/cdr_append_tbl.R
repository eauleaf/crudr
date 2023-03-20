
#' Appends data from a dataframe into an existing database table with the same structure
#'
#' @param conn_pool a database connection of class 'pool'
#' @param db_tbl the dataframe to append to the database table
#' @param db_tbl_name the name of the database table if different from the name of the dataframe passed to 'db_tbl'
#' @param chunk_size the maximum number of elements you want to pass to the db in one go
#'  (i.e. if you have a huge dataframe with 100 columns and 100,000 rows, a
#'  chunk_size of 10,000 elements would split the dataframe into 1000 groups and
#'  appends each successively.)
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'test.db'))
#' iris_tbl <- dplyr::mutate(iris, bool = Species == 'setosa', day = Sys.Date(), test = Sys.time())
#' pool::dbCreateTable(con,  'iris_tbl', iris_tbl)
#' crudr::cdr_append_tbl(con, iris_tbl)
#' dplyr::tbl(con, 'iris_tbl')
#' pool::dbRemoveTable(con,'iris_tbl')
#' pool::poolClose(con)
#' }
#'
cdr_append_tbl <- function(conn_pool, db_tbl, db_tbl_name = NULL, chunk_size = 10000){

  # if no 'db_tbl_name' supplied, name the table the same name as 'db_tbl'
  if( is.null(db_tbl_name) ){ db_tbl_name <- rlang::as_name(rlang::enquo(db_tbl)) }

  db_tbl <- tibble::tibble(db_tbl)

  # adjust for timezone treatment between databases
  # time_zone <- cdr_adj_timezone(conn_pool)

  # break the dataframe into chunks of elements to execute so you're not passing too much text over to a database
  split_count <- round(chunk_size/ncol(db_tbl))
  chopped_db_tbl <- split(
    x = db_tbl,
    f = rep(1:ceiling(nrow(db_tbl)/split_count), length.out = nrow(db_tbl), each = split_count)
  )
  row_counts <- chopped_db_tbl %>% purrr::map(nrow)


  # convert to ANSI std and append tables
  cat(glue::glue("\nAppending data to table '{db_tbl_name}' with truncated query below.\n\n"))
  sql_queries <- chopped_db_tbl %>%
    purrr::map(., ~dplyr::mutate(., dplyr::across(tidyselect::where(rlang::is_logical), as.character))) %>%
    purrr::map(., ~dplyr::mutate(., dplyr::across(tidyselect::where(lubridate::is.POSIXct),~paste(lubridate::with_tz(.,cdr_adj_timezone(conn_pool)))))) %>%
    purrr::map(., ~pool::sqlAppendTable(DBI::ANSI(), db_tbl_name, .)) %>%
    suppressWarnings()
  cat(paste(stringr::str_extract(sql_queries, '(?:)(.*\\n){5}'),'... etc... \n\n'))

  purrr::map2(.x = sql_queries, .y = row_counts,
             ~ pool::dbExecute(conn = conn_pool, statement = .x) %>%
               cat(glue::glue("\nAppended {.y} rows to table '{db_tbl_name}'\n"))
             )

  cat('\n\n')

}

