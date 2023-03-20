#' Populates deltas table when user updates primary table
#'
#' Connects to and posts who-what-when data to a change tracking table
#'  Function is called when a user updates the data in a primary table
#'
#' @param conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table the value to update is located in
#' @param to_deltas_tbl tibble of delta values to append
#'
#' @return an in-memory copy of the data just appended to the tracking table
#' @export
#'

cdr_update_db_deltas_tbl <- function(conn_pool,
                                     db_tbl_name,
                                     to_deltas_tbl){

  cat('\n--Running: crudr::cdr_update_db_deltas_tbl()\n')


  datetime_type <- dplyr::case_when(
    stringr::str_detect(conn_pool$objClass[[1]],"(?i)SQLite") ~ "DATETIME(?when)",
    stringr::str_detect(conn_pool$objClass[[1]],"(?i)postgres") ~ glue::glue("TIMESTAMP ?when AT TIME ZONE '{cdr_adj_timezone(conn_pool)}'"),
    stringr::str_detect(conn_pool$objClass[[1]],"(?i)Snowflake") ~ "TO_TIMESTAMP(?when)",
    TRUE ~ "TIMESTAMP ?when "
    )


  cat('\nUsing this SQL statement to append fields to the DB deltas table:\n')
  sql_stmt <-
    pool::sqlInterpolate(
      conn = conn_pool,
      sql  = glue::glue('
      INSERT INTO "{db_tbl_name}"
      ("OBS_ID","FIELD","CHG_FROM","CHG_TO","WHO_EDITED","WHEN_EDITED" )
      VALUES (
        ?uid,
        ?field,
        ?from,
        ?to,
        ?who,
        {datetime_type} )'),
      .dots = list(
         uid   = to_deltas_tbl$OBS_ID
        ,field = to_deltas_tbl$FIELD
        ,from  = to_deltas_tbl$CHG_FROM
        ,to    = to_deltas_tbl$CHG_TO
        ,who   = to_deltas_tbl$WHO_EDITED
        ,when  = paste(lubridate::with_tz(
          to_deltas_tbl$WHEN_EDITED, crudr::cdr_adj_timezone(conn_pool))
          )
      ))

  print(sql_stmt)

  pool::dbExecute(conn_pool, sql_stmt)


}




