#' Populates deltas table when user updates primary table
#'
#' Connects to and posts who-what-when data to a change tracking table
#'  Function is called when a user updates the data in a primary table
#'
#' @param conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific database table or [cdr_id()] object
#' @param to_deltas_tbl tibble of delta values to append
#' @param db_tzone the DB timezone (obtained from `cdr_adj_timezone(conn_pool)`)
#'
#' @return an in-memory copy of the data appended to the tracking table
#'
cdr_update_db_deltas_tbl <- function(conn_pool,
                                     db_tbl_name,
                                     to_deltas_tbl,
                                     db_tzone = cdr_adj_timezone(conn_pool)
                                     ){

  cat('\n--Running: cdr_update_db_deltas_tbl()\n')

  db_tbl_name <- cdr_id2sql(db_tbl_name)
  db_type <- cdr_which_db(conn_pool)

  if( any(stringr::str_detect(db_type,"(?i)postgres")) ){
    datetime_type <- glue::glue("TIMESTAMP ?when AT TIME ZONE '{db_tzone}'")
  } else if( any(stringr::str_detect(db_type,"(?i)SQLite")) ){
    datetime_type <- "DATETIME(?when)"
  } else if( any(stringr::str_detect(db_type,"(?i)Snowflake")) ){
    datetime_type <- "TO_TIMESTAMP(?when)"
  } else {
    datetime_type <- "TIMESTAMP ?when "
  }


  cat('\nUsing this SQL statement to append fields to the DB deltas table:\n')
  sql_stmt <- pool::sqlInterpolate(
      conn = conn_pool,
      sql  = glue::glue('
      INSERT INTO {db_tbl_name}
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
          to_deltas_tbl$WHEN_EDITED, db_tzone)
          )
      ))

  print(sql_stmt)

  pool::dbExecute(conn_pool, sql_stmt)


}



#
# datetime_type <- dplyr::case_when(
#   any(stringr::str_detect(cdr_which_db(conn_pool),"(?i)postgres")) ~ glue::glue("TIMESTAMP ?when AT TIME ZONE '{db_tzone}'"),
#   any(stringr::str_detect(cdr_which_db(conn_pool),"(?i)SQLite")) ~ "DATETIME(?when)",
#   any(stringr::str_detect(cdr_which_db(conn_pool),"(?i)Snowflake")) ~ "TO_TIMESTAMP(?when)",
#   TRUE ~ "TIMESTAMP ?when "
# )

