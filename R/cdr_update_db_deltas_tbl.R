#' Populates deltas table when user updates primary table
#'
#' Connects to and posts who-what-when data to a change tracking table
#'  Function is called when a user updates the data in a primary table
#'
#' @param db_conn_pool pool connection object: the pool of connections established by the session
#' @param db_tbl_name string: name of the specific table the value to update is located in
#' @param to_deltas_tbl tibble of delta values to append
#' Note: to_deltas_tbl$WHEN_EDITED must be a string in timezone 'UTC'
#'
#' @return an in-memory copy of the data just appended to the tracking table
#' @export
#'

cdr_update_db_deltas_tbl <- function(db_conn_pool,
                                     db_tbl_name,
                                     to_deltas_tbl){

  cat('\n--Running: crudr::cdr_update_db_deltas_tbl()\n')

  cat('\nUsing this SQL statement to append fields to the DB deltas table:\n')
  sql_stmt <-
    pool::sqlInterpolate(
      conn = db_conn_pool,
      sql  = glue::glue('
      INSERT INTO "{db_tbl_name}"
      ("OBS_ID","FIELD","CHG_FROM","CHG_TO","WHO_EDITED","WHEN_EDITED" )
      VALUES (
        ?uid,
        ?field,
        ?from,
        ?to,
        ?who,
        {paste0(ifelse(db_conn_pool$objClass[[1]] == "SQLiteConnection", "DATETIME(?when)","TO_TIMESTAMP(?when)"))}
                        )'),
      .dots = list(
         uid   = to_deltas_tbl$OBS_ID
        ,field = to_deltas_tbl$FIELD
        ,from  = to_deltas_tbl$CHG_FROM
        ,to    = to_deltas_tbl$CHG_TO
        ,who   = to_deltas_tbl$WHO_EDITED
        ,when  = as.character(lubridate::with_tz(to_deltas_tbl$WHEN_EDITED, 'UTC'))
      ))

  print(sql_stmt)

  pool::dbExecute(db_conn_pool, sql_stmt)


}




