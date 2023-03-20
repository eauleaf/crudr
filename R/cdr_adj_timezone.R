#' adjust for timezone treatment between databases
#'
#' @param conn_pool database connection of type pool
#'
#' @return 'UTC' or empty string
#' @export
#'
#' @examples \dontrun{ cdr_adj_timezone(conn_pool) }
#'
cdr_adj_timezone <- function(conn_pool){

  if(stringr::str_detect(conn_pool$objClass[[1]],"(?i)postgres")){
    pg_tz <- paste(pool::dbGetQuery(conn_pool, "SELECT current_setting('TIMEZONE')"))
  } else {
    pg_tz <- "UTC"
    }


  dplyr::case_when(
    stringr::str_detect(conn_pool$objClass[[1]],"(?i)Snowflake") ~ 'UTC',
    stringr::str_detect(conn_pool$objClass[[1]],"(?i)sqlite") ~ '',
    stringr::str_detect(conn_pool$objClass[[1]],"(?i)postgres") ~ pg_tz,
    T ~ Sys.timezone()
    )

}
