#' creates the name of the change tracking table so it's set in one location
#'
#' @param db_tbl_name char string: name of the primary table the deltas table will track
#' @param chg_log_suffix if specified, appends your string to your chg_log DB table name
#'   rather than "_DELTAS"
#'
#' @return char string: name of the table to track changes to the primary table
#'
#' @examples
#'   crudr:::cdr_name_delta_tbl('some_table_name')
#'   crudr:::cdr_name_delta_tbl('some_table_name', chg_log_suffix = '_deltas')
#'   crudr:::cdr_name_delta_tbl('some_table_name', chg_log_suffix = '_chglog')
#'
cdr_name_delta_tbl <- function(db_tbl_name, chg_log_suffix = '_DELTAS'){

  glue::glue("{db_tbl_name}{chg_log_suffix}")

}
