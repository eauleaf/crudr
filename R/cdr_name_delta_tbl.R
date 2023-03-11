#' creates the name of the change tracking table so it's set in one location
#' @param db_tbl_name char string: name of the primary table the deltas table will track
#'
#' @return char string: name of the table to track changes to the primary table
#' @export
#'
#' @examples
#' \dontrun{
#' crudr::cdr_name_delta_tbl('some_table_name')
#' }

cdr_name_delta_tbl <- function(db_tbl_name){
  glue::glue("{db_tbl_name}_DELTAS")
}
