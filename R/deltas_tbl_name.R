#' creates the name of the change tracking table so it's set in one location
#' @param db_tbl_name char string: name of the primary table the deltas table will track
#'
#' @return char string: name of the table to track changes to the primary table
#' @export
#'
#' @examples
#' \dontrun{
#' deltas_tbl_name('some_table_name')
#' }

deltas_tbl_name <- function(db_tbl_name){
  glue::glue("{db_tbl_name}_deltas")
}
