#' gets the change history table for the Shiny UI
#'
#' @param db_tbl_name string: name of the primary database table being managed
#'
#' @return the rendered Datatable interface
#' @export
#'
#' @examples \dontrun{ crudr::cdr_deliver_chg_log_tbl('iris') }
cdr_deliver_chg_log_tbl <- function(db_tbl_name = ''){

  crudr::cdr_admin_tbl_ui(id = db_tbl_name, tbl = 'chg_log_tbl')

}
