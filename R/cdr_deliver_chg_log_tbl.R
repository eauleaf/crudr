#' Gets the the change history table in the Shiny UI
#'
#' @param db_tbl_name string: name of the primary database table being managed
#'
#' @return the rendered Datatable interface
#' @export
#'
#' @examples cdr_deliver_chg_log_tbl('iris')
#'
cdr_deliver_chg_log_tbl <- function(db_tbl_name = ''){

  cdr_admin_tbl_ui(id = db_tbl_name, tbl = 'chg_log_tbl')

}
