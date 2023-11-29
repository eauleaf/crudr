#' Shiny UI HTML for the administrator table and
#'
#' @param db_tbl_name string: name of the primary database table being managed (for namespace id)
#'
#' @return  the rendered Datatable interface
#' @export
#'
#' @examples
#' ui_html <- cdr_deliver_admin_tbl('iris')
#'
cdr_deliver_admin_tbl <- function(db_tbl_name = ''){
  htmltools::div(
    cdr_admin_key_ui(db_tbl_name),
    cdr_admin_tbl_ui(id = db_tbl_name, tbl = 'db_tbl')
    )
}


