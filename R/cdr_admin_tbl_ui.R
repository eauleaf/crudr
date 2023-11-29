#' HTML to output a DT from the Server module
#'
#' A DT output for a namespace
#'
#' @param id primary table name - namespace ID that matches the
#'   table name of the primary table being administered in the database
#' @param tbl one of 'db_tbl' or 'chg_log_tbl' depending on which table
#'   from server module you want to present
#'
#' @return a DTOutput table with loading spinner
#'
#' @examples
#' ui_html <- shiny::fluidPage(crudr:::cdr_admin_tbl_ui('iris', 'db_tbl'))
#'
cdr_admin_tbl_ui <- function(id, tbl = 'db_tbl'){
  shinycssloaders::withSpinner(
    DT::DTOutput(shiny::NS(id,tbl))
  )
}
