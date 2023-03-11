#' UI module to output a DT from the Server module
#'
#' User interface portion of the crudr module. Presents an ajax datatable in a
#' shiny UI. Just a DT output wrapped in a namespace
#'
#' @param id primary table name - namespace ID that matches the
#'   table name of the primary table being administered in the database
#' @param tbl one of 'db_tbl' or 'chg_log_tbl' depending on which table
#'   from server module you want to present
#'
#' @return a DTOutput table with loading spinner
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(mod_tbl_ui('primary_table_name'))
#' }
cdr_admin_tbl_ui <- function(id, tbl = 'db_tbl'){
  shinycssloaders::withSpinner(
    DT::DTOutput(shiny::NS(id,tbl))
  )
}
