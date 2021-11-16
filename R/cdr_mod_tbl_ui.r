#' UI module for a DT from the Server module
#'
#' User interface portion of the crudr module Presents an ajax datatable in a
#' shiny UI Used exclusively on 'mod_tbl_server' object
#'
#' @param id primary table name - namespace ID corresponding to the
#'   'primary_tbl_name' in the database
#' @param tbl_type one of 'db_tbl' or 'db_tbl_deltas' depending on which table
#'   from mod_tbl_server you want to present
#'
#' @return html (to be called in a shiny app only)
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(mod_tbl_ui('primary_table_name'))
#' }
cdr_mod_tbl_ui <- function(id, tbl_type = 'db_tbl'){
  shinycssloaders::withSpinner(
    DT::DTOutput(shiny::NS(id,tbl_type))
  )
}
