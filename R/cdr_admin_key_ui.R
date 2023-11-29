#' Interface buttons for row additions and/or row deletions in the admin table
#'
#' User interface portion of the server module Unique ID input textbox and button
#' button UI shows up for primary table if cell_edit_permission is set to T in server
#'
#'
#' @param id your table name - namespace ID corresponding to the
#'   'primary_tbl_name' in the database
#'
#' @return html (to be called in `cdr_deliver_admin_tbl()`)
#'
#' @examples \dontrun{
#' ui_html <- shiny::fluidPage(crudr:::cdr_admin_key_ui('primary_table_name'))
#' }
#'
cdr_admin_key_ui <- function(id){

  ns <- shiny::NS(id)
  shinycssloaders::withSpinner(type = 7, size = 0.7, proxy.height = '80px',
    ui_element = shiny::uiOutput(ns('key_editor_ui'))
  )

}
