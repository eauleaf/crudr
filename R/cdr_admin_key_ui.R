#' UI module interface button for row additions and/or row deletions in server module table
#'
#' User interface portion of the server module UID input textbox and submit
#' button UI shows up for primary table if cell_edit_permission is set to T in server
#'
#'
#' @param id primary table name - namespace ID corresponding to the
#'   'primary_tbl_name' in the database
#'
#' @return html (to be called in a shiny app only)
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(cdr_admin_key_ui('primary_table_name'))
#' }
cdr_admin_key_ui <- function(id){

  ns <- shiny::NS(id)
  shinycssloaders::withSpinner(type = 7, size = 0.7, proxy.height = '80px',
    ui_element = shiny::uiOutput(ns('key_editor_ui'))
  )

}
