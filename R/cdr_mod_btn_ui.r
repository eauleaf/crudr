#' UI module interface for row additions in server module table
#'
#' User interface portion of the server module UID input textbox and submit
#' button UI shows up for primary table if open_sesame is set to T in server
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
#' ui <- fluidPage(mod_tbl_ui('primary_table_name'))
#' }
cdr_mod_btn_ui <- function(id){
  ns <- shiny::NS(id)
  shinycssloaders::withSpinner(
    shiny::uiOutput(ns('uid_btn'))
  )
}
