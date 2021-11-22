#' new observation row addition UI
#'
#' @param notes_txt message to send to the UI edit page
#' @param id namespace id (corresponds to DB name)
#' @param open_sesame whether user has permission to edit page
#'
#' @return ui output component
#' @export
#'
#' @examples cdr_render_new_row_ui('hello', 'iris', TRUE)
cdr_render_new_row_ui <- function(notes_txt = '', id, open_sesame){

  print('cdr_render_new_row_ui')

  if(open_sesame){
    shiny::renderUI({
      ns <- shiny::NS(id)
      shiny::tags$span(style="display: inline-flex; align-items: center; font-size: 10px;",
                       shiny::textInput(ns('uid'), '', placeholder = 'Enter unique ID', width = '180px'),
                       shiny::actionButton(ns('load_uid'), label = "Create Row", text = 'Create Row', style = 'margin-left: 15px;' ),
                       shiny::span(notes_txt, style = "color:red; font-size: 130%; margin-left: 15px;"))})
  } else {
    output$uid_btn <- shiny::renderUI(' ')
  }
}
