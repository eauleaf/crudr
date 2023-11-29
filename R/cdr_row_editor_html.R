#' The html for the UI to add and delete new rows
#'
#' Responsive to given permissions
#'
#' @param notes_txt message to send to the UI edit page
#' @param id namespace id - corresponds to table name
#' @param add_row_permission T or F: whether user has permission to add a new observation, i.e. the Unique ID for a row
#' @param del_row_permission T or F: whether user has permission to delete a new observation, i.e. the Unique ID for a row
#'
#' @return ui output component
#'
#' @examples crudr:::cdr_row_editor_html('hello', 'iris', TRUE, TRUE)
cdr_row_editor_html <- function(notes_txt = '', id, add_row_permission = F, del_row_permission = F){

  cat('\n--Running: cdr_row_editor_html()\n')

  ns <- shiny::NS(id)
  html_style <- "display: inline-flex; align-items: center; font-size: 10px;"
  html_for_uid_box <- shiny::textInput(ns('uid'), '', placeholder = 'Enter Unique ID', width = '180px')
  html_btn_to_create_row <- shiny::actionButton(ns('create_row_btn'), label = "Create Row", text = 'Create Row', style = 'margin-left: 15px;' )
  html_btn_to_delete_row <- shiny::actionButton(ns('delete_row_btn'), label = "Delete Row", text = 'Delete Row', style = 'margin-left: 15px;' )
  html_red_text_output <- shiny::span(notes_txt, style = "color:red; font-size: 130%; margin-left: 15px;")


  if (add_row_permission & del_row_permission) {
      cat('\n--Granting permissions: add and delete rows \n')
      shiny::tags$span(style=html_style,
                       html_for_uid_box,
                       html_btn_to_create_row,
                       html_btn_to_delete_row,
                       html_red_text_output
                       )

  } else if (add_row_permission) {
      cat('\n--Granting permissions: add rows only \n')
      shiny::tags$span(style=html_style,
                       html_for_uid_box,
                       html_btn_to_create_row,
                       html_red_text_output
      )

  } else if (del_row_permission){
      cat('\n--Granting permissions: delete rows only \n')
    shiny::tags$span(style=html_style,
                     html_for_uid_box,
                     html_btn_to_delete_row,
                     html_red_text_output
    )

  } else {
      cat('\n--Granting permissions: no add/delete rows \n')
    ' '

  }

}


