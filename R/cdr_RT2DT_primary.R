#' Format the primary R table into a DT
#'
#' @param db_tbl change log table imparted from the DB
#' @param cell_edit_permission bool: do you want to allow the admin table cells to be edited
#' @param lock_fields strings: column names you want to lock up e.g. c('Species','Petal.Width')
#'
#' @return a DT object to present the primary table
#'
#' @examples \dontrun{
#' crudr:::cdr_RT2DT_primary(iris, cell_edit_permission = T)
#' }
#'
cdr_RT2DT_primary <- function(db_tbl, cell_edit_permission, lock_fields = c() ){

  cat('\n   --Running: cdr_RT2DT_primary()')

  out <- db_tbl |>
    DT::datatable(
      options = list(scrollX = TRUE,  keys = TRUE),
      callback = cdr_js_edit_ctrl(),
      extensions = "KeyTable",
      selection = 'none',
      editable = if (cell_edit_permission) {
        list(target = "cell",
             disable = list(columns = c(0, 1, which(
               names(db_tbl) %in% lock_fields
             ))))
      }
    )

  # determine & format posix fields
  posix_fields <- db_tbl |>
    purrr::map_lgl(lubridate::is.POSIXct) |>
    purrr::keep(\(.) .) |> names()
  if( length(posix_fields) > 0 ) {
    out <- DT::formatDate(out, columns = posix_fields, method  = 'toLocaleString')
  }


  return(DT::renderDT( out ))

}
