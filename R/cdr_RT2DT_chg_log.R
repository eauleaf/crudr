#' Format the change log R table into a DT
#'
#' @param chg_log_tbl change log table imparted from the DB
#'
#' @return a DT object
#'
#' @examples \dontrun{
#' crudr:::cdr_RT2DT_chg_log(iris)
#' }
cdr_RT2DT_chg_log <- function(chg_log_tbl){

  cat('\n   --Running: cdr_RT2DT_chg_log()')

  chg_log_tbl |>
    DT::datatable(selection = 'none') |>
    DT::formatDate('WHEN_EDITED', method = 'toLocaleString') |>
    DT::renderDT()

}
