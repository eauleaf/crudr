#' Download database table and sync it to user interface
#'
#' @param db_conn_pool pool connection object from package 'pool'
#' @param db_tbl_name string: name of the database table you're managing
#' @param key_col string: the field that contains your unique ID per row
#' @param cell_edit_permission bool: do you want to allow the admin table cells to be edited
#' @param lock_fields strings: column names you want to lock up e.g. c('Species','Petal.Width')
#'
#' @return a DT object to present the primary table
#' @export
#'
#' @examples \dontrun{cdr_impart_primary_tbl(con, 'IRIS', 'UID', TRUE, 'Species')}
cdr_impart_primary_tbl <- function(
    db_conn_pool,
    db_tbl_name,
    key_col,
    cell_edit_permission,
    lock_fields = c()
) {
  cat('\n--Running: crudr::cdr_impart_primary_tbl()\n')

  cat('\n Downloading Primary table from the database and presenting it in the UI.\n')
  db_tbl <<- dplyr::tbl(db_conn_pool,db_tbl_name) %>% dplyr::collect() %>% dplyr::relocate(dplyr::all_of(key_col))
  proxy_db_tbl <<- DT::dataTableProxy('db_tbl')

  DT::renderDT(
    DT::datatable(
      db_tbl,
      options = list(scrollX = TRUE,  keys = TRUE),
      callback = crudr::cdr_js_edit_ctrl(),
      extensions = "KeyTable",
      selection = 'none',
      editable = if (cell_edit_permission) {
        list(target = "cell",
             disable = list(columns = c(0, 1, which(
               names(db_tbl) %in% lock_fields
             ))))
      }
    )
  )

}

