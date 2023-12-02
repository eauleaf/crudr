#' Convert a name or DBI::Id to SQL
#'
#' @param db_tbl_name db name as text or [cdr_id()] object
#' @param ... other DB params like `schema`
#'
#' @return SQL table object
#'
#' @examples
#' crudr:::cdr_id2sql('hello')
#' crudr:::cdr_id2sql(cdr_id(table = 'hello', schema = 'blue'))
#' crudr:::cdr_id(table = 'hello', schema = 'blue')
#' crudr:::cdr_id2sql(cdr_id(schema = 'blue', table = 'hello'))
#' crudr:::cdr_id(schema = 'blue', table = 'hello')
#'
cdr_id2sql <- function(db_tbl_name, ...){

  if(inherits(db_tbl_name, 'Id')){
    db_tbl_name
    DBI::dbQuoteIdentifier(DBI::ANSI(), x = db_tbl_name)
  } else {
    DBI::dbQuoteIdentifier(DBI::ANSI(), x = cdr_id(table = db_tbl_name, ...))
  }

}
