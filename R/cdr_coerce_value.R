#' Change cell value using [DT::coerceValue()]
#'
#' @param input_val cell_edit value
#' @param old_mem_val type from database table
#'
#' @return input_val value coerced to the correct `df_tbl` value
#'
#' @examples \dontrun{
#' cdr_coerce_value(c('2023-03-17', '2023-03-17T14:28:24Z'), lubridate::now())
#' input_val <- c('true','t','truthy','tri',1,0,5,-5,'false','bla bla bla','a',' ','',
#' 'na','none','no', 'yes', 'off', 'on', 'no', 'f','F','T', 'NO')
#' purrr::set_names(purrr::map(input_val, ~cdr_coerce_value(., TRUE)), input_val)
#' }

cdr_coerce_value <- function(input_val, old_mem_val){
  cat('\n--Running: cdr_coerce_value()\n')

  safe_coersion <- purrr::possibly(.f = DT::coerceValue, otherwise = NA)


  if(is.character(input_val)){ input_val <- stringr::str_trim(input_val) }


  if(!rlang::is_logical(old_mem_val)){

    out <- safe_coersion(input_val, old_mem_val)

  } else if (rlang::is_logical(old_mem_val)) {

    if( stringr::str_detect(input_val, '(?i)(true)|(^t$)|1|(yes)|(^y$)|(on)') ){
      out <- TRUE
    } else if ( stringr::str_detect(input_val, '(?i)(false)|(^f$)|0|(^n$)|(no)') ){
      out <- FALSE
    } else {
      out <- NA
      }

  }

  cat('\t Value coerced.\n')
  return(out)

}








