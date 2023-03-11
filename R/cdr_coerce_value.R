#' change cell value using DT::coerceValue
#'
#' @param input cell_edit value
#' @param old_mem_val type from database table
#'
#' @return input value coerced to the correct df_tbl value
#' @export
#'
#' @examples \dontrun{
#' input <- c('true','t','truthy','tri',1,0,'false','bla bla bla','a',' ','', 'na','none','no')
#' purrr::set_names(purrr::map(input, ~crudr::cdr_coerce_value(., TRUE)), input)
#' }

cdr_coerce_value <- function(input, old_mem_val){

  if(is.character(input)){ input <- stringr::str_trim(input) }

  if(!rlang::is_logical(old_mem_val)){

    out <- DT::coerceValue(input, old_mem_val)

  } else if (rlang::is_logical(old_mem_val)) {

    letter_vec <- stringr::str_split(stringr::str_to_upper(input), '|')[[1]]
    if(any(c('T', 1) %in% letter_vec)){ out <- TRUE
    } else if (any(c('F', 0) %in% letter_vec)){ out <- FALSE
    } else { out <- NA }

  }

  return(out)

}


