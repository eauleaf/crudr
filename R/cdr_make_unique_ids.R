# Ensure unique IDs in the key column
#' Title
#'
#' @param db_tbl a dataframe
#' @param key_field the name of the column with the Unique ID
#'
#' @return a table with a column of Unique IDs
#' @export
#'
#' @examples cdr_make_unique_ids(utils::head(iris))
cdr_make_unique_ids <- function(db_tbl, key_field = 'UID'){

  message('Auto generating Unique IDs for the table.')

  db_tbl %>%
    tibble::tibble() %>%
    dplyr::mutate(
    .before = 1,
    {{key_field}} := paste0('ID-',stringr::str_pad(dplyr::row_number(),nchar(nrow(.)),pad=0))
  )

}


