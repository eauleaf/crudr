#' Attach unique IDs as the key column
#'
#' @param db_tbl a dataframe
#' @param key_field the name of the column with the Unique ID
#' @param use_guid use a 12 digit GUID instead of the default format
#'
#' @return a table with a column of Unique IDs
#' @export
#'
#' @examples
#' cdr_make_unique_ids(utils::head(iris,11))
#' cdr_make_unique_ids(utils::head(iris,11), key_field = 'GUID', use_guid = TRUE)
cdr_make_unique_ids <- function(db_tbl, key_field = 'UID', use_guid = FALSE){

  message('Auto generating Unique IDs for the table.')

  if(use_guid){
    uuids <- uuid::UUIDgenerate(n = nrow(db_tbl)) |> stringr::str_sub(-12)
  } else {
    uuids <- paste0('ID-',cdr_std_pad(nrow(db_tbl)))
  }


  db_tbl |>
    tibble::tibble() |>
    dplyr::mutate(
    .before = 1,
    {{key_field}} := uuids
  )

}


