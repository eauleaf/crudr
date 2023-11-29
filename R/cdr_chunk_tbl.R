#' break a dataframe into a list of dataframes
#'
#' The number of dataframes in the list is
#' based on the number of elements specified for each chunk.
#'
#' @param db_tbl a dataframe
#' @param chunk_size number of table elements to put into each chunk
#'
#' @return a list of dataframes
#'
#' @examples
#' chunked_df <- crudr:::cdr_chunk_tbl(iris, 20)
#' chunked_df <- crudr:::cdr_chunk_tbl(iris, 1)
#' chunked_df <- crudr:::cdr_chunk_tbl(iris, -1)
#' chunked_df <- crudr:::cdr_chunk_tbl(iris, 500)
#' chunked_df <- crudr:::cdr_chunk_tbl(iris, 1000)
#' chunked_df <- crudr:::cdr_chunk_tbl(as.matrix(iris), 0)
#' chunked_df <- crudr:::cdr_chunk_tbl(mtcars, 100)
#'
cdr_chunk_tbl <- function(db_tbl, chunk_size = 1000){

  checkmate::assert_int(chunk_size, na.ok = FALSE)
  db_tbl <- tibble::tibble(db_tbl)
  chunk_size <-  max(chunk_size, ncol(db_tbl))

  # break the dataframe into chunks of elements to execute so to not pass too
  # much text over a connection to a database
  split_count <- round(chunk_size/ncol(db_tbl))
  chopped_db_tbl <- split(
    x = db_tbl,
    f = rep(1:ceiling(nrow(db_tbl)/split_count), length.out = nrow(db_tbl), each = split_count)
  )

  chopped_db_tbl <- rlang::set_names(chopped_db_tbl, paste0('chunk_',cdr_std_pad(length(chopped_db_tbl))))

  return(chopped_db_tbl)

}

# for tests:
# nrow(iris) == sum(purrr::map_int(crudr:::cdr_chunk_tbl(db_tbl), nrow))
# nrow(iris) == sum(purrr::map_int(crudr:::cdr_chunk_tbl(db_tbl,5), nrow))
# nrow(iris) == sum(purrr::map_int(crudr:::cdr_chunk_tbl(db_tbl,-1), nrow))
