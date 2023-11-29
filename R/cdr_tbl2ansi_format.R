#' Prep dataframe columns to become SQL insert stmts
#'
#' @param db_tbl a dataframe
#' @param tzone the database default timezone
#'
#' @return a tibble
#'
#' @examples
#' example_df <- dplyr::mutate(iris, bool = Species == 'setosa', day = Sys.Date(), test = Sys.time())
#' adj_df <- crudr:::cdr_tbl2ansi_format(example_df)
#' adj_df <- crudr:::cdr_tbl2ansi_format(example_df, Sys.timezone())
#'
cdr_tbl2ansi_format <- function(db_tbl, tzone = "UTC"){

# format for ANSI() sql strings: bools & dates to char fields
tibble::tibble(db_tbl) |>
  dplyr::mutate(dplyr::across(tidyselect::where(rlang::is_logical), as.character)) |>
  dplyr::mutate(dplyr::across(tidyselect::where(lubridate::is.POSIXct),\(.) paste(lubridate::with_tz(time = ., tzone = tzone))))


}
