#' Join primary and deltas tables
#'
#' Join data from db_tbl and recent change values from chg_log tables.
#' Attaches most recent 'WHO_EDITED' and 'WHEN_EDITED' form deltas table to primary table.
#'
#' @param db_tbl primary table from DB
#' @param chg_log_tbl deltas table from DB
#' @param key_col the name of the unique ID column in the primary table
#'
#' @return tibble of the joined tables
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
#' chg_log_r_tbl <- structure(
#' list(
#'   OBS_ID = c( "ID-001", "ID-002"),
#'   FIELD = c("Species", "Sepal.Width"),
#'   CHG_FROM = c("setosa", "3.5"),
#'   CHG_TO = c("set", "5"),
#'   WHO_EDITED = c("joe", "fred"),
#'   WHEN_EDITED = c("11/29/2023, 1:36:46 AM", "11/28/2023, 5:39:21 PM")
#' ),
#' class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -2L)
#' )
#' primary_r_tbl <- cdr_make_unique_ids(iris)
#' cdr_join_tbls(primary_r_tbl, chg_log_r_tbl, key_col = 'UID')
#'
cdr_join_tbls <- function(db_tbl, chg_log_tbl, key_col){
  cat('\n--Running: cdr_join_tbls()\n')
  cat('\n Combine or Recombine primary and delta tables for interface \n\n')

  OBS_ID <- WHO_EDITED <- WHEN_EDITED <- NULL

  out <- chg_log_tbl |>
    dplyr::group_by(OBS_ID) |>
    dplyr::filter(WHEN_EDITED == max(WHEN_EDITED, na.rm = T)) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::select(OBS_ID, WHO_EDITED, WHEN_EDITED) |>
    dplyr::left_join(
      x = dplyr::mutate(db_tbl, "{key_col}" := as.character(!!dplyr::sym(key_col))),
      by = stats::setNames('OBS_ID', key_col),
      suffix = c('','TBL2')) |>
    dplyr::rename(WHO_EDITED_LAST = WHO_EDITED, WHEN_EDITED_LAST = WHEN_EDITED) |>
    suppressWarnings()

  return(out)

}


