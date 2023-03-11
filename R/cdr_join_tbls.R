#' Join primary and deltas tables
#'
#' join data from db_tbl and recent change values from chg_log_tbl tables
#' (attaches most recent 'who' and 'when' form deltas table to primary table)
#' function used in 'mod_tbl_server' eventReactive
#'
#' @param db_tbl imported primary table
#' @param chg_log_tbl imported deltas table
#' @param key_col the name of the unique ID column in the primary table
#'
#' @return the joined tables; all of db_tbl, and who-when information from the
#'   deltas table (function also places the individual tables into output$ for
#'   ui presentation)
#'
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' combine_prim_delt_tbls(db_tbl, chg_log_tbl, key_col)
#' }
cdr_join_tbls <- function(db_tbl, chg_log_tbl, key_col){
  cat('\nRunning function: crudr::cdr_join_tbls()')
  cat('\n Combine or Recombine primary and delta tables for interface \n\n')

  chg_log_tbl %>%
    dplyr::group_by(OBS_ID) %>%
    dplyr::filter(WHEN_EDITED == max(WHEN_EDITED, na.rm = T)) %>%
    dplyr::select(OBS_ID, WHO_EDITED, WHEN_EDITED) %>%
    dplyr::left_join(x = dplyr::mutate(db_tbl, {{key_col}} := as.character(!!dplyr::sym(key_col))),
              y = .,
              by = stats::setNames('OBS_ID', key_col),
              suffix = c('','TBL2')) %>%
    dplyr::rename(WHO_EDITED_LAST = WHO_EDITED, WHEN_EDITED_LAST = WHEN_EDITED)

}


