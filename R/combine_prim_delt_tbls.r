#' join data from db_tbl and recent change values from db_tbl_deltas tables
#'  function used in 'mod_tbl_server' eventReactive
#'
#' @param db_tbl imported primary table
#' @param db_tbl_deltas imported deltas table
#' @param key_col the name of the unique ID column in the primary table
#'
#' @return the joined tables; all of db_tbl, and who-when information from the deltas table (function also places the individual tables into output$ for ui presentation)
#' @export
#'
#' @examples
#' \dontrun{
#' combine_prim_delt_tbls(db_tbl, db_tbl_deltas, key_col)
#' }
combine_prim_delt_tbls <- function(db_tbl, db_tbl_deltas, key_col){

  db_tbl_deltas %>%
    dplyr::group_by(uid) %>%
    dplyr::filter(when == max(when, na.rm = T)) %>%
    dplyr::select(uid, who, when) %>%
    dplyr::left_join(x = dplyr::mutate(db_tbl, {{key_col}} := as.character(!!sym(key_col))),
              y = .,
              by = setNames('uid', key_col),
              suffix = c('','_chg'))
}


