#' Build the admin table as it was at a point in the past
#'
#' @param as_of datetime object in the past, e.g. lubridate::now()-lubridate::weeks(4)
#' @param conn_pool pool or DBI connection
#' @param db_tbl_name primary table name
#' @param chg_log_suffix your table name extension specifying the corresponding
#'   change-log table, e.g. '_deltas'
#' @param key_field column in the primary table that acts as the unique row identifier
#' @param ... other connection parameters like `schema = public`
#'
#' @return list with primary and change-log tables reconstructed as they were at
#'   a time in the past
#' @export
#'
#' @examples \dontrun{
#' conn_pool <- pool::dbPool(
#'   DBI::dbConnect( drv = RPostgreSQL::PostgreSQL(),
#'                   dbname = "test",
#'                   host = "localhost",
#'                   port = "5432",
#'                   user = Sys.getenv('postgres_username'),
#'                   password = Sys.getenv('postgres_password'))
#' )
#' cdr_reconstruct_past_tbl(conn_pool = conn_pool, db_tbl_name = 'mtcars', schema = "test")
#' cdr_reconstruct_past_tbl(conn_pool = conn_pool, db_tbl_name = 'mtcars', schema = "test")
#' pool::poolClose(conn_pool)
#' }
#'
cdr_reconstruct_past_tbl <- function(as_of = lubridate::now() - lubridate::years(x = 1),
                                     conn_pool,
                                     db_tbl_name,
                                     chg_log_suffix = '_DELTAS',
                                     key_field = 'UID',
                                     ... ){

  return("Doesn't work yet.")

  CHG_FROM <- CHG_TO <- OBS_ID <- FIELD <- WHEN_EDITED <- ACTIVITY <- NULL
  checkmate::assert_true(lubridate::is.POSIXt(as_of))

  tbls <- cdr_pull_db_tbls(conn_pool, db_tbl_name, chg_log_suffix, key_field, ... )
  primary <- tbls[['primary_tbl']]
  chg_log <- tbls[['chg_log_tbl']] |> dplyr::filter(WHEN_EDITED >= as_of) |>
    cdr_label_chg_log_activity(key_field = key_field)


}

# DBI::dbListTables(con)
# DBI::dbListObjects(con)
# DBI::dbGetQuery(conn, "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA")

# filter chg_log_tbl for first update after the as-of date
# chg_log_tbl_trunc <- chg_log |> filter(WHEN_EDITED >= as_of) |>
#   arrange(desc(WHEN_EDITED)) |> group_by(OBS_ID, FIELD) |>
#   slice_tail() |> ungroup()




# monday ------------------------------------------------------------------

#
#   # reconstruct as of a point in the past -----------------------------------
#   as_of <- lubridate::now() - lubridate::years(x = 4)
#   checkmate::assert_true(lubridate::is.POSIXt(as_of))
#
#   # tbls <- cdr_pull_db_tbls(conn_pool, db_tbl_name, chg_log_suffix, key_field, ... )
#   primary <- tbls[['primary_tbl']]
#
#   # for the change-log:
#   #  - label the last activity for all observations
#   #  - delete all events after
#   # chg_log <- tbls[['chg_log']] |>
#   relevent_chg_log <- tbls[['chg_log']] |>
#     dplyr::filter(WHEN_EDITED >= as_of) |>
#     dplyr::group_by(OBS_ID, FIELD) |>
#     dplyr::filter(min(WHEN_EDITED) == WHEN_EDITED) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(
#       activity = dplyr::case_when(
#         FIELD == key_field & (!is.na(CHG_TO)  | nchar(CHG_TO) > 0)  ~ 'created',
#         FIELD == key_field & (is.na(CHG_FROM) | nchar(CHG_FROM))  ~ 'deleted'
#       )
#     ) |>
#     dplyr::group_by(WHEN_EDITED, OBS_ID) |>
#     dplyr::arrange(desc(WHEN_EDITED), activity) |>
#     tidyr::fill(activity, .direction = "down") |>
#     dplyr::ungroup() |>
#     dplyr::mutate(
#       activity = dplyr::if_else(is.na(activity), 'updated', activity)
#     )
#
#
#
#
# }
#
#
#
