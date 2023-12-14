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
cdr_reconstruct_past_tbl <- function(as_of = lubridate::now() - lubridate::weeks(x = 4),
                                     conn_pool,
                                     db_tbl_name,
                                     chg_log_suffix = '_DELTAS',
                                     key_field = 'UID',
                                     ... ){

  return("Doesn't work yet.")

  CHG_FROM <- CHG_TO <- OBS_ID <- FIELD <- WHEN_EDITED <- ACTIVITY <-
    created <- row_num <- NULL
  checkmate::assert_true(lubridate::is.POSIXt(as_of))

  tbls <- cdr_pull_db_tbls(conn_pool, db_tbl_name, chg_log_suffix, key_field, ... )

  as_of_chg_log <- tbls[['chg_log_tbl']] |>
    dplyr::filter(WHEN_EDITED >= as_of) |>
    dplyr::arrange(dplyr::desc(WHEN_EDITED))


  # filter the chg-log table for first update after the as-of date, and
  # tag FALSE (for remove) any event that happened after an UID was created
  chgs_2_undo <- as_of_chg_log |>
    dplyr::mutate(
      created = dplyr::if_else(
        FIELD == key_field & # is key field
          (is.na(CHG_FROM) | nchar(CHG_FROM) == 0) & # from nothing
          (!is.na(CHG_TO)  | nchar(CHG_TO) > 0), # to something
        TRUE, NA)
    ) |>
    dplyr::group_by(OBS_ID) |>
    tidyr::fill(created, .direction = 'up') |>
    dplyr::ungroup()

  # remove user-created UIDs from primary table, and
  # add UIDs to primary table that used to exist (but were deleted)
  created_ids <- chgs_2_undo |> dplyr::filter(!is.na(created)) |>
    dplyr::pull(OBS_ID) |> unique()
  expel_locns <- tbls[['primary_tbl']][[key_field]] %in% created_ids
  interim_primary <- tbls[['primary_tbl']] |> dplyr::filter(!expel_locns)
  deleted_ids <- chgs_2_undo |> dplyr::filter(is.na(created)) |>
    dplyr::pull("OBS_ID") |>
    setdiff(interim_primary[[key_field]]) |>
    tibble::tibble() |>
    rlang::set_names(key_field)
  interim_primary <- dplyr::bind_rows(deleted_ids, interim_primary) |>
    dplyr::mutate(row_num = dplyr::row_number(),.before = 1)

  # # remove all post-creation ID events from the as-of chg-log, then
  # # remove any 'delete-UID' events from the chg_log table
  # # discard all but the earliest change for each UID and user-field combination
  row_locns <- interim_primary |> dplyr::select(row_num, key_field)
  col_locns <- interim_primary |> names() |> tibble::tibble() |>
    rlang::set_names('col_name') |>
    dplyr::mutate(col_num = dplyr::row_number())


  remaining_chgs <- chgs_2_undo |>
    dplyr::filter(is.na(created), FIELD != key_field) |>
    dplyr::group_by(OBS_ID,FIELD) |>
    dplyr::slice_min(order_by = WHEN_EDITED, by = ) |>
    dplyr::ungroup() |>
    dplyr::select(-created) |>
    # maybe unnecessary joins from here...
    dplyr::left_join(row_locns, by = c(OBS_ID = key_field)) |>
    dplyr::left_join(col_locns, by = c(FIELD = 'col_name'))

    chg_list <- purrr::transpose(remaining_chgs)
    as_of_primary <- interim_primary

    # need casting function

    for(chg in chg_list[2]){
      print(chg)
      # print(list(chg[['col_num']], chg[['row_num']]))
      # print(list(chg[['FIELD']], chg[['row_num']]))
      # print(list(chg[['FIELD']], chg[['row_num']]))
      # print(chg[['CHG_FROM']])
      # purrr::pluck(as_of_primary, chg[['col_num']], chg[['row_num']]) <- chg[['CHG_FROM']]
      # purrr::pluck(as_of_primary, chg[['FIELD']], chg[['OBS_ID']]) <- chg[['CHG_FROM']]
      # purrr::assign_in(as_of_primary, list(chg[['FIELD']], chg[['OBS_ID']]), chg[['CHG_FROM']] )
      # purrr::assign_in(as_of_primary, list(chg[['FIELD']], chg[['row_num']]), chg[['CHG_FROM']] )
      purrr::assign_in(as_of_primary, list(chg[['col_num']], chg[['row_num']]), chg[['CHG_FROM']] )
      print(as_of_primary[chg[['row_num']],])
    }

    out <- list(
      as_of_primary,
      as_of_chg_log,
      cdr_join_tbls(as_of_primary, as_of_chg_log, key_col = key_field)
    ) |>
      rlang::set_names(c('primary_tbl', 'chg_log_tbl', 'user_view_tbl'))

    return(out)

}

