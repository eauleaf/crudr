#' Create change types for the change-log table
#'
#' Attach a column of data to the change-log describing the
#' type of user change, i.e.
#'  - 'created' - user created a new ID
#'  - 'deleted' user deleted an ID and all associated row elements
#'  - 'updated' user made a change to a field of an existing ID
#'
#' @param chg_log a change-log tibble pulled from the DB
#' @param key_field the field in the primary table that contains the unique-ID
#'   for each observation
#'
#' @return the change-log tibble with activity type attached
#' @export
#'
#' @examples \dontrun{
#'   cdr_label_chg_log_activity(chg_log, 'UID')
#' }
#'
cdr_label_chg_log_activity <- function(chg_log, key_field = 'UID'){

  WHEN_EDITED <- NULL

  if('ACTIVITY' %in% names(chg_log)){
    split_clog <- chg_log |> split(f = is.na(chg_log$ACTIVITY))
    split_clog[['TRUE']] <- split_clog[['TRUE']] |> cdr_attach_activity(key_field = key_field)
    out <- dplyr::bind_rows(split_clog[['TRUE']], split_clog[['FALSE']]) |>
      dplyr::arrange(dplyr::desc(WHEN_EDITED))
  } else {
    out <- chg_log |> cdr_attach_activity(key_field = key_field)
  }

  return(out)

}


#' Determine the type of changes for the user change-log
#'
#' Attach a column of data to the change-log describing the
#' type of change the user made. See: [cdr_label_chg_log_activity()]
#' @inheritParams cdr_label_chg_log_activity
#'
#' @return the change-log tibble with activity type attached
#'
#' @examples \dontrun{ cdr_attach_activity(chg_log, 'UID') }
cdr_attach_activity <- function(chg_log, key_field = 'UID'){

  CHG_FROM <- CHG_TO <- OBS_ID <- FIELD <- WHEN_EDITED <- ACTIVITY <- NULL

  chg_log |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ACTIVITY = dplyr::case_when(
        FIELD == key_field & # is key field
          (is.na(CHG_FROM) | nchar(CHG_FROM) == 0) & # from nothing
          (!is.na(CHG_TO)  | nchar(CHG_TO) > 0) ~ 'created', # to something
        FIELD == key_field &  # is key field
          (!is.na(CHG_FROM) | nchar(CHG_FROM) > 0) & # from something
          ( is.na(CHG_TO)   | nchar(CHG_TO) == 0)  ~ 'deleted' # to nothing
      )
    ) |> dplyr::group_by(WHEN_EDITED, OBS_ID) |>
    tidyr::fill(ACTIVITY, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ACTIVITY = dplyr::if_else(is.na(ACTIVITY), 'updated', ACTIVITY)
    )

  }

