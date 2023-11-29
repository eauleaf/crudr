#' Provides an equivalent R timezone for the DB timezone
#'
#' Translates between Postgres, Sqlite, Snowflake & R  timezones to adjust for timezone treatment
#'  between different databases
#'
#' Postgres timezone ref:
#' 'https://www.postgresql.org/docs/8.1/datetime-keywords.html'
#'
#' Snowflake is always: 'UTC'
#'
#' Sqlite is always an empty string: ""
#' (not 'UTC')
#'
#' @param conn_pool database connection of type pool
#'
#' @return an OlsonNames() timezone for R and lubridate
#'
#' @examples \dontrun{
#' conn_pool <- pool::dbPool(
#'   DBI::dbConnect( RPostgreSQL::PostgreSQL(),
#'                   dbname = "test",
#'                   host = "localhost",
#'                   port = "5432",
#'                   user = Sys.getenv('postgres_username'),
#'                   password = Sys.getenv('postgres_password'))
#' )
#' cdr_adj_timezone(conn_pool)
#' pool::poolClose(conn_pool)
#' }
#'
cdr_adj_timezone <- function(conn_pool){

  db_type <- cdr_which_db(conn_pool)

  if( any(stringr::str_detect(db_type,"(?i)postgres")) ) {

    db_tz <- paste(pool::dbGetQuery(conn_pool, "SELECT current_setting('TIMEZONE')"))

    if( !db_tz %in% OlsonNames() ){

      hr_offset <- as.numeric(pool::dbGetQuery(conn_pool, "SELECT EXTRACT(TIMEZONE FROM now())/3600.0;"))

      rtz <- structure(
        list(
          utc_offset_hr = c(
            -1, -2, -3, -3.5, -4, -5, -6, -7, -8, -9, -9.5, -10, -11, -12, 0, 1, 2, 3,
            3.5, 4, 4.5, 5, 5.5, 5.75, 6, 6.5, 7, 8, 8.75, 9, 9.5, 10, 10.5, 11, 12, 13,
            13.75, 14
          ),
          utc_offset = c(
            "-01:00:00", "-02:00:00", "-03:00:00", "-03:30:00", "-04:00:00", "-05:00:00",
            "-06:00:00", "-07:00:00", "-08:00:00", "-09:00:00", "-09:30:00", "-10:00:00",
            "-11:00:00", "-12:00:00", "00:00:00", "01:00:00", "02:00:00", "03:00:00",
            "03:30:00", "04:00:00", "04:30:00", "05:00:00", "05:30:00", "05:45:00",
            "06:00:00", "06:30:00", "07:00:00", "08:00:00", "08:45:00", "09:00:00",
            "09:30:00", "10:00:00", "10:30:00", "11:00:00", "12:00:00", "13:00:00",
            "13:45:00", "14:00:00"
          ),
          name = c(
            "Etc/GMT+1", "Etc/GMT+2", "Etc/GMT+3", "America/St_Johns", "Etc/GMT+4",
            "Etc/GMT+5", "Etc/GMT+6", "Etc/GMT+7", "Etc/GMT+8", "Etc/GMT+9",
            "Pacific/Marquesas", "Etc/GMT+10", "Etc/GMT+11", "Etc/GMT+12", "Etc/GMT",
            "Etc/GMT-1", "Etc/GMT-2", "Etc/GMT-3", "Asia/Tehran", "Etc/GMT-4", "Asia/Kabul",
            "Etc/GMT-5", "Asia/Colombo", "Asia/Kathmandu", "Etc/GMT-6", "Asia/Yangon",
            "Etc/GMT-7", "Etc/GMT-8", "Australia/Eucla", "Etc/GMT-9", "Australia/Darwin",
            "Etc/GMT-10", "Australia/Adelaide", "Etc/GMT-11", "Etc/GMT-12", "Etc/GMT-13",
            "Pacific/Chatham", "Etc/GMT-14"
          ),
          abbrev = c(
            "-01", "-02", "-03", "NST", "-04", "-05", "-06", "-07", "-08", "-09", "-0930",
            "-10", "-11", "-12", "GMT", "+01", "+02", "+03", "+0330", "+04", "+0430", "+05",
            "+0530", "+0545", "+06", "+0630", "+07", "+08", "+0845", "+09", "ACST", "+10",
            "ACDT", "+11", "+12", "+13", "+1345", "+14"
          ),
          name_alt = c(
            "Atlantic/Azores", "Atlantic/South_Georgia", "America/Sao_Paulo",
            "America/St_Johns", "America/Puerto_Rico", "EST5EDT", "CST6CDT", "MST7MDT",
            "PST8PDT", "America/Anchorage", "Pacific/Marquesas", "HST", "Pacific/Samoa",
            "Etc/GMT+12", "UTC", "CET", "EET", "Europe/Istanbul", "Asia/Tehran",
            "Asia/Dubai", "Asia/Kabul", "Asia/Karachi", "Asia/Kolkata", "Asia/Kathmandu",
            "Asia/Dhaka", "Asia/Yangon", "Asia/Jakarta", "Asia/Taipei", "Australia/Eucla",
            "Asia/Tokyo", "Australia/Darwin", "Australia/Brisbane", "Australia/Adelaide",
            "Australia/Sydney", "Pacific/Fiji", "Pacific/Auckland", "Pacific/Chatham",
            "Pacific/Kiritimati"
          ),
          abbrev_alt = c(
            "-01", "-02", "-03", "NST", "AST", "EST", "CST", "MST", "PST", "AKST", "-0930",
            "HST", "SST", "-12", "UTC", "CET", "EET", "+03", "+0330", "+04", "+0430", "PKT",
            "IST", "+0545", "+06", "+0630", "WIB", "CST", "+0845", "JST", "ACST", "AEST",
            "ACDT", "AEDT", "+12", "NZDT", "+1345", "+14"
          ),
          is_dst_alt = c(
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
            FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
            TRUE, FALSE, TRUE, TRUE, FALSE
          )
        ),
        class = c(
          "tbl_df", "tbl", "data.frame"
        ),
        row.names = c(
          NA, -38L
        )
      )

    }


  } else if( any(stringr::str_detect(db_type,"(?i)sqlite")) ){

    db_tz <- ""

  } else {

    db_tz <- "UTC"

  }


  return(db_tz)

}



