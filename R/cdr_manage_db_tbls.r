#' Server module to present and control a DB table
#'
#' This function is a server module that gets database tables from a DB,
#' presents that table in shiny server, and syncs user changes between the UI
#' server-memory R tables, and the back-end DB.
#'
#' @param db_tbl_name a string specifying the table name for the the primary
#'   database table you are managing, e.g. 'iris_tbl'
#' @param key_col name of the unique ID column in the db table (table must have
#'   a unique ID column with unique IDs)
#' @param conn_pool db connection from package 'pool' or 'DBI'
#' @param session current shiny session
#' @param cell_edit_permission T or F: to make editable the primary table from the module
#'    (cell_edit_permission = T means the user can change the data)
#'    (cell_edit_permission = F means the user can only see the data)
#' @param add_row_permission T or F: allows user to add a row to the primary table of the module
#' @param del_row_permission T or F: allows user to delete a row on the primary table of the module
#' @param lock_fields strings: a vector of field names from the database table to lock from user editing
#' @param multiuser_update_wait numeric: minimum time in seconds between checking for
#'   and incorporating potential data changes made by other users
#' @param chg_log_suffix optional string to identify a suffix other than '_DELTAS'
#'   for your database change-log table
#' @param ... additional parameters to specify the primary table location in
#' the database if needed, e.g. schema = 'my_schema', catalog = 'some_catalog'
#'
#'
#' @return returns a DT to the shiny ui environment, and as side-effect returns
#'   a `chg_log_tbl` and an editable `db_tbl` specified by namespace
#' @export
#'
#' @examples \dontrun{
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' cdr_make_db_tbls(con, iris)
#' server <- function(input, output, session){
#'              r_tbl <- cdr_manage_db_tbls('iris', 'UID', con, session)
#'              output$iris <- DT::renderDT(r_tbl()) }
#' ui <- shiny::fluidPage(DT::DTOutput('iris'))
#' shiny::shinyApp(ui,server)
#'}

cdr_manage_db_tbls <- function(db_tbl_name,
                               key_col, conn_pool, session,
                               add_row_permission   = FALSE,
                               del_row_permission   = FALSE,
                               cell_edit_permission = FALSE,
                               lock_fields = c(),
                               multiuser_update_wait = 0,
                               chg_log_suffix = '_DELTAS',
                               ...
){

  shiny::moduleServer(db_tbl_name, module = function(input, output, session){



    # SECTION 1: INITIALIZE TABLES ---------------------------------------------------
    cat('\n\n# SECTION 1: INITIALIZE & SETUP -----------------------------------------')

    cat('\n  Sec1 - Initializing Server-module variables\n')

    db_tz <- cdr_adj_timezone(conn_pool)
    WHEN_EDITED <- NULL
    table_edited <- F
    last_multiuser_timechk <- Sys.time()
    user <- ifelse(is.null(session$user), Sys.info()[['user']], session$user)

    primary_table_id <- cdr_id(table = db_tbl_name, ...)
    deltas_tbl_name <- cdr_name_delta_tbl(db_tbl_name, chg_log_suffix = chg_log_suffix)
    deltas_table_id <- cdr_id(table = deltas_tbl_name, ...)


    cat('\n  Sec1 - Creating the UI above the primary table based on add-row and delete-row permissions\n')
    output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(
      '', db_tbl_name, add_row_permission, del_row_permission)
    })

    cat('\n  Sec1 - Getting and Posting the Primary and Change log tables\n')
    cat('\n  Sec1 - Primary DB table: collect, sync, and present')
    db_tbl <- cdr_DB2RT_primary(conn_pool,db_tbl_name = primary_table_id, key_col = key_col)
    output$db_tbl <- cdr_RT2DT_primary(db_tbl,cell_edit_permission,lock_fields)
    proxy_db_tbl <- DT::dataTableProxy('db_tbl')

    cat('\n  Sec1 - Deltas DB table: collect, sync, and present')
    chg_log_tbl <- cdr_DB2RT_chg_log(conn_pool, chg_log_tbl_name = deltas_table_id)
    output$chg_log_tbl <- cdr_RT2DT_chg_log(chg_log_tbl)
    proxy_chg_log_tbl <- DT::dataTableProxy('chg_log_tbl')

    cat('\n\n# SECTION 1: DONE INITIALIZING ------------------------------------------\n\n')


    # SECTION 2: EDIT CELL ---------------------------------------------------------------
    shiny::observeEvent(input$db_tbl_cell_edit, {
      cat('\n\n# SECTION 2: EDIT CELL ----------------------------------------------------')
      cat('\n  Sec2 - Updates Interface, DB, and R tables when user enters new data into a cell in the primary table\n')
      table_edited <<- T

      if(nrow(input$db_tbl_cell_edit)==0) {
        cat('\n  Sec2 - WHOA! Slow down there Tiger. Computers gotta think too. \n\n')
        table_edited <<- F
        return()
      }


      cat("\n  Sec2 - Collecting the information that the User just typed into the UI\n")
      to_update <- input$db_tbl_cell_edit
      old_mem_val <- db_tbl[ to_update[['row']], to_update[['col']] ][[1]]
      update_value <- cdr_coerce_value(to_update[['value']], old_mem_val)
      value_colname <- names(db_tbl)[to_update[['col']]]
      value_rowuid <- db_tbl[ to_update[['row']], key_col ][[1]] # Unique key Id


      cat("\n  Sec2 - If the User put in nothing, then skip out of the cell update.\n")
      if(((is.na(old_mem_val) | is.null(old_mem_val) | identical(old_mem_val, '')) &
          (is.na(update_value) | is.null(update_value) | identical(update_value, ''))) |
         identical(old_mem_val, update_value)){
        cat(glue::glue(.trim = F, "  Sec2 - The old value is '{old_mem_val}'",
                       "and the new value is '{update_value}'. Not updating the DB.\n"))

        cat("  Sec2 - Given no user edit, sync just the R primary table tibble and UI proxy table with the UI interface table\n")
        db_tbl[to_update[['row']], to_update[['col']]] <- update_value
        db_tbl <<- db_tbl
        DT::replaceData(proxy_db_tbl, db_tbl)
        table_edited <<- F
        return()
      } else {cat("   - Not skipping... user entered info to process. \n")}


      cat("\n  Sec2 - Update the R primary table and UI proxy table\n")
      db_tbl[to_update[['row']], to_update[['col']]] <- update_value

      output$key_editor_ui <- shiny::renderUI({
        cdr_row_editor_html('... Updating Database ...',db_tbl_name,
                                   add_row_permission, del_row_permission)
      })

      cat('\n  Sec2 - Creating tibble to append changes to Delta table\n')
      cat('\n value_rowuid:',value_rowuid,'\n')
      cat('\n value_colname:',value_colname,'\n')
      cat('\n old_mem_val:',old_mem_val,'\n')
      cat('\n update_value:',update_value,'\n')
      cat('\n timestamp:',lubridate::now(tzone = db_tz),'\n')

      to_deltas_tbl <- tibble::tibble(
        OBS_ID = value_rowuid,
        FIELD = value_colname,
        CHG_FROM = as.character(old_mem_val),
        CHG_TO = as.character(update_value),
        WHO_EDITED = user,
        WHEN_EDITED = lubridate::now(tzone = db_tz)
      )

      cat('\n  Sec2 - Update deltas table in R parent env\n')
      chg_log_tbl <- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) |>
        dplyr::arrange(dplyr::desc(WHEN_EDITED))


      cat('\n  Sec2 - Updating primary table in Database\n')
      cdr_update_db_primary_tbl(
        conn_pool     = conn_pool,
        db_tbl_name   = primary_table_id,
        update_value  = update_value,
        value_colname = value_colname, # specific column name to insert value
        value_rowuid  = value_rowuid, # specific row UID to insert value
        key_column    = key_col
      )

      cat('\n  Sec2 - Updating delta table in Database\n')
      cdr_update_db_deltas_tbl(
        conn_pool     = conn_pool,
        db_tbl_name   = deltas_table_id,
        to_deltas_tbl = to_deltas_tbl,
        db_tzone      = db_tz
      )

      cat('\n  Sec2 - Update deltas proxy table for UI\n')
      db_tbl <<- db_tbl
      chg_log_tbl <<- chg_log_tbl
      DT::replaceData(proxy_db_tbl, db_tbl)
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html('',db_tbl_name, add_row_permission, del_row_permission) })


    })



    # SECTION 3: NEW ROW ------------------------------------------------------
    shiny::observeEvent(input$create_row_btn, {
      cat('\n\n# SECTION 3: NEW ROW ------------------------------------------------------')
      cat('\n  Sec3 - Update Interface, DB, and R tables when user clicks button input$create_row_btn\n')
      table_edited <<- T


      input_uid <- stringr::str_trim(input$uid)


      cat('\n  Sec3 - Check unique ID for uniqueness on "create".\n')
      out_text <- cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'create')


      if(is.null(out_text)){

        output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html('... Updating Database ...',db_tbl_name, add_row_permission, del_row_permission) })

        db_tbl <- dplyr::bind_rows(tibble::tibble("{key_col}" := input_uid), db_tbl)

        cat('\n  Sec3 - Run SQL to make new row in Primary table.\n')
        cdr_create_row_in_db(
          conn_pool   = conn_pool,
          db_tbl_name = primary_table_id,
          key_col     = key_col,
          input_uid   = input_uid
        )


        cat('\n  Sec3 - Create append deltas tibble.\n')
        to_deltas_tbl <- tibble::tibble(
          OBS_ID = input_uid,
          FIELD = key_col,
          # CHG_FROM = "",
          CHG_FROM = NA_character_,
          CHG_TO = input_uid,
          WHO_EDITED = user,
          WHEN_EDITED = lubridate::now(tzone = db_tz)
        )

        cat('\n  Sec3 - Update parent env and proxy Deltas tables.\n')
        chg_log_tbl <- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) |> dplyr::arrange(dplyr::desc(WHEN_EDITED))

        cat('\n  Sec3 - Update database Deltas table.\n')
        cdr_update_db_deltas_tbl(
          conn_pool = conn_pool,
          db_tbl_name = deltas_table_id,
          to_deltas_tbl = to_deltas_tbl
        )

      }

      db_tbl <<- db_tbl
      chg_log_tbl <<- chg_log_tbl
      DT::replaceData(proxy_db_tbl, db_tbl)
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })


    })



    # SECTION 4: DELETE ROW REQUEST ---------------------------------------------------
    shiny::observeEvent(input$delete_row_btn, {
      cat('\n\n# SECTION 4: DELETE ROW REQUEST ---------------------------------------------------\n')
      cat('\n  Sec4 - Throws a delete popup to determine if the user is serious\n')


      input_uid <- stringr::str_trim(input$uid)

      cat('\n  Sec4 - Check unique ID for uniqueness on "delete".\n')
      out_text <- cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'delete')


      if(is.null(out_text)){
        cat('\n  Sec4 - Asking if they are sure they want to delete.\n')

        ns <- shiny::NS(db_tbl_name)
        shiny::showModal(
          shiny::modalDialog(
            footer = NULL,
            title=paste0("Are you sure you want to delete row '",input_uid,"'?"),
            htmltools::tagList(shiny::actionButton(ns("confirmDelete"), label = "Yes, delete."), shiny::modalButton("No, cancel.") )
          )
        )

      } else {

        output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })

      }

    })



    # SECTION 5: CONFIRM ROW DELETE -----------------------------------------------
    shiny::observeEvent(input$confirmDelete, {
      cat('\n# SECTION 5: CONFIRM ROW DELETE -----------------------------------------------')
      cat('\n  Sec5 - Updates Interface, DB, and R tables when user clicks delete confirm button\n')
      table_edited <<- T

      cat("\n  Sec5 - Okie Dokie. They're sure they want to delete.\n")

      out_text <- NULL
      input_uid <- stringr::str_trim(input$uid)
      shiny::removeModal()

      output$key_editor_ui <- shiny::renderUI({
        cdr_row_editor_html('... Updating Database ...',db_tbl_name, add_row_permission, del_row_permission) })


      cat('\n  Sec5 - These values are being deleted:\n')
      vals_to_delete <- db_tbl |>
        dplyr::filter( input_uid == db_tbl[[key_col]] ) |>
        dplyr::select(tidyselect::where(~!all(is.na(.))))
      print(vals_to_delete)

      cat('\n  Sec5 - Replacing all the data in the row with NAs, from back to front.')
      cat('\n  Sec5 - And, creating this list of tibbles to append to the deltas table:\n')
      to_deltas_tbl <- vals_to_delete |>
        rev() |>
        purrr::imap(
          ~tibble::tibble(
            OBS_ID      = input_uid,
            FIELD       = .y,
            CHG_FROM    = as.character(.x),
            CHG_TO      = NA,
            WHO_EDITED  = user,
            WHEN_EDITED = lubridate::now(tzone = db_tz)
          ) )
      print(to_deltas_tbl)

      cat('\n  Sec5 - Updating deltas table in the R parent env and UI\n')
      chg_log_tbl <- to_deltas_tbl |>
        dplyr::bind_rows() |>
        dplyr::bind_rows(chg_log_tbl) |>
        dplyr::arrange(dplyr::desc(WHEN_EDITED))

      cat('\n  Sec5 - Updating Primary table in R parent env and UI\n')
      db_tbl <- db_tbl |> dplyr::filter( input_uid != db_tbl[[key_col]] )

      cat('\n  Sec5 - Updating deltas table in database...\n')
      purrr::map(.x = to_deltas_tbl,
                 .f = \(.x) cdr_update_db_deltas_tbl(
                   conn_pool     = conn_pool,
                   db_tbl_name   = deltas_table_id,
                   to_deltas_tbl = .x,
                   db_tzone      = db_tz
                 )
      )

      cat(paste0('\n  Sec5 - Deleting row from primary table in the DB.\n'))
      cdr_delete_row_in_db(
        conn_pool    = conn_pool,
        db_tbl_name  = primary_table_id,
        value_rowuid = input_uid,
        key_column   = key_col
      )


      db_tbl <<- db_tbl
      chg_log_tbl <<- chg_log_tbl
      DT::replaceData(proxy_db_tbl, db_tbl)
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)


      output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })

    })



    # SECTION 6: MULTIUSER CHECK ------------------------------------------------
    shiny::observeEvent( list(input$db_tbl_cell_edit, input$create_row_btn, input$confirmDelete), ignoreInit = T, {
      cat('\n\n# SECTION 6: MULTIUSER CHECK ------------------------------------------------')
      cat('\n  Sec6 - Re-syncs the tables if a different User made a change to the primary table')


      # only check for multiuser updates after the number of seconds in multiuser_update_wait
      if ( table_edited & (as.numeric(Sys.time() - last_multiuser_timechk > multiuser_update_wait ) ) ) {

        last_multiuser_timechk <- Sys.time() # reset

        cat('\n  Sec6 - Checking if someone else made updates to the tables ... \n')
        row_count_deltas_db <- dplyr::tbl(conn_pool, deltas_table_id) |>
          dplyr::summarize(dplyr::n()) |> dplyr::pull() |> as.integer()
        cat(fill = TRUE, paste("Your change-log table has",nrow(chg_log_tbl),"rows.") )
        cat(fill = TRUE, paste("The DB change-log table has",row_count_deltas_db,"rows.") )
        if( nrow(chg_log_tbl) != row_count_deltas_db ){
          cat('\n  Sec6 - Yep, there are new deltas. Someone else is inputting data. Updating your local tables.\n')
          db_tbl <- cdr_DB2RT_primary(conn_pool,db_tbl_name = primary_table_id, key_col = key_col)
          output$db_tbl <- cdr_RT2DT_primary(db_tbl, cell_edit_permission, lock_fields)
          chg_log_tbl <- cdr_DB2RT_chg_log(conn_pool, chg_log_tbl_name = deltas_table_id)
          output$chg_log_tbl <- cdr_RT2DT_chg_log(chg_log_tbl)
        } else {
          cat('\n  Sec6 - So, nope. Nobody else. You are the only one making changes right now.')
          cat('\n  Sec6 - Just you.')
          cat('\n  Sec6 - By your lonesome.')
          cat('\n  Sec6 - Maybe they ought to pay you more.\n')
        }

      } else {
        cat('\n  Sec6 - No need to re-sync yet.\n')
      }

      db_tbl <<- db_tbl
      chg_log_tbl <<- chg_log_tbl
      DT::replaceData(proxy_db_tbl, db_tbl)
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

    })


    cat(paste("\nModule Server env variables:\n"))
    print(ls())

    # SECTION 7: OUTPUT & TABLE JOINS -------------------------------------------------
    joined_tbls <- shiny::eventReactive(
      list(input$db_tbl_cell_edit, input$create_row_btn, input$confirmDelete), ignoreNULL=FALSE, {
      cat('\n\n# SECTION 7: TABLE JOINS -------------------------------------------------')
      cat('\n  Sec7 - Join Primary and Deltas tables as the modules returned output:\n')
      cdr_join_tbls(db_tbl, chg_log_tbl, key_col)
    })

    return(joined_tbls)


  } # END shiny::moduleServer


  )} # END cdr_manage_db_tbls


