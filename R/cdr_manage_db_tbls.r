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

    if(!exists('module_count')){ module_call_count <- 0 }
    module_call_count <- module_call_count + 1
    cat('\n\n\n\n@ MODULE SERVER CALL BEGUN ( for',db_tbl_name,'- call count',
        module_call_count,')\n\n\n')


# SECTION 1: INITIALIZE TABLES ---------------------------------------------------
    cat('\n\n##### SECTION 1: SETUP & VARIABLES -----------------------------------------')
    cat('\nSection 1 initializes server-module variables.\n')

    cat('\n  Sec1 - Initializing variables ... ')
    db_tz <- cdr_adj_timezone(conn_pool)
    WHEN_EDITED <- NULL
    table_edited <- FALSE
    last_multiuser_timechk <- Sys.time()
    user <- ifelse(is.null(session$user), Sys.info()[['user']], session$user)

    primary_table_id <- cdr_id(table = db_tbl_name, ...)
    deltas_tbl_name <- cdr_name_delta_tbl(db_tbl_name, chg_log_suffix = chg_log_suffix)
    deltas_table_id <- cdr_id(table = deltas_tbl_name, ...)


    cat('\n  Sec1 - Creating the UI above the primary table based on add-row and delete-row permissions ... ')
    output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(
      '', db_tbl_name, add_row_permission, del_row_permission)
    })

    cat('\n  Sec1 - Getting, syncing, and posting the Primary table ... ')
    db_tbl <- cdr_DB2RT_primary(conn_pool,db_tbl_name = primary_table_id, key_col = key_col)
    output$db_tbl <- cdr_RT2DT_primary(db_tbl,cell_edit_permission,lock_fields)
    proxy_db_tbl <- DT::dataTableProxy('db_tbl')

    cat('\n  Sec1 - Getting, synching, and posting the Change-log table ... ')
    chg_log_tbl <- cdr_DB2RT_chg_log(conn_pool, chg_log_tbl_name = deltas_table_id)
    output$chg_log_tbl <- cdr_RT2DT_chg_log(chg_log_tbl)
    proxy_chg_log_tbl <- DT::dataTableProxy('chg_log_tbl')

    cat('\n\nSECTION 1: COMPLETED ----------------------------------------------\n\n\n')



# SECTION 2: EDIT CELL ---------------------------------------------------------------
    shiny::observeEvent(input$db_tbl_cell_edit, {
      cat('\n\n##### SECTION 2: EDIT CELL ----------------------------------------------------')
      cat('\nSection 2 updates the Interface, DB, and R tables when user edits a cell.\n')

      table_edited <<- TRUE

      if(nrow(input$db_tbl_cell_edit)==0) {
        cat('\n  Sec2 - WHOA! Slow down there Tiger. Computers gotta think too. \n\n')
        table_edited <<- FALSE
        return()
      }

      cat("\n  Sec2 - Collecting the information that the User just typed into the UI ... ")
      to_update <- input$db_tbl_cell_edit
      old_mem_val <- db_tbl[ to_update[['row']], to_update[['col']] ][[1]]
      update_value <- cdr_coerce_value(to_update[['value']], old_mem_val)
      value_colname <- names(db_tbl)[to_update[['col']]]
      value_rowuid <- db_tbl[ to_update[['row']], key_col ][[1]] # Unique key Id


      cat("\n  Sec2 - Checking if the User entered something new ... ")
      if(
        identical(old_mem_val, update_value) || (
          (is.na(old_mem_val)  || is.null(old_mem_val)  || identical(old_mem_val, '')) &&
          (is.na(update_value) || is.null(update_value) || identical(update_value, ''))
        )
      ){
        cat("\n  Sec2 - The old value is ",old_mem_val,
            "and the new value is",update_value,
            ". Not updating the DB.")

        cat("\n  Sec2 - Given no user edit, syncing just the R primary table and
            UI proxy table with the UI interface table.")
        db_tbl[to_update[['row']], to_update[['col']]] <- update_value
        db_tbl <<- db_tbl
        DT::replaceData(proxy_db_tbl, db_tbl)
        table_edited <<- FALSE
        return()
      }

      cat("\n  Sec2 - Yep, the User entered some info to process, so:")

      cat("\n  Sec2 - Updating the R primary table and UI proxy table")
      db_tbl[to_update[['row']], to_update[['col']]] <- update_value

      output$key_editor_ui <- shiny::renderUI({
        cdr_row_editor_html('... Updating Database ...',
                            db_tbl_name,
                            add_row_permission, del_row_permission)
      })

      cat('\n  Sec2 - Creating tibble to append changes to Delta table ... ')
      cat('\n\t value_rowuid:',value_rowuid)
      cat('\n\t value_colname:',value_colname)
      cat('\n\t old_mem_val:',old_mem_val)
      cat('\n\t update_value:',update_value)
      cat('\n\t timestamp:',lubridate::now(tzone = db_tz))

      to_deltas_tbl <- tibble::tibble(
        OBS_ID = value_rowuid,
        FIELD = value_colname,
        CHG_FROM = as.character(old_mem_val),
        CHG_TO = as.character(update_value),
        WHO_EDITED = user,
        WHEN_EDITED = lubridate::now(tzone = db_tz)
      )

      cat('\n  Sec2 - Updating deltas table in R parent env ... ')
      chg_log_tbl <- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) |>
        dplyr::arrange(dplyr::desc(WHEN_EDITED))


      cat('\n  Sec2 - Updating primary table in Database ... ')
      cdr_update_db_primary_tbl(
        conn_pool     = conn_pool,
        db_tbl_name   = primary_table_id,
        update_value  = update_value,
        value_colname = value_colname, # specific column name to insert value
        value_rowuid  = value_rowuid, # specific row UID to insert value
        key_column    = key_col
      )

      cat('\n  Sec2 - Updating the delta table in Database ... ')
      cdr_update_db_deltas_tbl(
        conn_pool     = conn_pool,
        db_tbl_name   = deltas_table_id,
        to_deltas_tbl = to_deltas_tbl,
        db_tzone      = db_tz
      )

      cat('\n  Sec2 - Updating the R and UI proxy "Deltas" tables ... ')
      db_tbl <<- db_tbl
      chg_log_tbl <<- chg_log_tbl
      DT::replaceData(proxy_db_tbl, db_tbl)
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html('',db_tbl_name, add_row_permission, del_row_permission) })

      cat('\n\nSECTION 2: COMPLETED -----------------------------------------------\n\n\n')
    })



# SECTION 3: NEW ROW ------------------------------------------------------
    shiny::observeEvent(input$create_row_btn, {
      cat('\n\n##### SECTION 3: CREATE NEW ROW ---------------------------------------------------')
      cat('\nSection 3 updates the Interface, DB, and R tables when the user clicks
          the create-new-row button.\n')


      input_uid <- stringr::str_trim(input$uid)

      cat('\n  Sec3 - Checking unique ID for uniqueness on "create" request ... ')
      out_text <- cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'create')


      if(is.null(out_text)){
      table_edited <<- TRUE
        cat('\n  Sec3 - ID is unique. Processing change ... ')
        output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(
          '... Updating Database ...',db_tbl_name, add_row_permission, del_row_permission) })


        cat('\n  Sec3 - Running SQL to make new row in Primary table ... ')
        cdr_create_row_in_db(
          conn_pool   = conn_pool,
          db_tbl_name = primary_table_id,
          key_col     = key_col,
          input_uid   = input_uid
        )

        cat('\n  Sec3 - Creating append deltas tibble.\n')
        to_deltas_tbl <- tibble::tibble(
          OBS_ID = input_uid,
          FIELD = key_col,
          # CHG_FROM = "",
          CHG_FROM = NA_character_,
          CHG_TO = input_uid,
          WHO_EDITED = user,
          WHEN_EDITED = lubridate::now(tzone = db_tz)
        )

        cat('\n  Sec3 - Updating database, R, and proxy change-log tables.')
        cdr_update_db_deltas_tbl(
          conn_pool = conn_pool,
          db_tbl_name = deltas_table_id,
          to_deltas_tbl = to_deltas_tbl
        )
        db_tbl <- dplyr::bind_rows(tibble::tibble("{key_col}" := input_uid), db_tbl)
        chg_log_tbl <- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) |>
          dplyr::arrange(dplyr::desc(WHEN_EDITED))
        db_tbl <<- db_tbl
        chg_log_tbl <<- chg_log_tbl
        DT::replaceData(proxy_db_tbl, db_tbl)
        DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      } else {
        cat('\n  Sec3 - ID is NOT unique. No row added. ')
        table_edited <<- FALSE
      }


      output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })

      cat('\n\nSECTION 3: COMPLETED -----------------------------------------------\n\n\n')
    })



# SECTION 4: DELETE ROW REQUEST ---------------------------------------------------
    shiny::observeEvent(input$delete_row_btn, {
      cat('\n\n##### SECTION 4: DELETE ROW REQUEST ---------------------------------------------')
      cat('\nSection 4 sends a modal popup to determine if the user is serious about deleting a row.\n')


      input_uid <- stringr::str_trim(input$uid)

      cat('\n  Sec4 - Checking unique ID for uniqueness on "delete" request ... ')
      out_text <- cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'delete')


      if(is.null(out_text)){
        cat('\n  Sec4 - ID is unique.\n')
        cat('\n  Sec4 - Asking if they are sure they want to delete ... ')

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

      cat('\n\nSECTION 4: COMPLETED -----------------------------------------------\n\n\n')
    })



# SECTION 5: DELETE ROW -------------------------------------------------------
    shiny::observeEvent(input$confirmDelete, {
      cat('\n\n##### SECTION 5: DELETE ROW ---------------------------------------------------')
      cat('\nSection 5 updates Interface, DB, and R tables when user clicks delete confirm button.\n')

      cat("\n  Sec5 - Okie Dokie. They're sure they want to delete.\n")
      table_edited <<- TRUE
      out_text <- NULL
      input_uid <- stringr::str_trim(input$uid)
      shiny::removeModal()

      output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(
        '... Updating Database ...', db_tbl_name, add_row_permission, del_row_permission) })

      cat('\n  Sec5 - These values are being deleted:\n')
      vals_to_delete <- db_tbl |>
        dplyr::filter( input_uid == db_tbl[[key_col]] ) |>
        dplyr::select(tidyselect::where(~!all(is.na(.))))
      print(vals_to_delete)

      cat('\n  Sec5 - Replacing all the data in the row with NAs, from back to front,
          and creating a list of tibbles to append to the deltas table ... ')
      # tmp_timestamp <- lubridate::now(tzone = db_tz)
      to_deltas_tbl <- vals_to_delete |> rev() |>
        purrr::imap(
          ~tibble::tibble(
            OBS_ID      = input_uid,
            FIELD       = .y,
            CHG_FROM    = as.character(.x),
            CHG_TO      = NA,
            WHO_EDITED  = user,
            WHEN_EDITED = lubridate::now(tzone = db_tz)
            # WHEN_EDITED = tmp_timestamp
          ) )
      print(to_deltas_tbl)

      cat('\n  Sec5 - Updating Deltas R table ... ')
      chg_log_tbl <- to_deltas_tbl |>
        dplyr::bind_rows() |>
        dplyr::bind_rows(chg_log_tbl) |>
        dplyr::arrange(dplyr::desc(WHEN_EDITED))

      cat('\n  Sec5 - Updating Primary R table ... ')
      db_tbl <- db_tbl |> dplyr::filter( input_uid != db_tbl[[key_col]] )

      cat('\n  Sec5 - Updating deltas table in database ... ')
      purrr::map(.x = to_deltas_tbl,
                 .f = \(.x) cdr_update_db_deltas_tbl(
                   conn_pool     = conn_pool,
                   db_tbl_name   = deltas_table_id,
                   to_deltas_tbl = .x,
                   db_tzone      = db_tz
                 )
      )

      cat('\n  Sec5 - Deleting row from primary table in the DB ... ')
      cdr_delete_row_in_db(
        conn_pool    = conn_pool,
        db_tbl_name  = primary_table_id,
        value_rowuid = input_uid,
        key_column   = key_col
      )

      cat('\n  Sec5 - Updating the R and UI proxy tables ... ')
      db_tbl <<- db_tbl
      chg_log_tbl <<- chg_log_tbl
      DT::replaceData(proxy_db_tbl, db_tbl)
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)


      output$key_editor_ui <- shiny::renderUI({ cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })

      cat('\n\nSECTION 5: COMPLETED -----------------------------------------------\n\n\n')
    })



# SECTION 6: MULTIUSER CHECK ------------------------------------------------
    shiny::observeEvent( list(input$db_tbl_cell_edit, input$create_row_btn, input$confirmDelete), ignoreInit = T, {
      cat('\n\n##### SECTION 6: MULTIUSER CHECK ------------------------------------------------')
      cat('\nSection 6 re-syncs the tables if a different User made a change to the primary table.\n')


      # only check for multi-user updates after the number of seconds in multiuser_update_wait
      #  and if the user edited the table
      cat('\n  Sec6 - Checking if we made any changes to the tables ... ')
      if ( table_edited && (as.numeric( Sys.time() - last_multiuser_timechk > multiuser_update_wait ) ) ) {

        last_multiuser_timechk <<- Sys.time() # reset

        cat('\n  Sec6 - Checking if another user made updates to the tables ... ')
        row_count_deltas_db <- dplyr::tbl(conn_pool, deltas_table_id) |>
          dplyr::summarize(dplyr::n()) |> dplyr::pull() |> as.integer()
        cat('\n\t',paste("Your change-log table has",nrow(chg_log_tbl),"rows.") )
        cat('\n\t',paste("The DB change-log table has",row_count_deltas_db,"rows.") )
        if( nrow(chg_log_tbl) != row_count_deltas_db ){
          cat('\n  Sec6 - Yep, there are new deltas. Someone else is inputting data.
              Updating your local tables...')
          db_tbl <- cdr_DB2RT_primary(conn_pool,db_tbl_name = primary_table_id, key_col = key_col)
          output$db_tbl <- cdr_RT2DT_primary(db_tbl, cell_edit_permission, lock_fields)
          chg_log_tbl <- cdr_DB2RT_chg_log(conn_pool, chg_log_tbl_name = deltas_table_id)
          output$chg_log_tbl <- cdr_RT2DT_chg_log(chg_log_tbl)
          db_tbl <<- db_tbl
          chg_log_tbl <<- chg_log_tbl
          DT::replaceData(proxy_db_tbl, db_tbl)
          DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)
          cat('\n  Sec6 - Tables updated.\n')
        } else {
          cat('\n  Sec6 - So, nope. Nobody else. You are the only one making changes right now.
              Just you.
              By your lonesome.')
          cat('\n  Sec6 - Maybe they ought to pay you more.\n')
        }

        #reset
        table_edited <<- FALSE

      }

      cat('\n\nSECTION 6: COMPLETED -----------------------------------------------\n\n\n')

    })



# SECTION 7: OUTPUT & TABLE JOINS -------------------------------------------------
    joined_tbls <- shiny::eventReactive(
      list(input$db_tbl_cell_edit, input$create_row_btn, input$confirmDelete), ignoreNULL=FALSE, {
        cat('\n\n##### SECTION 7: JOIN TABLES -------------------------------------------------')
        cat('\nSection 7 joins the Primary and Deltas tables for the end-user output view.\n')
        out_tbl <- cdr_join_tbls(db_tbl, chg_log_tbl, key_col)
        cat('\n\nSECTION 7: COMPLETED -----------------------------------------------\n\n\n')
        return(out_tbl)
      })



    # cat("\n\nModule Server env variables:\n")
    # print(ls())
    # cat('@ MODULE SERVER CALL COMPLETED -----------------------------------------------\n\n\n')
    cat('\n@ MODULE SERVER CALL COMPLETED ( for',db_tbl_name,' count',
        module_call_count,')\n\n\n')

    return(joined_tbls)

  } # END shiny::moduleServer


  )} # END cdr_manage_db_tbls


