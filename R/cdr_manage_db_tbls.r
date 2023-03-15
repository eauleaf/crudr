#' Server module to present and control a db table
#'
#' This function is a server module that gets database tables from db specified by pool connection
#' and the db table name specified in 'db_tbl_name', server module manages and syncs
#' changes between the UI DT, in-server-memory tbl, and backend db, for the
#' primary as well as the corresponding deltas table (and joined table if it's
#' presented in the UI)
#'
#' @param db_tbl_name primary table name - namespace ID corresponding to the
#'   'primary_tbl_name' in the database
#' @param key_col name of the unique ID column in the db table (table must have
#'   a unique ID column with unique IDs)
#' @param db_conn_pool db connection from package 'pool'
#' @param session current shiny session
#' @param cell_edit_permission T or F: to make editable the primary table from the module
#'    (cell_edit_permission = T means the user can change the data)
#'    (cell_edit_permission = F means the user can only see the data)
#' @param add_row_permission T or F: allows user to add a row to the primary table of the module
#' @param del_row_permission T or F: allows user to delete a row on the primary table of the module
#' @param lock_fields strings: a vector of field names from the database to lock from admin editing
#'
#' @return returns DT reactive tables to the shiny ui environment
#' @export
#'
#' @examples \dontrun{
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' crudr::cdr_create_tbls_in_db(con, iris)
#' server <- function(input, output, session){
#'              r_tbl <- crudr::cdr_manage_db_tbls('iris', 'UID', con, session)
#'              output$iris <- DT::renderDT(r_tbl()) }
#' ui <- shiny::fluidPage(DT::DTOutput('iris'))
#' shiny::shinyApp(ui,server)
#'}

cdr_manage_db_tbls <- function(db_tbl_name, key_col, db_conn_pool, session,
                               add_row_permission   = F,
                               del_row_permission   = F,
                               cell_edit_permission = F,
                               lock_fields = c()
                               ){

  shiny::moduleServer(db_tbl_name, module = function(input, output, session){



# SECTION 1: INITIALIZE ---------------------------------------------------
    cat('\n\n# SECTION 1: INITIALIZE ---------------------------------------------------')
    cat('\n  S1 - Gets and Posts the Primary and Change log tables\n')
    table_edited <- F #initialize

    user <- ifelse(is.null(session$user), Sys.info()[['user']], session$user)

    cat('\n  S1 - Create the UI above the primary table based on add row and delete row permissions\n')
    output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('',db_tbl_name, add_row_permission, del_row_permission) })
    cat('\n  S1 - Collect, sync, and present the Primary table')
    output$db_tbl <- cdr_impart_primary_tbl(db_conn_pool,db_tbl_name, key_col, cell_edit_permission, lock_fields)
    cat('\n  S1 - Collect, sync, and present the Deltas table')
    output$chg_log_tbl <- cdr_impart_chg_log_tbl(db_conn_pool, crudr::cdr_name_delta_tbl(db_tbl_name))




# SECTION 2: EDIT CELL ---------------------------------------------------------------
    shiny::observeEvent(input$db_tbl_cell_edit, {
      cat('\n\n# SECTION 2: EDIT CELL ----------------------------------------------------')
      cat('\n  S2 - Updates Interface, DB, and R tables when user enters new data into a cell in the primary table\n')
      table_edited <<- T

      if(nrow(input$db_tbl_cell_edit)==0) {
        cat('\n  S2 - WHOA! Slow down there Tiger. Computers gotta think too. \n\n')
        table_edited <<- F
        return()
      }


      cat("\n  S2 - Collect the information that the User just typed into the UI\n")
      to_update <- input$db_tbl_cell_edit
      old_mem_val <- db_tbl[ to_update[['row']], to_update[['col']] ][[1]]
      update_value <- crudr::cdr_coerce_value(to_update[['value']], old_mem_val)
      value_colname <- names(db_tbl)[to_update[['col']]]
      value_rowuid <- db_tbl[ to_update[['row']], key_col ][[1]] # Unique key Id


      cat("\n  S2 - If the User put in nothing, then skip out of the cell update.\n")
      if(((is.na(old_mem_val) | is.null(old_mem_val) | identical(old_mem_val, '')) &
          (is.na(update_value) | is.null(update_value) | identical(update_value, ''))) |
         identical(old_mem_val, update_value)){
        cat(glue::glue(.trim = F, "\n  S2 - The old value is '{old_mem_val}'",
                       "and the new value is '{update_value}'. Not updating the DB.\n"))
        table_edited <<- F
        return()
      }


      cat("\n  S2 - Update the R primary table and UI proxy table\n")
      db_tbl[to_update[['row']], to_update[['col']]] <<- update_value
      DT::replaceData(proxy_db_tbl, db_tbl)


      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('... Updating Database ...',db_tbl_name, add_row_permission, del_row_permission) })


      cat('\n  S2 - Creating tibble to append changes to Delta table\n')
      to_deltas_tbl <- tibble::tibble(
        OBS_ID = value_rowuid,
        FIELD = value_colname,
        CHG_FROM = as.character(old_mem_val),
        CHG_TO = as.character(update_value),
        WHO_EDITED = user,
        WHEN_EDITED = lubridate::now(tzone = 'UTC')
      )

      cat('\n  S2 - Update deltas table in R parent env\n')
      chg_log_tbl <<- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) %>% dplyr::arrange(dplyr::desc(WHEN_EDITED))

      cat('\n  S2 - Update deltas proy table for UI\n')
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      cat('\n  S2 - Updating primary table in Database\n')
      crudr::cdr_update_db_primary_tbl(
        db_conn_pool  = db_conn_pool,
        db_tbl_name   = db_tbl_name,
        update_value  = update_value,
        value_colname = value_colname,
        value_rowuid  = value_rowuid, # unique key identifier
        key_column    = key_col  # column name for unique key identifier
      )

      cat('\n  S2 - Updating delta table in Database\n')
      crudr::cdr_update_db_deltas_tbl(
        db_conn_pool  = db_conn_pool,
        db_tbl_name   = crudr::cdr_name_delta_tbl(db_tbl_name),
        to_deltas_tbl = to_deltas_tbl
      )

      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('',db_tbl_name, add_row_permission, del_row_permission) })



    })



# SECTION 3: NEW ROW ------------------------------------------------------
    shiny::observeEvent(input$create_row_btn, {
      cat('\n\n# SECTION 3: NEW ROW ------------------------------------------------------')
      cat('\n  S3 - Update Interface, DB, and R tables when user clicks button input$create_row_btn\n')
      table_edited <<- T


      input_uid <- stringr::str_trim(input$uid)


      cat('\n  S3 - Check unique ID for uniqueness on "create".\n')
      out_text <- crudr::cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'create')


      if(is.null(out_text)){

        output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('... Updating Database ...',db_tbl_name, add_row_permission, del_row_permission) })

        db_tbl <<- dplyr::bind_rows(tibble::tibble({{key_col}} := input_uid), db_tbl)
        DT::replaceData(proxy_db_tbl, db_tbl)

        cat('\n  S3 - Run SQL to make new row in Primary table.\n')
        crudr::cdr_create_row_in_db(
          db_conn_pool  = db_conn_pool,
          db_tbl_name   = db_tbl_name,
          key_col       = key_col,
          input_uid     = input_uid
        )


        cat('\n  S3 - Create append deltas tibble.\n')
        to_deltas_tbl <- tibble::tibble(
          OBS_ID = input_uid,
          FIELD = key_col,
          CHG_FROM = "",
          CHG_TO = input_uid,
          WHO_EDITED = user,
          WHEN_EDITED = lubridate::now(tzone = 'UTC')
        )

        cat('\n  S3 - Update parent env and proxy Deltas tables.\n')
        chg_log_tbl <<- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) %>% dplyr::arrange(dplyr::desc(WHEN_EDITED))
        DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

        cat('\n  S3 - Update database Deltas table.\n')
        crudr::cdr_update_db_deltas_tbl(
          db_conn_pool = db_conn_pool,
          db_tbl_name = crudr::cdr_name_delta_tbl(db_tbl_name),
          to_deltas_tbl = to_deltas_tbl
        )

      }

      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })


    })



# SECTION 4: DELETE ROW REQUEST ---------------------------------------------------
    shiny::observeEvent(input$delete_row_btn, {
      cat('\n\n# SECTION 4: DELETE ROW REQUEST ---------------------------------------------------\n')
      cat('\n  S4 - Throws a delete popup to determine if the user is serious\n')


      input_uid <- stringr::str_trim(input$uid)

      cat('\n  S4 - Check unique ID for uniqueness on "delete".\n')
      out_text <- crudr::cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'delete')


      if(is.null(out_text)){
        cat('\n  S4 - Asking if they are sure they want to delete.\n')

        ns <- shiny::NS(db_tbl_name)
        shiny::showModal(
          shiny::modalDialog(
            footer = NULL,
            title=paste0("Are you sure you want to delete row '",input_uid,"'?"),
            htmltools::tagList(shiny::actionButton(ns("confirmDelete"), label = "Yes, delete."), shiny::modalButton("No, cancel.") )
          )
        )

      } else {

        output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })

      }

    })



# SECTION 5: CONFIRM ROW DELETE -----------------------------------------------
    shiny::observeEvent(input$confirmDelete, {
    cat('\n# SECTION 5: CONFIRM ROW DELETE -----------------------------------------------')
      cat('\n  S5 - Updates Interface, DB, and R tables when user clicks delete confirm button\n')
      table_edited <<- T

      cat("\n  S5 - Okie Dokie. They're sure they want to delete.\n")

      out_text <- NULL
      input_uid <- stringr::str_trim(input$uid)
      shiny::removeModal()

      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('... Updating Database ...',db_tbl_name, add_row_permission, del_row_permission) })


      cat('\n  S5 - These values are being deleted:\n')
      vals_to_delete <- db_tbl %>%
        dplyr::filter( input_uid == db_tbl[[key_col]] ) %>%
        dplyr::select(tidyselect::where(~!all(is.na(.))))
      print(vals_to_delete)

      cat('\n  S5 - Replacing all the data in the row with NAs, from back to front.')
      cat('\n  S5 - And, creating this list of tibbles to append to the deltas table:\n')
      to_deltas_tbl <- vals_to_delete %>%
        rev() %>%
        purrr::imap(
          ~tibble::tibble(
            OBS_ID      = input_uid,
            FIELD       = .y,
            CHG_FROM    = as.character(.x),
            CHG_TO      = NA,
            WHO_EDITED  = user,
            WHEN_EDITED = lubridate::now(tzone = 'UTC')
            ) )
      print(to_deltas_tbl)

      cat('\n  S5 - Updating deltas table in the R parent env and UI\n')
      chg_log_tbl <<- to_deltas_tbl %>%
        dplyr::bind_rows() %>%
        dplyr::bind_rows(chg_log_tbl) %>%
        dplyr::arrange(dplyr::desc(WHEN_EDITED))
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      cat('\n  S5 - Updating Primary table in R parent env and UI\n')
      db_tbl <<- db_tbl %>% dplyr::filter( input_uid != db_tbl[[key_col]] )
      DT::replaceData(proxy_db_tbl, db_tbl)

      cat('\n  S5 - Updating deltas table in database...\n')
      purrr::map(.x = to_deltas_tbl,
                 .f = ~ crudr::cdr_update_db_deltas_tbl(
                   db_conn_pool  = db_conn_pool,
                   db_tbl_name   = crudr::cdr_name_delta_tbl(db_tbl_name),
                   to_deltas_tbl = .x )
      )

      cat(paste0('\n  S5 - Deleting row from primary table in the DB.\n'))
      crudr::cdr_delete_row_in_db(
        db_conn_pool = db_conn_pool,
        db_tbl_name  = db_tbl_name,
        value_rowuid = input_uid,
        key_column   = key_col
      )

      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html(out_text,db_tbl_name, add_row_permission, del_row_permission) })

    })



# SECTION 6: MULTIUSER CHECK ------------------------------------------------
    shiny::observeEvent( list(input$db_tbl_cell_edit, input$create_row_btn, input$confirmDelete), ignoreInit = T, {
      cat('\n\n# SECTION 6: MULTIUSER CHECK ------------------------------------------------')
      cat('\n  S6 - Re-syncs the tables if a different User made a change to the primary table')

      if ( table_edited ) {

        cat('\n  S6 - Checking if someone else made updates to the tables ... ')
        row_count_deltas_db <- dplyr::tbl(con, crudr::cdr_name_delta_tbl('IRIS')) %>%
          dplyr::summarize(n()) %>% dplyr::collect() %>% as.integer()
        if( nrow(chg_log_tbl) != row_count_deltas_db ){
          cat('\n  S6 - Yep, there are new deltas. Someone else is inputting data. Updating your local tables.\n')
          output$db_tbl <- cdr_impart_primary_tbl(db_conn_pool,db_tbl_name, key_col, cell_edit_permission, lock_fields)
          output$chg_log_tbl <- cdr_impart_chg_log_tbl(db_conn_pool, crudr::cdr_name_delta_tbl(db_tbl_name))
        } else {
          cat('\n  S6 - Nope. You are the only one making changes right now. The only one. Just you--by your lonesome.')
          cat('\n  S6 - Maybe they ought to pay you more.\n')
        }

      } else {
        cat('\n  S6 - No need to re-sync yet.\n')
      }

    })



# SECTION 7: TABLE JOINS -------------------------------------------------
    shiny::eventReactive(  list(input$db_tbl_cell_edit, input$create_row_btn, input$confirmDelete), ignoreNULL=FALSE, {
      cat('\n\n# SECTION 7: TABLE JOINS -------------------------------------------------')
      cat('\n  S7 - Join Primary and Deltas tables as the modules returned output:\n')
      cdr_join_tbls(db_tbl, chg_log_tbl, key_col)
    })




  } # END shiny::moduleServer


  )} # END crudr::cdr_manage_db_tbls


