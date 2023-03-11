#' Server module to present and control a db table
#'
#' This function is a server module that gets database tables from db specified by pool connection
#' and the db table name specified in 'id', server module manages and syncs
#' changes between the UI DT, in-server-memory tbl, and backend db, for the
#' primary as well as the corresponding deltas table (and joined table if it's
#' presented in the UI)
#'
#' @param id primary table name - namespace ID corresponding to the
#'   'primary_tbl_name' in the database
#' @param key_col name of the unique ID column in the db table (table must have
#'   a unique ID column with unique IDs)
#' @param db_conn_pool db connection from package 'pool'
#' @param session current shiny session
#' @param cell_edit_permission T or F: to make editable the primary table from the module
#'    (cell_edit_permission = T means the user can change the data)
#'    (cell_edit_permission = F means the user can only see the data)
#' @param add_row_permission T or F: allows user to add a row to the primary table from the module
#' @param del_row_permission T or F: allows user to delete a row on the primary table from the module
#' @param lock_fields strings: a vector of field names from the database to lock from editing
#'
#' @return returns DT reactive tables to the shiny ui environment
#' @export
#'
#' @examples \dontrun{
#' data("iris")
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' iris <- dplyr::mutate(iris,
#'            unique_id = paste0('uid_',stringr::str_pad(dplyr::row_number(),3,pad=0)))
#' crudr::cdr_create_tbls_in_db(db_conn_pool = con, db_tbl = iris)
#' server <- function(input, output, session){
#'              iris_r_tbl <- crudr::cdr_manage_db_tbls('iris', 'unique_id', con, session)
#'              output$iris <- DT::renderDT(iris_r_tbl())
#' }
#' ui <- fluidPage(DTOutput('iris'))
#' shinyApp(ui,server)
#'}

cdr_manage_db_tbls <- function(id, key_col, db_conn_pool, session,
                               add_row_permission   = F,
                               del_row_permission   = F,
                               cell_edit_permission = F,
                               lock_fields = c()
                               ){

  shiny::moduleServer(id, module = function(input, output, session){


# POST TABLES PRIMARY AND DELTA ---------------------------------------------------------
    cat('\n\n# SECTION: LOAD TABLES, INITIALIZE VARIABLES AND UI ------------------------------------\n')

    user <- ifelse(is.null(session$user), Sys.info()[['user']], session$user)

    cat('\n Create the UI above the primary table based on add row and delete row permissions\n')
    output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('', id, add_row_permission, del_row_permission) })

    cat('\n Download Primary table from the database and present it in the UI with permissions\n')
    db_tbl <- dplyr::tbl(db_conn_pool, id) %>% dplyr::collect() %>% dplyr::relocate(dplyr::all_of(key_col))
    proxy_db_tbl = DT::dataTableProxy('db_tbl')
    output$db_tbl <- DT::renderDT( DT::datatable(
      db_tbl,
      options = list(scrollX = TRUE,  keys = TRUE),
      callback = crudr::cdr_js_edit_ctrl(),
      extensions = "KeyTable",
      selection = 'none',
      editable = if(cell_edit_permission){
        list(target = "cell",
             disable = list(columns = c(0,1,which(names(db_tbl) %in% lock_fields))))}
    ))


    cat('\n Download Delta table from the database and present it in the UI\n')
    chg_log_tbl <- dplyr::tbl(db_conn_pool, crudr::cdr_name_delta_tbl(id)) %>%
      dplyr::collect() %>% dplyr::arrange(dplyr::desc(WHEN_EDITED))
    if(db_conn_pool$objClass[[1]] == "SQLiteConnection"){ chg_log_tbl <- dplyr::mutate(chg_log_tbl, WHEN_EDITED = lubridate::ymd_hms(WHEN_EDITED))}
    proxy_chg_log_tbl = DT::dataTableProxy('chg_log_tbl')
    output$chg_log_tbl <- DT::renderDT(
      DT::datatable(chg_log_tbl, selection = 'none') %>%
        DT::formatDate('WHEN_EDITED', method = 'toLocaleString')
    )

    cat('\n The Primary and Delta table Join occurs at the end of this script \n')




# EDIT CELL ---------------------------------------------------------------
    shiny::observeEvent(input$db_tbl_cell_edit, {
      cat('\n\n# LVL-B SECTION: EDIT CELL ----------------------------------------------------\n')
      cat('\n -Update Interface, DB, and R tables when user enters new data into a cell in the primary table\n')


      # Rapid changes brake the database connection.
      #  This catches any changes where the computer has not had time to update.
      if(nrow(input$db_tbl_cell_edit)==0) {
        cat('\n\n ########## WHOA! Slow down there Tiger. Computers gotta think too. ###########\n\n')
        return()
        }

      cat("\n Get the information that the User just typed into the UI and update the R primary table \n")
      to_update <- input$db_tbl_cell_edit
      old_mem_val <- db_tbl[ to_update[['row']], to_update[['col']] ][[1]]
      update_value = crudr::cdr_coerce_value(to_update[['value']], old_mem_val)
      value_colname = names(db_tbl)[to_update[['col']]]
      value_rowuid = db_tbl[ to_update[['row']], key_col ][[1]] # Unique key Id
      db_tbl[to_update[['row']], to_update[['col']]] <<- update_value
      DT::replaceData(proxy_db_tbl, db_tbl)


      cat("\n If the User put in nothing, then skip out of the cell update.\n")
      if(((is.na(old_mem_val) | is.null(old_mem_val) | identical(old_mem_val, '')) &
          (is.na(update_value) | is.null(update_value) | identical(update_value, ''))) |
          identical(old_mem_val, update_value)){
        cat(paste0("\nThe old value is '",old_mem_val,"' and the new value is '",
                    update_value,"'. Not updating the DB.\n"))
        return()
      }

      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('... Updating Database ...', id, add_row_permission, del_row_permission) })


      cat('\nCreating tibble to append changes to Delta table\n')
      # create append tibble
      to_deltas_tbl <- tibble::tibble(
        OBS_ID = value_rowuid,
        FIELD = value_colname,
        CHG_FROM = as.character(old_mem_val),
        CHG_TO = as.character(update_value),
        WHO_EDITED = user,
        WHEN_EDITED = Sys.time()
      )
      print(to_deltas_tbl)

      cat('\nUpdate deltas table in R parent env\n')
      chg_log_tbl <<- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) %>%
        dplyr::arrange(dplyr::desc(WHEN_EDITED))

      cat('\nUpdate deltas table in Ajax\n')
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      cat('\nUpdating primary table in Database\n')
      crudr::cdr_update_db_primary_tbl(
        db_conn_pool  = db_conn_pool,
        db_tbl_name   = id,
        update_value  = update_value,
        value_colname = value_colname,
        value_rowuid  = value_rowuid, # unique key identifier
        key_column    = key_col  # column with unique key identifier
      )

      cat('\nUpdating delta table in Database\n')
      crudr::cdr_update_db_deltas_tbl(
        db_conn_pool  = db_conn_pool,
        db_tbl_name   = crudr::cdr_name_delta_tbl(id),
        to_deltas_tbl = to_deltas_tbl
      )

      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('', id, add_row_permission, del_row_permission) })
      cat('\n\n# END SECTION: EDIT CELL ----------------------------------------------------\n')

    })




# NEW ROW -----------------------------------------------------------------
    shiny::observeEvent(input$create_row_btn, {
      cat('\n\n# LVL-B SECTION: NEW ROW --------------------------------------------------------\n')
      cat('\n Update Interface, DB, and R tables when user clicks button input$create_row_btn\n')

      input_uid <- stringr::str_trim(input$uid)

      out_text <- crudr::cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'create')

      if(is.null(out_text)){

        output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('... Updating Database ...', id, add_row_permission, del_row_permission) })

        db_tbl <<- dplyr::bind_rows(tibble::tibble({{key_col}} := input_uid), db_tbl)
        DT::replaceData(proxy_db_tbl, db_tbl)

        crudr::cdr_create_row_in_db(
          db_conn_pool  = db_conn_pool,
          db_tbl_name   = id,
          key_col       = key_col,
          input_uid     = input_uid
        )
        print(dplyr::slice(db_tbl,1))

        # create append tibble
        to_deltas_tbl <- tibble::tibble(
          OBS_ID = input_uid,
          FIELD = key_col,
          CHG_FROM = "",
          CHG_TO = input_uid,
          WHO_EDITED = user,
          WHEN_EDITED= Sys.time()
        )

        # update parent env
        chg_log_tbl <<- dplyr::bind_rows(chg_log_tbl, to_deltas_tbl) %>% dplyr::arrange(dplyr::desc(WHEN_EDITED))
        DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

        # update deltas db table
        crudr::cdr_update_db_deltas_tbl(
          db_conn_pool = db_conn_pool,
          db_tbl_name = crudr::cdr_name_delta_tbl(id),
          to_deltas_tbl = to_deltas_tbl
        )

      }

      # update the UI
      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html(out_text, id, add_row_permission, del_row_permission) })
      cat('\n# END SECTION: NEW ROW (input$create_row_btn) --------------------------------------------------------\n')

    })
# END NEW ROW -----------------------------------------------------------------



# DELETE ROW BUTTON PUSH --------------------------------------------------
    shiny::observeEvent(input$delete_row_btn, {
      cat('\n\n# LVL-B SECTION: DELETE ROW REQUEST -----------------------------------------------------\n')

      input_uid <- stringr::str_trim(input$uid)
      print(input_uid)


      out_text <- crudr::cdr_chk_uniq_id(db_tbl, input_uid, key_col, 'delete')

      if(is.null(out_text)){
        cat('\nAsking if they are sure they want to delete.\n')

        ns <- shiny::NS(id)
        shiny::showModal(
          shiny::modalDialog(
            footer = NULL,
            title=paste0("Are you sure you want to delete row '",input_uid,"'?"),
            htmltools::tagList(shiny::actionButton(ns("confirmDelete"), label = "Yes, delete."), shiny::modalButton("No, cancel.") )
          )
        )

      } else {

        cat('\n Update the UI row controls.\n\n')
        output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html(out_text, id, add_row_permission, del_row_permission) })
        cat('\n# LVL-B SUB-SECTION: DELETE ROW (update DB tables upon confirm) ----------\n\n')

      }

    })



# DELETE CONFIRM ----------------------------------------------------------
    shiny::observeEvent(input$confirmDelete, {
    cat('\n# LVL-B SUB-SECTION: DELETE ROW (update DB tables upon confirm) ----------')
      cat('\n -Update Interface, DB, and R tables when user clicks confirm button')
      cat("\nOkie Dokie. They're sure they want to delete.\n")

      out_text <- NULL
      input_uid <- stringr::str_trim(input$uid)
      print(paste('+++++++++++++++++',input_uid,'++++++++++++++++++++++++'))

      shiny::removeModal()

      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html('... Updating Database ...', id, add_row_permission, del_row_permission) })

      cat('\nWhich values are being deleted?\n')
      vals_to_delete <- db_tbl %>%
        dplyr::filter( input_uid == db_tbl[[key_col]] ) %>%
        dplyr::select(tidyselect::where(~!all(is.na(.))))
      print(vals_to_delete)

      cat('\nReplacing all the data in the row with NAs, from back to front.')
      cat('\nCreating list of tibbles to append  to deltas table for observation:')
      to_deltas_tbl <- vals_to_delete %>%
        rev() %>%
        purrr::imap(
          ~tibble::tibble(
            OBS_ID      = input_uid,
            FIELD       = .y,
            CHG_FROM    = as.character(.x),
            CHG_TO      = NA,
            WHO_EDITED  = user,
            WHEN_EDITED = Sys.time() )
        )
      print(to_deltas_tbl)

      cat('\nUpdate deltas table in R parent env\n')
      chg_log_tbl <<- to_deltas_tbl %>%
        dplyr::bind_rows() %>%
        dplyr::bind_rows(chg_log_tbl) %>%
        dplyr::arrange(dplyr::desc(WHEN_EDITED))

      cat('\nUpdate deltas table in UI\n')
      DT::replaceData(proxy_chg_log_tbl, chg_log_tbl)

      cat('\nUpdate primary table in R parent env\n')
      db_tbl <<- db_tbl %>% dplyr::filter( input_uid != db_tbl[[key_col]] )

      cat('\nUpdate primary table in UI\n')
      DT::replaceData(proxy_db_tbl, db_tbl)

      cat('\nUpdating deltas table in database...\n')
      to_deltas_tbl %>%
        purrr::map(
          ~ crudr::cdr_update_db_deltas_tbl(
            db_conn_pool  = db_conn_pool,
            db_tbl_name   = crudr::cdr_name_delta_tbl(id),
            to_deltas_tbl = . )
        )

      cat(paste0('\nDeleting row from primary table in the DB.\n'))
      crudr::cdr_delete_row_in_db(
        db_conn_pool = db_conn_pool,
        db_tbl_name  = id,
        value_rowuid = input_uid,
        key_column   = key_col
      )
      cat('\n### Finished Deleting Stuff from the DB.\n')

      cat('\n Update the UI row controls.\n')
      output$key_editor_ui <- shiny::renderUI({ crudr::cdr_row_editor_html(out_text, id, add_row_permission, del_row_permission) })

      cat('\n# LVL-C SUB-SECTION: DELETE ROW (input$confirmDelete) ----------\n')
    })


    # shiny::eventReactive(list(input$create_row_btn, input$delete_row_btn, input$confirmDelete),
    #                      {cdr_join_tbls(db_tbl, chg_log_tbl, key_col)},
    #                      ignoreNULL=FALSE)



    # POST TABLE JOIN --------------------------------------------------------------------
    cat('\n\n# LVL-A SECTION: JOIN PRIMARY AND DELTA TABLES AFTER UPDATE ---------------------')
    shiny::eventReactive(
      eventExpr = list(input$db_tbl_cell_edit, input$create_row_btn, input$confirmDelete),
      valueExpr = {cdr_join_tbls(db_tbl, chg_log_tbl, key_col)},
      ignoreNULL=FALSE
    )


  } # END shiny::moduleServer

  )} # END crudr::cdr_manage_db_tbls

