#' Server module to present and control db tables
#'
#' Shiny server module gets database tables from db specified by pool connection
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
#' @param open_sesame T or F to make editable the primary table from the module
#'
#' @return returns DT reactive tables to the shiny ui environment
#' @export
#'
#' @examples \dontrun{
#' data("iris")
#' con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
#' iris <- dplyr::mutate(iris,
#'            unique_id = paste0('uid_',stringr::str_pad(dplyr::row_number(),3,pad=0)))
#' cdr_create_new_db_tbls(db_conn_pool = con, db_tbl = iris)
#' server <- function(input, output, session){
#'              iris_tbl_out <- cdr_mod_tbl_server('iris', 'unique_id', con, session, open_sesame = T)
#'              output$iris <- DT::renderDT(iris_tbl_out())
#' }
#' ui <- fluidPage(DTOutput('iris'))
#' shinyApp(ui,server)
#'}

cdr_mod_tbl_server <- function(id, key_col, db_conn_pool, session, open_sesame = F){
  shiny::moduleServer(id, module = function(input, output, session){

    user <- ifelse(is.null(session$user), Sys.info()[['user']], session$user)


    ### primary edit tbl
    output$uid_btn <- cdr_render_new_row_ui('', id, open_sesame)

    db_tbl <- dplyr::tbl(db_conn_pool, id) %>% dplyr::collect() %>% dplyr::relocate(dplyr::all_of(key_col))
    proxy_db_tbl = DT::dataTableProxy('db_tbl')
    output$db_tbl <- DT::renderDT(
      DT::datatable(db_tbl,
                    options = list(scrollX = TRUE,  keys = TRUE),
                    callback = cdr_js_edit_ctrl(),
                    extensions = "KeyTable",
                    selection = 'none',
                    editable = if(open_sesame){list(target = "cell", disable = list(columns = c(0,1)))}
      ))


    ### deltas tbl
    db_tbl_deltas <- dplyr::tbl(db_conn_pool, cdr_deltas_tbl_name(id)) %>% dplyr::collect() %>% dplyr::arrange(dplyr::desc(when))
    if(class(db_tbl_deltas$when)[1] == 'numeric'){ # for SQLite
      db_tbl_deltas <- dplyr::mutate(db_tbl_deltas, when = as.POSIXct(when,origin = "1970-01-01"))}
    proxy_db_tbl_deltas = DT::dataTableProxy('db_tbl_deltas')
    output$db_tbl_deltas <- DT::renderDT(
      DT::datatable(db_tbl_deltas, selection = 'none') %>%
        DT::formatDate('when', method = 'toLocaleString')
    )


    ###listener to update db and tables when user updates the primary table
    shiny::observeEvent(input$db_tbl_cell_edit, {

      cat('\n\n###### CELL EDIT #######\n\n')
      print(input$db_tbl_cell_edit)

      to_update <- input$db_tbl_cell_edit
      row_idx <- to_update[['row']]
      col_idx <- to_update[['col']]
      old_mem_val <- db_tbl[row_idx, col_idx][[1]]
      print(typeof(to_update[['value']]))
      print(class(to_update[['value']]))

      update_value = cdr_coerce_value(to_update[['value']], old_mem_val)

      value_colname = names(db_tbl)[col_idx]
      value_rowuid = db_tbl[row_idx, key_col][[1]] # unique key identifier
      db_tbl[row_idx, col_idx] <<- update_value
      DT::replaceData(proxy_db_tbl, db_tbl)


      if(((is.na(old_mem_val) | is.null(old_mem_val) | identical(old_mem_val, '')) &
          (is.na(update_value) | is.null(update_value) | identical(update_value, ''))) |
          identical(old_mem_val, update_value)){
        cat(paste0("\nThe old value is '",old_mem_val,"' and the new value is '",
                    update_value,"'. Not updating the DB.\n"))
        return()
      }

      output$uid_btn <- cdr_render_new_row_ui('... Updating Database ...', id, open_sesame)



      # create append tibble
      to_deltas_tbl <-
        tibble::tibble(uid = value_rowuid,
                       field = value_colname,
                       to = as.character(update_value),
                       from = as.character(old_mem_val),
                       who = user,
                       when = lubridate::now())

      print(to_deltas_tbl)

      # update parent env
      db_tbl_deltas <<- dplyr::bind_rows(db_tbl_deltas, to_deltas_tbl) %>% dplyr::arrange(dplyr::desc(when))
      DT::replaceData(proxy_db_tbl_deltas, db_tbl_deltas)

      # update main table
      cdr_update_primary_tbl(
        db_conn_pool = db_conn_pool,
        db_tbl_name = id,
        update_value = update_value,
        value_colname = value_colname,
        value_rowuid = value_rowuid, # unique key identifier
        value_rowuid_colname = key_col  # column with unique key identifier
      )

      # update deltas db table
      pool::dbAppendTable(conn = db_conn_pool, name = cdr_deltas_tbl_name(id), value = to_deltas_tbl)
      cat('\n\nAppended these fields to deltas table:\n')
      print(to_deltas_tbl)

      output$uid_btn <- cdr_render_new_row_ui('', id, open_sesame)

    })


    ###listener to update db and tables when user creates a new row
    shiny::observeEvent(input$load_uid, {

      cat('\n\n###### NEW ROW #######\n\n')

      input_uid <- stringr::str_trim(input$uid)

      out_text <- cdr_check_unique_id(db_tbl, input_uid, key_col)

      if(is.null(out_text)){

        output$uid_btn <- cdr_render_new_row_ui('... Updating Database ...', id, open_sesame)

        db_tbl <<- dplyr::bind_rows(tibble::tibble({{key_col}} := input_uid), db_tbl)
        DT::replaceData(proxy_db_tbl, db_tbl)

        pool::dbAppendTable(conn = db_conn_pool, name = id, value = dplyr::slice(db_tbl,1))
        cat(glue::glue("\n\nCreated new record in table '{id}':\n\n"))
        print(dplyr::slice(db_tbl,1))

        # create append tibble
        to_deltas_tbl <-
          tibble::tibble(uid = input_uid,
                         field = key_col,
                         to = input_uid,
                         from = "",
                         who = user,
                         when= lubridate::now())

        # update parent env
        db_tbl_deltas <<- dplyr::bind_rows(db_tbl_deltas, to_deltas_tbl) %>% dplyr::arrange(dplyr::desc(when))
        DT::replaceData(proxy_db_tbl_deltas, db_tbl_deltas)

        # update deltas db table
        pool::dbAppendTable(conn = db_conn_pool, name = cdr_deltas_tbl_name(id), value = to_deltas_tbl)
        cat('\n\nAppended these fields to deltas table:\n')
        print(to_deltas_tbl)

      }

      # update the UI
      output$uid_btn <- cdr_render_new_row_ui(out_text, id, open_sesame)

      cat('\n\n### END NEW ROW\n\n')


    })


    # return combined tables to module parent env
    shiny::eventReactive(list(input$db_tbl_cell_edit, input$load_uid),
                         {cdr_combine_prim_delt_tbls(db_tbl, db_tbl_deltas, key_col)},
                         ignoreNULL=FALSE)

  }

  )}

