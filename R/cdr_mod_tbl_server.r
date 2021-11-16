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
#' @param open_sesame T or F to make editable the primary table from the module
#' @param db_conn_pool db connection from package 'pool'
#' @param session current shiny session
#'
#' @return returns DT reactive tables to the shiny ui environment
#' @export
#'
#' @examples
#' \dontrun{
#' # ensure db is already setup with cdr_create_new_db_tbls()
#' db_conn_pool <- easydbconn::get_db_con(friendly_db_name)
#' server <- function(input, output, session){
#'              cdr_mod_tbl_server('primary_tbl_name', 'key_col_name', db_conn_pool)}
#' shinyApp(ui, server)
#' }

cdr_mod_tbl_server <- function(id, key_col, db_conn_pool, session, open_sesame = F){
  shiny::moduleServer(id, module = function(input, output, session){

    # new observation row edit function
    cdr_render_new_row_ui <- function(id, notes_txt = ''){
      if(open_sesame){
        output$uid_btn <- shiny::renderUI({
          ns <- shiny::NS(id)
          shiny::tags$span(style="display: inline-flex; align-items: center; gap: 0px 15px; font-size: 10px;",
                           shiny::textInput(ns('uid'), '', placeholder = 'Enter unique ID', width = '180px'),
                           shiny::actionButton(ns('load_uid'), label = "Create Row", text = 'Create Row'),
                           shiny::span(notes_txt, style = "color:red; font-size: 130%;"))})
      } else {
        output$uid_btn <- shiny::renderUI(' ')
      }
    }

    cdr_render_new_row_ui(id)



    # key use and edit highlighting
    js <- c(
      "table.on('key', function(e, datatable, key, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  if(key == 13 && targetName == 'body'){",
      "    $(cell.node()).trigger('dblclick.dt');",
      "  }",
      "});",
      "table.on('keydown', function(e){",
      "  if(e.target.localName == 'input' && [9,13,37,38,39,40].indexOf(e.keyCode) > -1){",
      "    $(e.target).trigger('blur');",
      "  }",
      "});",
      "table.on('key-focus', function(e, datatable, cell, originalEvent){",
      "  var targetName = originalEvent.target.localName;",
      "  var type = originalEvent.type;",
      "  if(type == 'keydown' && targetName == 'input'){",
      "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
      "      $(cell.node()).trigger('dblclick.dt');",
      "    }",
      "  }",
      "});"
    )

    ### primary tbl
    db_tbl <- dplyr::tbl(db_conn_pool, id) %>% dplyr::collect() %>% dplyr::relocate(dplyr::all_of(key_col))
    proxy_db_tbl = DT::dataTableProxy('db_tbl')
    output$db_tbl <- DT::renderDT(
      DT::datatable(db_tbl,
                    options = list(scrollX = TRUE,  keys = TRUE),
                    callback = DT::JS(js),
                    extensions = "KeyTable",
                    selection = 'none',
                    editable = if(open_sesame){list(target = "cell", disable = list(columns = c(0,1)))}
      ))


    ### deltas tbl
    db_tbl_deltas <- dplyr::tbl(db_conn_pool, cdr_deltas_tbl_name(id)) %>% dplyr::collect() %>% dplyr::arrange(dplyr::desc(when))
    if(class(db_tbl_deltas$when) == 'numeric'){ # for SQLite
      db_tbl_deltas <- dplyr::mutate(db_tbl_deltas, when = as.POSIXct(when,origin = "1970-01-01"))}
    proxy_db_tbl_deltas = DT::dataTableProxy('db_tbl_deltas')
    output$db_tbl_deltas <- DT::renderDT(
      DT::datatable(db_tbl_deltas,
                    selection = 'none') %>%
        DT::formatDate('when', method = 'toLocaleString')
    )


    ###listener to update db and tables when user updates the primary table
    shiny::observeEvent(input$db_tbl_cell_edit, {

      to_update <- input$db_tbl_cell_edit
      row_idx <- to_update[['row']]
      col_idx <- to_update[['col']]
      old_mem_val <- db_tbl[row_idx, col_idx][[1]]
      update_value = DT::coerceValue(to_update[['value']], old_mem_val)

      value_colname = names(db_tbl)[col_idx]
      value_rowuid = db_tbl[row_idx, key_col][[1]] # unique key identifier
      db_tbl[row_idx, col_idx] <<- update_value
      DT::replaceData(proxy_db_tbl, db_tbl, resetPaging = FALSE)

      old_db_val <- cdr_update_primary_tbl(
        db_conn_pool = db_conn_pool,
        db_tbl_name = id,
        update_value = update_value,
        value_colname = value_colname,
        value_rowuid = value_rowuid, # unique key identifier
        value_rowuid_colname = key_col  # column with unique key identifier
      )

      print(paste0("overwrote: '",old_db_val,"' with '",update_value,"' in '",id,"' database"))

      changes <- cdr_update_deltas_tbl(
        db_conn_pool = db_conn_pool,
        db_tbl_name = cdr_deltas_tbl_name(id),
        old_value = old_db_val,
        update_value = update_value,
        value_rowuid = as.character(value_rowuid),
        value_colname = value_colname,
        who = ifelse(is.null(session$user), Sys.info()[['user']], session$user)
      )

      # update parent env
      db_tbl_deltas <<- dplyr::bind_rows(db_tbl_deltas, changes) %>% dplyr::arrange(dplyr::desc(when))
      DT::replaceData(proxy_db_tbl_deltas, db_tbl_deltas)
      print(paste0("updated: '",id,"' deltas database with: ", changes))

    })


    ###listener to update db and tables when user creates a new row
    shiny::observeEvent(input$load_uid, {

      out_text <- cdr_check_unique_id(db_conn_pool, id, input$uid, key_col)

      if(is.null(out_text)){
        new_row <- cdr_create_new_db_row(db_conn_pool, id, input$uid, (!!key_col))
        db_tbl <<- dplyr::bind_rows(new_row, db_tbl)
        DT::replaceData(proxy_db_tbl, db_tbl)

        changes <- cdr_update_deltas_tbl(
          db_conn_pool = db_conn_pool,
          db_tbl_name = cdr_deltas_tbl_name(id),
          old_value = "",
          update_value = input$uid,
          value_rowuid = input$uid,
          value_colname = key_col,
          who = ifelse(is.null(session$user), Sys.info()[['user']], session$user)
        )

        # update parent env
        db_tbl_deltas <<- dplyr::bind_rows(db_tbl_deltas, changes) %>% dplyr::arrange(dplyr::desc(when))
        DT::replaceData(proxy_db_tbl_deltas, db_tbl_deltas)
        print(paste0("updated: '",id,"' deltas database with: ", changes))

      }
      cdr_render_new_row_ui(id, out_text)
    })


    # return combined tables to module parent env
    shiny::eventReactive(list(input$db_tbl_cell_edit, input$load_uid),
                         {cdr_combine_prim_delt_tbls(db_tbl, db_tbl_deltas, key_col)},
                         ignoreNULL=FALSE)

  }

  )}

