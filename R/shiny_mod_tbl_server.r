#' shiny server module gets database data from db specified by db_conn_pool connection and the db table name specified in 'id',
#'  server module manages and syncs changes between the UI DT, in-server-memory tbl, and backend db
#'
#' @param id primary table name - namespace ID corresponding to the 'primary_tbl_name' in the database
#' @param key_col name of the unique ID column in the db table (table must have a unique ID column with unique IDs)
#' @param db_conn_pool db connection from package 'pool'
#'
#' @return returns DT reactive tables to the shiny ui environment
#' @export
#'
#' @examples
#' \dontrun{
#' db_conn_pool <- easydbconn::get_db_con(friendly_db_name) # ensure db is already setup with crudr::create_new_db_tbls()
#' server <- function(input, output, session){mod_tbl_server('primary_tbl_name', 'key_col_name', db_conn_pool)}
#' shinyApp(ui, server)
#' }

mod_tbl_server <- function(id, key_col, db_conn_pool, who){
  moduleServer(id,
               module = function(input, output, session){

                 ### initial collection of db tables
                 # get primary table; create DT proxy
                 db_tbl <- tbl(db_conn_pool, id) %>% collect() %>% relocate(all_of(key_col))
                 proxy = DT::dataTableProxy('db_tbl')
                 output$db_tbl <- renderDT(db_tbl,
                                           options = list(scrollX = TRUE,  keys = TRUE),
                                           callback = JS(js),
                                           extensions = "KeyTable",
                                           editable = list(target = "cell", disable = list(columns = c(0,1))))

                 # Javascript function to make UI edits from keyboard. JS from:
                 # https://stackoverflow.com/questions/54907273/use-tab-to-edit-next-cell-on-dt-table/54910411
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


                 # get deltas tbl; create DT proxy
                 db_tbl_deltas <- tbl(db_conn_pool, deltas_tbl_name(id)) %>% collect() %>% arrange(desc(when))
                 proxy = DT::dataTableProxy('db_tbl_deltas')
                 output$db_tbl_deltas <- renderDT(db_tbl_deltas)


                 ###update db and tables when user updates the primary table
                 observeEvent(input$db_tbl_cell_edit, {

                   to_update <- input$db_tbl_cell_edit
                   row_idx <- to_update[['row']]
                   col_idx <- to_update[['col']]
                   old_mem_val <- db_tbl[row_idx, col_idx][[1]]
                   update_value = DT::coerceValue(to_update[['value']], old_mem_val)

                   value_colname = names(db_tbl)[col_idx]
                   value_rowuid = db_tbl[row_idx, key_col][[1]] # unique key identifier
                   db_tbl[row_idx, col_idx] <<- update_value # do last

                   # update primary DT proxy and db
                   replaceData(proxy, db_tbl, resetPaging = FALSE)

                   old_db_val <- crudr::update_primary_tbl(db_conn_pool = db_conn_pool,
                                                           db_tbl_name = id,
                                                           update_value = update_value,
                                                           value_colname = value_colname,
                                                           value_rowuid = value_rowuid, # unique key identifier
                                                           value_rowuid_colname = key_col  # column with unique key identifier
                   )

                   print(paste0("updated: '", old_db_val, "' in '",id ,"' database"))

                   changes <- crudr::update_deltas_tbl(db_conn_pool = db_conn_pool,
                                                       db_tbl_name = crudr::deltas_tbl_name(id),
                                                       old_value = old_db_val,
                                                       update_value = update_value,
                                                       value_rowuid = as.character(value_rowuid),
                                                       value_colname = value_colname,
                                                       who = who
                   )

                   # update parent env
                   db_tbl_deltas <<- bind_rows(db_tbl_deltas, changes) %>% arrange(desc(when))
                   replaceData(proxy, db_tbl_deltas)
                   print(paste0("updated: '",id,"' deltas database with: "))
                   print(changes)

                 })

                 # return combined tables to module parent env
                 eventReactive(input$db_tbl_cell_edit,
                               {combine_prim_delt_tbls(db_tbl, db_tbl_deltas, key_col)},
                               ignoreNULL=FALSE)

             }

  )}

