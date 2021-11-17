# crudr Minimal App Example

library(shiny)
library(shinydashboard)
library(tidyverse)
library(pool)
library(DT)
library(crudr)

##############################################
# Example database setup
data("iris")
con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
# dataset must have a unique identifier column and unique identifiers must be chars, not numeric
iris <- dplyr::mutate(iris, unique_id = paste0('uid_',dplyr::row_number()))
cdr_create_new_db_tbls(db_conn_pool = con, db_tbl = iris)
locn <- print(here::here('iris.db')) # location of database
dbs <- print(paste(dbListTables(con), collapse = ', ')) # tables in database
##############################################


header <- dashboardHeader(title = 'Minimal Example',
                          dropdownMenu(messageItem('', paste('DB-locn:',locn)),
                                       messageItem('', paste('DB-tbls:',dbs))
                                       )
                          )

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'tabs',
    menuItem("Iris",
             menuSubItem("Iris Output Table", tabName = "datr_joined"),
             menuSubItem("Iris Managed Table (editable)", tabName = "datr_editable"),
             menuSubItem("Iris Change Log", tabName = "datr_change_log")
    )))

body <- dashboardBody(
  tabItems(
    tabItem("datr_joined", DT::DTOutput('iris_db_data')),
    tabItem("datr_editable", div(cdr_mod_btn_ui('iris'), cdr_mod_tbl_ui('iris', 'db_tbl'))),
    tabItem("datr_change_log", cdr_mod_tbl_ui('iris', 'db_tbl_deltas'))
  ))

ui <- dashboardPage(header, sidebar, body)




server <- function(input, output, session){

  # set up user change permission
  open_sesame <- T

  # function: mod_tbl_server() returns 3 tables...
  # namespace tables: output$db_tbl and output$db_tbl_deltas, and the tables joined in iris_tbl_out()
  iris_tbl_out <- cdr_mod_tbl_server('iris', 'unique_id', con, session, open_sesame)
  output$iris_db_data <- renderDT(
    formatDate(table = DT::datatable(iris_tbl_out()),
               columns = 'when',
               method = 'toLocaleString')
  )

}



shinyApp(ui, server)
