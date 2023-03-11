# Minimal crudr App Example

# TODO: Can the 'text_out' code be combined into a single listener?
# TODO: Can the deltas table updater be combined into a single function?
# TODO: Add F2 to JS user control
# TODO: (important) alert user to table change, or do an automatic update given changes
# someone else has made a change in the DB since you've been updating, do you want to reload?
# TODO: update whether to export objects
# TODO: (important) update db_tbl name
# TODO: (important) add some DT changes to user-view table


# crudr's goal is to:
# 1) Present data from a database through Shiny for viewing and management.
# 2) Make it easy to control who can update a DB table.
# 3) Keep a record of all changes made to the database table.
# The idea is to make it easy for an organization to centrally contain a single
#  source of information while at the same time allowing distributed control of
#  datasets.

# crudr:
# 1) Works by synching tables between the User, the Host Server, and the Database
# 2) Is built with Shiny modules so that you can manage many tables in the same Shiny app.


# To install crudr, run:
# remotes::install_github('eauleaf/crudr')

# The Example is built from these packages
# install.packages('shiny')
# install.packages('shinydashboard')
# install.packages('tidyverse')
# install.packages('pool')
# install.packages('DT')

# ##############################################
# # Example database setup
# create a connection to a database called 'iris.db'
con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))

# paste('Your Database is saved here:', here::here('iris.db'))
# if the datatable doesn't exist, go make it; otherwise, say it's already there
# Dataset must have a unique identifier column and unique identifiers must be characters in the dataset
IRIS <- tibble::tibble(dplyr::mutate(iris, day = lubridate::today(), time = lubridate::now()))

cdr_create_tbls_in_db(db_conn_pool = con, db_tbl = IRIS)
# what tables are in the 'iris' database?
# paste(pool::dbListTables(con), collapse = ', ')
# if you want to delete the tables later, run these lines. (Does not delete the database; just the tables)
# crudr::cdr_remove_tbl(db_conn_pool = con, db_tbl = 'IRIS')
# crudr::cdr_remove_tbl(db_conn_pool = con, db_tbl = 'iris_DELTAS')
# pool::dbRemoveTable(con,'IRIS')
# pool::dbRemoveTable(con,'IRIS_DELTAS')
# pool::poolClose(con)
##############################################


# Launch a Shiny App
header <- shinydashboard::dashboardHeader(title = 'Tiny CRUDR Example')

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = 'tabs',
    shinydashboard::menuItem(
      text = "Iris Data",
      startExpanded = TRUE,
      shinydashboard::menuSubItem("Administrator Table", tabName = "datr_editable", icon = shiny::icon('edit')),
      shinydashboard::menuSubItem("Change Log", tabName = "datr_change_log"),
      shinydashboard::menuSubItem("Iris End User View", tabName = "datr_end_usr")
    )))

body <- shinydashboard::dashboardBody(
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "datr_editable", htmltools::div(crudr::cdr_admin_key_ui('IRIS'), crudr::cdr_admin_tbl_ui('IRIS', 'db_tbl'))),
    shinydashboard::tabItem(tabName = "datr_change_log", crudr::cdr_admin_tbl_ui('IRIS', 'chg_log_tbl')),
    shinydashboard::tabItem(tabName = "datr_end_usr", DT::DTOutput('iris_db_data'))
  ))

ui <- shinydashboard::dashboardPage(header, sidebar, body)




server <- function(input, output, session){

  # The Function: crudr::cdr_manage_db_tbls() returns 1 table directly and 2 tables invisibly:
  #   1) The Output table: is a view-only join of your primary table and the most recent change for each row
  #       In this example, the output table is a tibble and is called simply by "iris_r_tbl()".
  #       You output your database table by
  #       sing this function also sends in initial parameters to prepare the module.
  #   2) The Editable data table: is the primary database table that you're managing. In this example, it's called "IRIS". You obtain the table by calling crudr::cdr_admin_tbl_ui('IRIS', 'db_tbl')
  #   3) The Changes Log table: is your record of changes to the Editable table. You get the log table through crudr::cdr_admin_tbl_ui('IRIS', 'chg_log_tbl')

  iris_r_tbl <- crudr::cdr_manage_db_tbls(
    id = 'IRIS',
    key_col = 'UID',
    db_conn_pool = con,
    session = session,
    add_row_permission = T,
    del_row_permission = T,
    cell_edit_permission = T,
    lock_fields = c('Species','Petal.Width')
  )

  output$iris_db_data <- DT::renderDT(
    DT::formatDate(table   = DT::datatable(iris_r_tbl()),
                   columns = 'WHEN_EDITED_LAST',
                   method  = 'toLocaleString')
  )

}


shiny::shinyApp(ui, server)
