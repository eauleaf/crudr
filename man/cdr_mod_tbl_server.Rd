% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdr_mod_tbl_server.r
\name{cdr_mod_tbl_server}
\alias{cdr_mod_tbl_server}
\title{Server module to present and control db tables}
\usage{
cdr_mod_tbl_server(id, key_col, db_conn_pool, session, open_sesame = F)
}
\arguments{
\item{id}{primary table name - namespace ID corresponding to the
'primary_tbl_name' in the database}

\item{key_col}{name of the unique ID column in the db table (table must have
a unique ID column with unique IDs)}

\item{db_conn_pool}{db connection from package 'pool'}

\item{session}{current shiny session}

\item{open_sesame}{T or F to make editable the primary table from the module}
}
\value{
returns DT reactive tables to the shiny ui environment
}
\description{
Shiny server module gets database tables from db specified by pool connection
and the db table name specified in 'id', server module manages and syncs
changes between the UI DT, in-server-memory tbl, and backend db, for the
primary as well as the corresponding deltas table (and joined table if it's
presented in the UI)
}
\examples{
\dontrun{
data("iris")
con <- pool::dbPool(DBI::dbConnect(RSQLite::SQLite(), 'iris.db'))
iris <- dplyr::mutate(iris,
           unique_id = paste0('uid_',stringr::str_pad(dplyr::row_number(),3,pad=0)))
cdr_create_new_db_tbls(db_conn_pool = con, db_tbl = iris)
server <- function(input, output, session){
             iris_tbl_out <- cdr_mod_tbl_server('iris', 'unique_id', con, session, open_sesame = T)
             output$iris <- DT::renderDT(iris_tbl_out())
}
ui <- fluidPage(DTOutput('iris'))
shinyApp(ui,server)
}
}
