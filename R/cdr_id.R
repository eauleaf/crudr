#' A drop-in replacement for [DBI::Id()], but with ordered output
#'
#' @param ... DB table index parameters, i.e. strings with with labels
#' table, schema, catalog, cluster, database
#'
#'
#' @return equivalent [DBI::Id()] object
#' @export
#'
#' @examples
#' cdr_id(table = 'my_table', schema = 'a_schema', some_new_spec = 'in_case_you_need_it')
#' cdr_id(table = 'table_last', schema = 'schema_3rd', cluster = 'clus_1st', catalog = 'cat_2nd')
#'
cdr_id <- function(...){


  components <- orderIdParams(...)

  methods::new(Class = structure("Id", package = "DBI"), name = components)

}


# crudr:::orderIdParams('green', talb = 'blue', table = 'blue')
# crudr:::orderIdParams('green', talb = 'blue', table = 'blue')
# crudr:::orderIdParams(table = 'table', 'schema')
# crudr:::orderIdParams(list(table = 'table', 'schema'), table = NULL, catalog = 'cat')
# crudr:::orderIdParams()
orderIdParams <- function(...,
                          database = NULL,
                          cluster = NULL,
                          catalog = NULL,
                          schema = NULL,
                          table = NULL){

  out <- c(database = database,
    cluster = cluster,
    catalog = catalog,
    schema = schema,
    unlist(list(...)),
    table = table
  )

  preceeding_locns <- c(
    which(names(out) == 'database'),
    which(names(out) == 'cluster'),
    which(names(out) == 'catalog'),
    which(names(out) == 'schema')
  )

  table_locn <- c(which(names(out) == 'table'))

  other_locns <- setdiff(1:length(out), c(preceeding_locns, table_locn))

  out[c(preceeding_locns,other_locns,table_locn)]

}




