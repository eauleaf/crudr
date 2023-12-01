#' A drop-in replacement for [DBI::Id()], but with ordered output
#'
#' @param ... other table index parameters
#' @param table table name as string
#' @param schema schema name as string
#' @param catalog catalog name as string
#' @param cluster cluster name as string
#'
#' @return equivalent [DBI::Id()] object
#' @export
#'
#' @examples
#' cdr_id(table = 'my_table', schema = 'a_schema', some_new_spec = 'in_case_you_need_it')
#' cdr_id(table = 'table_last', schema = 'schema_3rd', cluster = 'clus_1st', catalog = 'cat_2nd')
#'
cdr_id <- function(..., table = NULL, schema = NULL, catalog = NULL, cluster = NULL){

  components <- c(cluster = cluster, catalog = catalog, schema = schema, ..., table = table)

  if (is.null(names(components)) || any(names(components) == "")) {
    stop("All arguments to Id() must be named.", call. = FALSE)
  }

  methods::new(Class = structure("Id", package = "DBI"), name = components)

}


