#' A drop-in replacement for [DBI::Id()], but with ordered output
#'
#' @param ... table spec parameters, i.e. cluster, catalog, schema, table
#'
#' @return equivalent [DBI::Id()] object
#' @export
#'
#' @examples
#' cdr_id(table = 'my_table', schema = 'a_schema', some_new_spec = 'in_case_you_need_it')
#' cdr_id(table = 'table_last', schema = 'schema_3rd', cluster = 'clus_1st', catalog = 'cat_2nd')
#'
cdr_id <- function (...) {

  components <- list(cluster = NULL, catalog = NULL, schema = NULL, table = NULL) |>
    utils::modifyList(rlang::list2(...)) |> unlist()


  if (is.null(names(components)) || any(names(components) == "")) {
    stop("All arguments to Id() must be named.", call. = FALSE)
  }

  methods::new(Class = structure("Id", package = "DBI"), name = components)

}


