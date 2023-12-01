test_that("cdr_id() makes ordered DBI Id objects.", {
  expect_equal(
    slot(cdr_id(table = 'my_table', schema = 'a_schema', some_new_spec = 'in_case_you_need_it'), 'name'),
    c(schema = 'a_schema', some_new_spec = 'in_case_you_need_it', table = 'my_table')
  )
  expect_equal(
    slot(cdr_id(table = 'table_last', schema = 'schema_3rd', cluster = 'clus_1st', catalog = 'cat_2nd'), 'name'),
    c(cluster = 'clus_1st', catalog = 'cat_2nd', schema = 'schema_3rd', table = 'table_last')
  )
})




