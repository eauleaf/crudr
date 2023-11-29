test_that("unique Ids populate", {
  expect_equal(
    cdr_make_unique_ids(utils::head(iris,11)),
    structure(
      list(
        UID = c(
          "ID-01", "ID-02", "ID-03", "ID-04", "ID-05", "ID-06", "ID-07", "ID-08", "ID-09",
          "ID-10", "ID-11"
        ),
        Sepal.Length = c(
          5.1, 4.9, 4.7, 4.6, 5, 5.4, 4.6, 5, 4.4, 4.9, 5.4
        ),
        Sepal.Width = c(
          3.5, 3, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7
        ),
        Petal.Length = c(
          1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5, 1.5
        ),
        Petal.Width = c(
          0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1, 0.2
        ),
        Species = structure(
          c(
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
          ),
          levels = c(
            "setosa", "versicolor", "virginica"
          ),
          class = "factor"
        )
      ),
      row.names = c(
        NA, -11L
      ),
      class = c(
        "tbl_df", "tbl", "data.frame"
      )
    )
  )

})

