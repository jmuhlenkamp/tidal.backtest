context("Test Utility Functions")

test_that("validate_df_columns() handles errors correctly", {
    expect_error(validate_df_columns(iris,
                                     c("Sepal.Length",
                                       "Sepal.Width",
                                       "Column.Not.In.Iris")))
})
