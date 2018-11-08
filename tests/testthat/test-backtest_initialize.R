context("Test backtest_initialize()")

test_that("backtest_initialize() handles errors correctly", {
    expect_error(backtest_initialize(df_prices = 1:26, df_weights = LETTERS, name = ".test"))
})

test_that("backtest_initialize() return list does not contain NAs", {
    list_initialized <- backtest_initialize(dailycloseetf,
                                            dplyr::mutate(dailycloseetf, w = 1/3),
                                            FALSE, FALSE, ".test")
    list_nas <- lapply(list_initialized, function(x) any(is.na(x)))
    expect_identical(list_nas, list(dates = FALSE, dt = FALSE, dt_fund = FALSE, name = FALSE))
})
