context("Test backtest_initialize()")

test_that("backtest_initialize() handles errors correctly", {
    expect_error(backtest_initialize(1:26, LETTERS))
})

test_that("backtest_initialize() return list does not contain NAs", {
    list_initialized <- backtest_initialize(dailycloseetf,
                                                 dplyr::mutate(dailycloseetf, w = 1/3),
                                                 FALSE, FALSE)
    list_nas <- lapply(list_initialized, function(x) any(is.na(x)))
    expect_identical(list_nas, list(dates = FALSE, dt = FALSE, dt_fund = FALSE))
})
