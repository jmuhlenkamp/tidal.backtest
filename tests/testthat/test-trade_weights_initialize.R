context("Test trade_weights_initialize()")

test_that("trade_weights_initialize() handles errors correctly", {
    expect_error(trade_weights_initialize(1:26, LETTERS))
})

test_that("trade_weights_initialize() return list does not contain NAs", {
    list_initialized <- trade_weights_initialize(etfdailyclose,
                                                 dplyr::mutate(etfdailyclose, w = 1/3),
                                                 FALSE, FALSE)
    list_nas <- lapply(list_initialized, function(x) any(is.na(x)))
    expect_identical(list_nas, list(dates = FALSE, dt = FALSE, dt_fund = FALSE))
})
