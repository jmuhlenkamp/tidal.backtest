context("Test fedfunds Functions")

test_that("create_cash_assets() runs within an error", {
    expect_identical(nrow(create_cash_assets(fedfunds)),
                     as.integer(730))
})

test_that("convert_fedfunds_broker() does not return borrowing > 0 or earning < 0", {
    expect_identical(min(convert_fedfunds_broker(fedfunds, "_CASH_LONG_", 1, -9.00)$rate) >= 0,
                     TRUE)
    expect_identical(min(convert_fedfunds_broker(fedfunds, "_CASH_SHORT_", -1, +9.00)$rate) <= 0,
                     TRUE)
})

test_that("convert_*() final price within 1 bp excel calculation", {
    expect_equal(tail(convert_g_price(convert_rate_g(fedfunds)), 1)$price /
                     1.01712548616142 - 1,
        0,
        tolerance = 0.0001,
        scale = 1)
})
