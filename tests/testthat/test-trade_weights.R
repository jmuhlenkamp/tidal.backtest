context("Test trade_weights()")

test_that("trade_weights() handles errors correctly", {
    expect_error(trade_weights(1:26, LETTERS))
})
