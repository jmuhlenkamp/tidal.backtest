context("Test trade_weights()")

test_that("trade_weights() handles errors correctly", {
    expect_error(trade_weights(1:26, LETTERS))
})

test_that("trade_weights() final nav within 1 bp excel calculation", {
    expect_equal(
        dplyr::pull(
            dplyr::filter(
                trade_weights(etfdailyclose, dplyr::mutate(etfdailyclose, w = 1/3)),
                dplyr::row_number() == dplyr::n()),
            nav
            ) / 977866.109536611 - 1,
        0,
        tolerance = 0.0001,
        scale = 1)
})
