context("Test trade_weights()")
etfs_5days <- etfdailyclose[etfdailyclose$date <= as.Date('2017-10-30'),]
etfs_5days_cash <- dplyr::bind_rows(create_cash_assets(fedfunds), etfs_5days)

readr::write_csv(etfs_5days_cash, '~/Desktop/df.csv')

test_that("trade_weights() handles errors correctly", {
    expect_error(trade_weights(1:26, LETTERS))
})

test_that(paste("trade_weights() final nav within 1 bp excel calculation:",
                "100% Long Exposure & daily rebalance"), {
    expect_equal(
        dplyr::pull(
            dplyr::filter(
                trade_weights(etfs_5days, dplyr::mutate(etfdailyclose, w = 1/3)),
                dplyr::row_number() == dplyr::n()),
            nav
            ) / 1003182.5310446 - 1,
        0,
        tolerance = 0.0001,
        scale = 1)
})

test_that(paste("trade_weights() final nav within 1 bp excel calculation:",
                "50% Long Exposure & daily rebalance"), {
        expect_equal(
            dplyr::pull(
                dplyr::filter(
                    trade_weights(etfs_5days_cash, dplyr::mutate(etfdailyclose, w = 1/6)),
                    dplyr::row_number() == dplyr::n()),
                nav
            ) / 1001675.91084044 - 1,
            0,
            tolerance = 0.0001,
            scale = 1)
})

test_that(paste("trade_weights() final nav within 1 bp excel calculation:",
                "150% Long Exposure & daily rebalance"), {
    expect_equal(
        dplyr::pull(
            dplyr::filter(
                trade_weights(etfs_5days_cash, dplyr::mutate(etfdailyclose, w = 1/2)),
                dplyr::row_number() == dplyr::n()),
            nav
        ) / 1004631.17245305 - 1,
        0,
        tolerance = 0.0001,
        scale = 1)
})
