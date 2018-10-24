context("trade_weights() basics")
test_that("trade_weights() handles errors correctly", {
    expect_error(trade_weights(1:26, LETTERS))
})


context("trade_weights() final nav within 1 bp excel calculation: daily rebalance")
etfs_5days <- etfdailyclose[etfdailyclose$date <= as.Date('2017-10-30'),]
etfs_5days_cash <- dplyr::bind_rows(create_cash_assets(fedfunds), etfs_5days)

daily_long100 <- dplyr::pull(dplyr::filter(
    trade_weights(etfs_5days_cash, dplyr::mutate(etfdailyclose, w = 1/3)),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Daily Rebalance & 100% Long Exposure",
          expect_equal(daily_long100 / 1003182.5310446 - 1,
                        0, tolerance = 0.0001, scale = 1))

daily_long050 <- dplyr::pull(dplyr::filter(
    trade_weights(etfs_5days_cash, dplyr::mutate(etfdailyclose, w = 1/6)),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Daily Rebalance & 50% Long Exposure",
          expect_equal(daily_long050 / 1001675.91084044 - 1,
                       0, tolerance = 0.0001, scale = 1))

daily_long150 <- dplyr::pull(dplyr::filter(
    trade_weights(etfs_5days_cash, dplyr::mutate(etfdailyclose, w = 1/2)),
    dplyr::row_number() == dplyr::n()),
    nav)


context("trade_weights() final nav within 1 bp excel calculation: weekly rebalance")
etfs_7days <- etfdailyclose[etfdailyclose$date <= as.Date('2017-10-31'),]
etfs_7days_cash <- dplyr::bind_rows(create_cash_assets(fedfunds), etfs_7days)
df_price <- etfs_7days_cash

week_long100 <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/3), 'week'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Weekly Rebalance & 100% Long Exposure",
          expect_equal(week_long100 / 1004273.34906276 - 1,
                       0, tolerance = 0.0001, scale = 1))

week_long050 <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/6), 'week'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Weekly Rebalance & 50% Long Exposure",
          expect_equal(week_long050 / 1002217.74233325 - 1,
                       0, tolerance = 0.0001, scale = 1))

week_long150 <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/2), 'week'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Weekly Rebalance & 150% Long Exposure",
          expect_equal(week_long150 / 1006249.14558189 - 1,
                       0, tolerance = 0.0001, scale = 1))


context("trade_weights() final nav within 1 bp excel calculation: monthly rebalance")
etfs_9days <- etfdailyclose[etfdailyclose$date <= as.Date('2017-11-02'),]
etfs_9days_cash <- dplyr::bind_rows(create_cash_assets(fedfunds), etfs_9days)
df_price <- etfs_9days_cash

df_test <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/3), 'month'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Monthly Rebalance & 100% Long Exposure",
          expect_equal(df_test / 1009969.6748769 - 1,
                       0, tolerance = 0.0001, scale = 1))

df_test <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/6), 'month'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Month Rebalance & 50% Long Exposure",
          expect_equal(df_test / 1005087.90897054 - 1,
                       0, tolerance = 0.0001, scale = 1))

df_test <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/2), 'month'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Monthly Rebalance & 150% Long Exposure",
          expect_equal(df_test / 1014748.29119619 - 1,
                       0, tolerance = 0.0001, scale = 1))


context("trade_weights() final nav within 1 bp excel calculation: qtr rebalance")
etfs_50days <- etfdailyclose[etfdailyclose$date <= as.Date('2018-01-03'),]
etfs_50days_cash <- dplyr::bind_rows(create_cash_assets(fedfunds), etfs_50days)
df_price <- etfs_50days_cash

df_test <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/6), 'qtr'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Quarterly Rebalance & 50% Long Exposure",
          expect_equal(df_test / 1036400.81989253 - 1,
                       0, tolerance = 0.0001, scale = 1))

df_test <- dplyr::pull(dplyr::filter(
    trade_weights(df_price, dplyr::mutate(etfdailyclose, w = 1/2), 'qtr'),
    dplyr::row_number() == dplyr::n()),
    nav)
test_that("Final Nav: Quarterly Rebalance & 150% Long Exposure",
          expect_equal(df_test / 1106057.80874977 - 1,
                       0, tolerance = 0.0001, scale = 1))
