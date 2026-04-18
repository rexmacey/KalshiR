# tests/testthat/test-historical.R
# Tests for R/historical.R
#
# Unit tests run offline.
# Integration tests are skipped unless KALSHI_DEMO=true.

library(testthat)

# ---------------------------------------------------------------------------
# Unit tests — .parse_candlesticks()
# ---------------------------------------------------------------------------

test_that(".parse_candlesticks() returns empty tibble for zero rows", {
  result <- kalshiR:::.parse_candlesticks(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("end_period_ts", "open_price", "high_price",
                         "low_price", "close_price", "volume", "open_interest"))
})

test_that(".parse_candlesticks() parses nested price object", {
  fake_rows <- list(
    list(
      end_period_ts = "2024-01-15T01:00:00Z",
      price = list(open = 55L, high = 65L, low = 50L, close = 60L),
      volume        = 200L,
      open_interest = 1500L
    ),
    list(
      end_period_ts = "2024-01-15T02:00:00Z",
      price = list(open = 60L, high = 70L, low = 58L, close = 68L),
      volume        = 350L,
      open_interest = 1600L
    )
  )

  result <- kalshiR:::.parse_candlesticks(fake_rows)

  expect_equal(nrow(result), 2L)
  expect_equal(result$open_price,  c(0.55, 0.60), tolerance = 1e-6)
  expect_equal(result$high_price,  c(0.65, 0.70), tolerance = 1e-6)
  expect_equal(result$low_price,   c(0.50, 0.58), tolerance = 1e-6)
  expect_equal(result$close_price, c(0.60, 0.68), tolerance = 1e-6)
  expect_equal(result$volume,        c(200L, 350L))
  expect_s3_class(result$end_period_ts, "POSIXct")
})

test_that(".parse_candlesticks() handles missing price fields as NA", {
  fake_rows <- list(
    list(
      end_period_ts = "2024-01-15T01:00:00Z",
      price         = list(),   # empty price object
      volume        = 0L
    )
  )

  result <- kalshiR:::.parse_candlesticks(fake_rows)
  expect_equal(nrow(result), 1L)
  # All price fields should be NA (converted via price_to_prob(NA))
  expect_true(is.na(result$open_price))
  expect_true(is.na(result$close_price))
})


# ---------------------------------------------------------------------------
# Unit tests — .parse_historical_markets()
# ---------------------------------------------------------------------------

test_that(".parse_historical_markets() returns empty tibble for zero rows", {
  result <- kalshiR:::.parse_historical_markets(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_true("ticker" %in% names(result))
})

test_that(".parse_historical_markets() parses a full record", {
  fake_rows <- list(
    list(
      ticker          = "INXD-23DEC29-T4000",
      event_ticker    = "INXD-23DEC29",
      series_ticker   = "INXD",
      title           = "Will the S&P 500 be above 4000?",
      status          = "settled",
      result          = "yes",
      yes_bid         = 95L,
      no_bid          = 5L,
      open_time       = "2023-12-01T14:00:00Z",
      close_time      = "2023-12-29T21:00:00Z",
      expiration_time = "2023-12-29T22:00:00Z",
      volume          = 50000L,
      open_interest   = 0L,
      liquidity       = 0L
    )
  )

  result <- kalshiR:::.parse_historical_markets(fake_rows)

  expect_equal(nrow(result), 1L)
  expect_equal(result$ticker,       "INXD-23DEC29-T4000")
  expect_equal(result$status,       "settled")
  expect_equal(result$result,       "yes")
  expect_equal(result$yes_price,    0.95, tolerance = 1e-6)
  expect_s3_class(result$open_time, "POSIXct")
})


# ---------------------------------------------------------------------------
# Unit tests — .parse_historical_trades()
# ---------------------------------------------------------------------------

test_that(".parse_historical_trades() returns empty tibble for zero rows", {
  result <- kalshiR:::.parse_historical_trades(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("trade_id", "ticker", "side", "count",
                         "yes_price", "no_price", "created_time", "taker_side"))
})

test_that(".parse_historical_trades() converts prices and timestamps", {
  fake_rows <- list(
    list(
      trade_id     = "trade-001",
      ticker       = "FAKE-MKT",
      side         = "yes",
      count        = 10L,
      yes_price    = 72L,
      no_price     = 28L,
      created_time = "2023-06-15T09:45:00Z",
      taker_side   = "yes"
    )
  )

  result <- kalshiR:::.parse_historical_trades(fake_rows)

  expect_equal(result$yes_price, 0.72, tolerance = 1e-6)
  expect_equal(result$no_price,  0.28, tolerance = 1e-6)
  expect_s3_class(result$created_time, "POSIXct")
})


# ---------------------------------------------------------------------------
# Unit tests — get_historical_candlesticks() argument validation
# ---------------------------------------------------------------------------

test_that("get_historical_candlesticks() errors without required args", {
  #mock_creds <- kalshiR:::mock_creds  # from helper.R

  expect_error(
    get_historical_candlesticks(
      ticker   = "INXD-23DEC29-T4000",
      start_ts = as.POSIXct("2023-12-01", tz = "UTC"),
      end_ts   = as.POSIXct("2023-12-29", tz = "UTC"),
      creds    = mock_creds
    ),
    regexp = "series_ticker"
  )

  expect_error(
    get_historical_candlesticks(
      series_ticker = "INXD",
      start_ts = as.POSIXct("2023-12-01", tz = "UTC"),
      end_ts   = as.POSIXct("2023-12-29", tz = "UTC"),
      creds    = mock_creds
    ),
    regexp = "ticker"
  )
})

test_that("get_historical_trades() errors without ticker", {

  expect_error(
    get_historical_trades(creds = mock_creds),
    regexp = "ticker"
  )
})


# ---------------------------------------------------------------------------
# Integration tests — require demo credentials
# ---------------------------------------------------------------------------

test_that("get_historical_cutoff() returns a one-row tibble on demo", {
  skip_if_not_demo()

  cutoff <- get_historical_cutoff(creds = live_creds())

  expect_s3_class(cutoff, "tbl_df")
  expect_equal(nrow(cutoff), 1L)
  expect_named(cutoff, "cutoff_time")
  expect_s3_class(cutoff$cutoff_time, "POSIXct")
  # Cutoff should be in the past
  expect_true(cutoff$cutoff_time < Sys.time())
})

test_that("get_historical_markets() returns a tibble on demo", {
  skip_if_not_demo()

  hist <- get_historical_markets(
    status = "settled",
    limit  = 5L,
    creds  = live_creds()
  )

  expect_s3_class(hist, "tbl_df")
  expect_true(all(c("ticker", "status", "close_time") %in% names(hist)))

  if (nrow(hist) > 0L) {
    expect_true(all(hist$status == "settled"))
    expect_s3_class(hist$close_time, "POSIXct")
  }
})

test_that("get_historical_fills() returns a tibble on demo", {
  skip_if_not_demo()

  fills <- get_historical_fills(limit = 5L, creds = live_creds())

  expect_s3_class(fills, "tbl_df")
  expect_true(all(c("fill_id", "ticker", "yes_price") %in% names(fills)))
})

test_that("get_historical_orders() returns a tibble on demo", {
  skip_if_not_demo()

  orders <- get_historical_orders(limit = 5L, creds = live_creds())

  expect_s3_class(orders, "tbl_df")
  expect_true(all(c("order_id", "ticker", "status") %in% names(orders)))
})

test_that("get_historical_candlesticks() returns candles for a known market", {
  skip_if_not_demo()

  # First find a settled market to use as our test subject
  hist <- get_historical_markets(
    status = "settled",
    limit  = 1L,
    creds  = live_creds()
  )
  skip_if(nrow(hist) == 0L, "No settled markets found in historical store")

  ticker        <- hist$ticker[[1]]
  series_ticker <- hist$series_ticker[[1]]
  close_time    <- hist$close_time[[1]]

  # Request one day of hourly candles ending at close
  candles <- get_historical_candlesticks(
    series_ticker   = series_ticker,
    ticker          = ticker,
    start_ts        = close_time - 86400,   # 24 hours before close
    end_ts          = close_time,
    period_interval = 60L,
    creds           = live_creds()
  )

  expect_s3_class(candles, "tbl_df")
  expect_true(all(c("end_period_ts", "open_price", "close_price",
                    "volume") %in% names(candles)))

  if (nrow(candles) > 0L) {
    expect_true(all(candles$open_price  >= 0 & candles$open_price  <= 1,
                    na.rm = TRUE))
    expect_true(all(candles$close_price >= 0 & candles$close_price <= 1,
                    na.rm = TRUE))
  }
})
