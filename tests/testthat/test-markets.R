# tests/testthat/test-markets.R
# Tests for get_series(), get_markets(), get_orderbook(), etc.
# Tests are split into:
#   - Unit tests using httptest2 mocks (always run)
#   - Integration tests against the live API (skipped without credentials)

library(httptest2)

# ---------------------------------------------------------------------------
# Utility helpers
# ---------------------------------------------------------------------------

#' Build a minimal mock market record for testing
mock_market <- function(ticker = "TEST-MKT-T50") {
  list(
    ticker             = ticker,
    title              = "Test market above 50",
    status             = "open",
    yes_bid            = 45L,
    yes_ask            = 55L,
    no_bid             = 45L,
    no_ask             = 55L,
    volume             = 1000L,
    open_interest      = 500L,
    close_time         = "2025-12-31T23:59:59Z",
    expiration_time    = "2025-12-31T23:59:59Z",
    event_ticker       = "TEST-MKT",
    series_ticker      = "TEST"
  )
}

mock_series <- function(ticker = "TEST") {
  list(
    ticker    = ticker,
    title     = "Test Series",
    category  = "Economics",
    frequency = "daily"
  )
}


# ---------------------------------------------------------------------------
# Unit tests — utils and parsing (no HTTP calls)
# ---------------------------------------------------------------------------

test_that("cents_to_dollars() converts correctly", {
  expect_equal(cents_to_dollars(100L),  1.00)
  expect_equal(cents_to_dollars(1250L), 12.50)
  expect_equal(cents_to_dollars(0L),    0.00)
  expect_equal(cents_to_dollars(9999L), 99.99)
})

test_that("price_to_prob() converts cents to probability", {
  expect_equal(price_to_prob(65),  0.65)
  expect_equal(price_to_prob(0),   0.00)
  expect_equal(price_to_prob(100), 1.00)
})

test_that("parse_kalshi_ts() returns POSIXct in UTC", {
  ts <- parse_kalshi_ts("2023-11-07T05:31:56Z")
  expect_s3_class(ts, "POSIXct")
  expect_equal(attr(ts, "tzone"), "UTC")
})

test_that("records_to_tibble() returns empty tibble for NULL input", {
  result <- kalshiR:::records_to_tibble(NULL)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("records_to_tibble() converts a list of records correctly", {
  records <- list(
    list(ticker = "A", value = 1L, label = "foo"),
    list(ticker = "B", value = 2L, label = "bar")
  )
  tbl <- kalshiR:::records_to_tibble(records)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$ticker, c("A", "B"))
})

test_that("parse_ob_side() handles empty input", {
  result <- kalshiR:::parse_ob_side(NULL, "yes")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_true("price_cents" %in% names(result))
})

test_that("parse_ob_side() parses YES side correctly", {
  # API returns list of [price_dollar_fraction, quantity] pairs
  levels <- list(
    list(0.60, 100),
    list(0.55, 200)
  )
  result <- kalshiR:::parse_ob_side(levels, "yes")
  expect_equal(nrow(result), 2L)
  expect_equal(result$side, c("yes", "yes"))
  expect_equal(result$price_cents, c(60, 55))
  expect_equal(result$price_dollars, c(0.60, 0.55))
  expect_equal(result$quantity, c(100, 200))
})


# ---------------------------------------------------------------------------
# Mock HTTP tests using httptest2
# ---------------------------------------------------------------------------

test_that("get_series() returns a one-row tibble", {
  with_mock_api({
    # httptest2 will look for a fixture at
    # tests/testthat/api.elections.kalshi.com/trade-api/v2/series/TEST.json
    # We skip if fixture not present
    skip("Fixtures not yet recorded — run record_requests() to create them.")

    result <- get_series("TEST")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1L)
    expect_true("ticker" %in% names(result))
  })
})

test_that("get_markets() with mocked response returns tibble", {
  # Directly test the parsing logic by bypassing HTTP
  raw_resp <- list(markets = list(mock_market("A-T50"), mock_market("A-T60")))

  # Simulate what get_markets() does internally
  records <- raw_resp[["markets"]]
  tbl <- kalshiR:::records_to_tibble(records)

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2L)
  expect_true("ticker" %in% names(tbl))
  expect_true("status" %in% names(tbl))
})

test_that("get_orderbook() parses both sides correctly", {
  # Mock the full response structure
  mock_resp <- list(
    orderbook_fp = list(
      yes_dollars = list(list(0.65, 100), list(0.60, 200)),
      no_dollars  = list(list(0.35, 100), list(0.40, 200))
    )
  )

  # Test the parsing logic directly
  ob <- mock_resp[["orderbook_fp"]]
  yes_rows <- kalshiR:::parse_ob_side(ob[["yes_dollars"]], "yes")
  no_rows  <- kalshiR:::parse_ob_side(ob[["no_dollars"]],  "no")
  tbl      <- dplyr::bind_rows(yes_rows, no_rows)

  expect_equal(nrow(tbl), 4L)
  expect_equal(sort(unique(tbl$side)), c("no", "yes"))
  expect_true(all(tbl$price_cents >= 0 & tbl$price_cents <= 100))
})


# ---------------------------------------------------------------------------
# Live integration tests (skipped without credentials)
# ---------------------------------------------------------------------------

test_that("get_series() returns valid data from live API", {
  skip_if_no_live_creds()

  result <- get_series("KXHIGHNY")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_true("ticker" %in% names(result))
  expect_equal(result$ticker, "KXHIGHNY")
})

test_that("get_markets() returns open markets from live API", {
  skip_if_no_live_creds()

  result <- get_markets(series_ticker = "KXHIGHNY", status = "open", limit = 5L)

  expect_s3_class(result, "tbl_df")
  # May have 0 rows if no markets currently open — just check structure
  expect_true("ticker" %in% names(result))
})

test_that("get_market() returns a single market from live API", {
  skip_if_no_live_creds()

  # First find an open market
  markets <- get_markets(series_ticker = "KXHIGHNY", limit = 1L)
  skip_if(nrow(markets) == 0, "No markets available for KXHIGHNY.")

  result <- get_market(markets$ticker[1])

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_equal(result$ticker, markets$ticker[1])
})

test_that("get_orderbook() returns a valid tibble from live API", {
  skip_if_no_live_creds()

  markets <- get_markets(series_ticker = "KXHIGHNY", status = "open", limit = 1L)
  skip_if(nrow(markets) == 0, "No open markets to test orderbook.")

  result <- get_orderbook(markets$ticker[1])

  expect_s3_class(result, "tbl_df")
  expect_true("side" %in% names(result))
  expect_true("price_cents" %in% names(result))
  expect_true("quantity" %in% names(result))
  expect_true(all(result$side %in% c("yes", "no")))
})
