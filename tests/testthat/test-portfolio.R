# tests/testthat/test-portfolio.R
# Tests for R/portfolio.R
#
# Unit tests run offline (no credentials needed).
# Integration tests are skipped unless KALSHI_DEMO=true is set.

library(testthat)

# ---------------------------------------------------------------------------
# Unit tests — .parse_positions()
# ---------------------------------------------------------------------------

test_that(".parse_positions() returns empty tibble for zero rows", {
  result <- kalshiR:::.parse_positions(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("ticker", "event_ticker", "side", "position",
                         "market_exposure", "realized_pnl", "unrealized_pnl",
                         "total_traded", "fees_paid"))
})

test_that(".parse_positions() converts cent fields to dollars", {
  fake_rows <- list(
    list(
      ticker               = "FAKE-MKT",
      event_ticker         = "FAKE",
      side                 = "yes",
      position             = 10L,
      market_exposure_cents = 500L,    # $5.00
      realized_pnl         = 200L,    # $2.00
      unrealized_pnl       = -100L,   # -$1.00
      total_traded         = 20L,
      fees_paid            = 10L      # $0.10
    )
  )

  result <- kalshiR:::.parse_positions(fake_rows)

  expect_equal(nrow(result), 1L)
  expect_equal(result$market_exposure, 5.00)
  expect_equal(result$realized_pnl,    2.00)
  expect_equal(result$unrealized_pnl, -1.00)
  expect_equal(result$fees_paid,       0.10)
})


# ---------------------------------------------------------------------------
# Unit tests — .parse_fills()
# ---------------------------------------------------------------------------

test_that(".parse_fills() returns empty tibble for zero rows", {
  result <- kalshiR:::.parse_fills(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("fill_id", "order_id", "ticker", "side", "action",
                         "count", "yes_price", "no_price", "is_taker",
                         "created_time"))
})

test_that(".parse_fills() converts prices to probabilities", {
  fake_rows <- list(
    list(
      fill_id      = "fill-001",
      order_id     = "ord-001",
      ticker       = "FAKE-MKT",
      side         = "yes",
      action       = "buy",
      count        = 5L,
      yes_price    = 60L,   # should become 0.60
      no_price     = 40L,   # should become 0.40
      is_taker     = TRUE,
      created_time = "2024-01-15T10:30:00Z"
    )
  )

  result <- kalshiR:::.parse_fills(fake_rows)

  expect_equal(result$yes_price, 0.60, tolerance = 1e-6)
  expect_equal(result$no_price,  0.40, tolerance = 1e-6)
  expect_true(result$is_taker)
  expect_s3_class(result$created_time, "POSIXct")
})

test_that(".parse_fills() handles missing optional fields gracefully", {
  # Minimal record — many fields absent
  fake_rows <- list(
    list(fill_id = "fill-002", ticker = "FAKE-MKT", count = 1L)
  )

  result <- kalshiR:::.parse_fills(fake_rows)
  expect_equal(nrow(result), 1L)
  # NA expected for absent fields
  expect_true(is.na(result$order_id))
  expect_true(is.na(result$side))
})


# ---------------------------------------------------------------------------
# Unit tests — .parse_settlements()
# ---------------------------------------------------------------------------

test_that(".parse_settlements() returns empty tibble for zero rows", {
  result <- kalshiR:::.parse_settlements(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("market_result", "ticker", "settled_time",
                         "revenue", "realized_pnl", "no_count", "yes_count"))
})

test_that(".parse_settlements() converts revenue and pnl from cents", {
  fake_rows <- list(
    list(
      market_result = "yes",
      ticker        = "FAKE-MKT",
      settled_time  = "2024-01-20T18:00:00Z",
      revenue       = 100L,   # $1.00
      realized_pnl  = 40L,    # $0.40
      no_count      = 0L,
      yes_count     = 1L
    )
  )

  result <- kalshiR:::.parse_settlements(fake_rows)

  expect_equal(result$revenue,      1.00)
  expect_equal(result$realized_pnl, 0.40)
  expect_equal(result$market_result, "yes")
})


# ---------------------------------------------------------------------------
# Unit tests — .to_kalshi_ms()
# ---------------------------------------------------------------------------

test_that(".to_kalshi_ms() converts POSIXct to millisecond string", {
  ts  <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  ms  <- kalshiR:::.to_kalshi_ms(ts)
  # 2024-01-01 UTC in ms: 1704067200000
  expect_equal(ms, "1704067200000")
})

test_that(".to_kalshi_ms() converts ISO-8601 string", {
  ms <- kalshiR:::.to_kalshi_ms("2024-01-01T00:00:00Z")
  expect_equal(ms, "1704067200000")
})


# ---------------------------------------------------------------------------
# Integration tests — require demo credentials
# ---------------------------------------------------------------------------

test_that("get_balance() returns a valid one-row tibble on demo", {
  skip_if_not_demo()

  bal <- get_balance(creds = live_creds())

  expect_s3_class(bal, "tbl_df")
  expect_equal(nrow(bal), 1L)
  expect_true(all(c("balance", "portfolio_value", "bonus_balance",
                    "total_value") %in% names(bal)))
  # All dollar values should be non-negative
  expect_true(bal$balance        >= 0)
  expect_true(bal$portfolio_value >= 0)
  expect_true(bal$bonus_balance  >= 0)
  # total_value should equal balance + portfolio_value
  expect_equal(bal$total_value, bal$balance + bal$portfolio_value,
               tolerance = 1e-6)
})

test_that("get_positions() returns a tibble on demo", {
  skip_if_not_demo()

  pos <- get_positions(limit = 50L, creds = live_creds())

  expect_s3_class(pos, "tbl_df")
  # May be empty if no open positions, but must have correct columns
  expect_true(all(c("ticker", "side", "position") %in% names(pos)))

  if (nrow(pos) > 0L) {
    expect_true(all(pos$side %in% c("yes", "no")))
    # Dollar amounts should be finite
    expect_true(all(is.finite(pos$market_exposure)))
  }
})

test_that("get_fills() returns a tibble on demo", {
  skip_if_not_demo()

  fills <- get_fills(limit = 10L, creds = live_creds())

  expect_s3_class(fills, "tbl_df")
  expect_true(all(c("fill_id", "ticker", "yes_price", "created_time")
                  %in% names(fills)))

  if (nrow(fills) > 0L) {
    expect_true(all(fills$yes_price >= 0 & fills$yes_price <= 1,
                    na.rm = TRUE))
    expect_s3_class(fills$created_time, "POSIXct")
  }
})

test_that("get_settlements() returns a tibble on demo", {
  skip_if_not_demo()

  sett <- get_settlements(limit = 10L, creds = live_creds())

  expect_s3_class(sett, "tbl_df")
  expect_true(all(c("market_result", "ticker", "revenue") %in% names(sett)))

  if (nrow(sett) > 0L) {
    expect_true(all(sett$market_result %in% c("yes", "no"), na.rm = TRUE))
    expect_true(all(sett$revenue >= 0, na.rm = TRUE))
  }
})
