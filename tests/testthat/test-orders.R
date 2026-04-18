# tests/testthat/test-orders.R
# Tests for R/orders.R
# Unit tests run offline; integration tests require demo credentials.
# Set env vars KALSHI_DEMO_KEY_ID and KALSHI_DEMO_PRIVATE_KEY_PATH to run live tests.

# ── unit tests: input validation ──────────────────────────────────────────────

test_that("create_order() rejects missing ticker", {
  expect_error(
    create_order(
      ticker = NULL,
      side   = "yes",
      action = "buy",      # <-- add this
      type   = "limit",
      yes_price = 50,      # <-- also needed; limit orders require a price
      count  = 1L,
      creds  = mock_creds
    ),
    regexp = "ticker"
  )
})

test_that("create_order() rejects invalid side", {
  expect_error(
    create_order(
      ticker = "FAKE-TICKER",
      side   = "maybe",          # not "yes" or "no"
      action = "buy",
      type   = "limit",
      count  = 1L,
      creds  = mock_creds
    ),
    regexp = "should be one of",
    fixed  = TRUE
  )
})

test_that("create_order() rejects invalid type", {
  expect_error(
    create_order(
      ticker = "FAKE-TICKER",
      side   = "yes",
      action = "buy",
      type   = "iceberg",        # not "limit" or "market"
      count  = 1L,
      creds  = mock_creds
    ),
    regexp = "should be one of",
    fixed = TRUE
  )
})

test_that("create_order() requires limit_price for limit orders", {
  expect_error(
    create_order(
      ticker      = "FAKE-TICKER",
      side        = "yes",
      action = "buy",
      type        = "limit",
      count       = 1L,
      yes_price = NULL,        # missing!
      creds       = mock_creds
    ),
    regexp = "Limit orders require `yes_price` or `no_price` in cents.",
    fixed = TRUE
  )
})

test_that("create_order() rejects limit_price outside 1–99", {
  expect_error(
    create_order(
      ticker      = "FAKE-TICKER",
      side        = "yes",
      action      = "buy",
      type        = "limit",
      count       = 1L,
      yes_price = 0, # below limit
      creds       = mock_creds
    ),
    regexp = "`yes_price` must be between 1 and 99 cents.",
    fixed = TRUE
  )

  expect_error(
    create_order(
      ticker      = "FAKE-TICKER",
      side        = "yes",
      action = "buy",
      type        = "limit",
      count       = 1L,
      yes_price = 100,         # above maximum
      creds       = mock_creds
    ),
    regexp = "`yes_price` must be between 1 and 99 cents.",
    fixed = TRUE
  )
})

test_that("create_order() rejects non-positive count", {
  expect_error(
    create_order(
      ticker      = "FAKE-TICKER",
      side        = "yes",
      action = "buy",
      type        = "limit",
      count       = 0L,
      yes_price = 10L,
      creds       = mock_creds
    ),
    regexp = "Count 0 must be greater than zero.",
    fixed = TRUE
  )
})

test_that("cancel_order() rejects missing order_id", {
  expect_error(
    cancel_order(order_id = NULL, creds = mock_creds),
    regexp = "order_id"
  )

  expect_error(
    cancel_order(order_id = "", creds = mock_creds),
    regexp = "order_id"
  )
})

test_that("get_order() rejects missing order_id", {
  expect_error(
    get_order(order_id = NULL, creds = mock_creds),
    regexp = "`order_id` must be a non-empty string",
    fixed = TRUE
  )
})

test_that("amend_order() rejects missing order_id", {
  expect_error(
    amend_order(order_id = NULL, count = 2L, creds = mock_creds),
    regexp = "order_id"
  )
})

test_that("decrease_order() rejects missing order_id", {
  expect_error(
    decrease_order(order_id = NULL, reduce_by = 1L, creds = mock_creds),
    regexp = "order_id"
  )
})

test_that("decrease_order() rejects non-positive reduce_by", {
  expect_error(
    decrease_order(order_id = "abc123", reduce_by = 0L, creds = mock_creds),
    regexp = "reduce_by",
    fixed = TRUE
  )
})

test_that("batch_create_orders() rejects empty orders list", {
  expect_error(
    batch_create_orders(orders = list(), creds = mock_creds),
    regexp = "orders"
  )
})

test_that("batch_create_orders() rejects non-list input", {
  expect_error(
    batch_create_orders(orders = "not-a-list", creds = mock_creds),
    regexp = "orders"
  )
})

test_that("batch_cancel_orders() rejects empty ids vector", {
  expect_error(
    batch_cancel_orders(order_ids = character(0), creds = mock_creds),
    regexp = "order_ids"
  )
})

# ── unit tests: build_order_body() helper ─────────────────────────────────────
# These test the internal body-construction logic without hitting the API.

test_that(".build_order_body() produces correct structure for limit order", {
  body <- kalshiR:::.build_order_body(
    ticker          = "TICKER-A",
    side            = "yes",
    action          = "buy",      # <-- explicit in actual code
    type            = "limit",
    count           = 5L,
    yes_price       = 35L,        # <-- actual param name
    expiration_ts   = NULL,
    client_order_id = NULL
  )

  expect_equal(body$action,    "buy")
  expect_equal(body$ticker,    "TICKER-A")
  expect_equal(body$side,      "yes")
  expect_equal(body$type,      "limit")
  expect_equal(body$count,     5L)
  expect_equal(body$yes_price, 35L)
  expect_null(body$no_price)
  expect_null(body$expiration_ts)
  expect_null(body$client_order_id)
})

test_that("build_order_body() uses no_price field when side = 'no'", {
  body <- kalshiR:::.build_order_body(
    ticker      = "TICKER-B",
    side        = "no",
    action          = "buy",
    type        = "limit",
    count       = 2L,
    yes_price = NULL,
    no_price = 60L,
    expiration_ts = NULL,
    client_order_id = NULL
  )

  expect_equal(body$no_price,  60L)
  expect_null(body$yes_price)
})

test_that("build_order_body() omits price fields for market orders", {
  body <- kalshiR:::.build_order_body(
    ticker      = "TICKER-C",
    side        = "yes",
    action          = "buy",
    type        = "market",
    count       = 1L,
    expiration_ts = NULL,
    client_order_id = NULL
  )

  expect_equal(body$type, "market")
  expect_null(body$yes_price)
  expect_null(body$no_price)
})

test_that("build_order_body() includes expiration_ts and client_order_id when provided", {
  ts  <- as.integer(Sys.time()) + 3600L
  cid <- "my-client-id-001"

  body <- kalshiR:::.build_order_body(
    ticker          = "TICKER-D",
    side            = "yes",
    action          = "buy",
    type            = "limit",
    count           = 1L,
    yes_price     = 20L,
    expiration_ts   = ts,
    client_order_id = cid
  )

  expect_equal(body$expiration_ts,   ts)
  expect_equal(body$client_order_id, cid)
})

# ── unit tests: parse_order() / parse_orders_response() ─────────────────────

test_that("parse_order() converts a raw list to a one-row tibble", {
  raw <- list(
    order_id         = "ord-001",
    ticker           = "TICKER-E",
    client_order_id  = "cid-001",
    type             = "limit",
    action           = "buy",
    side             = "yes",
    status           = "resting",
    yes_price        = 35L,
    no_price         = 65L,
    count            = 10L,
    filled_count     = 0L,
    remaining_count  = 10L,
    place_count      = 10L,
    decrease_count   = 0L,
    queue_position   = 1L,
    expiration_time  = NULL,
    created_time     = "2024-01-15T12:00:00Z",
    last_update_time = "2024-01-15T12:00:01Z",
    close_cancel_count = 0L,
    fcc_cancel_count   = 0L,
    user_id          = "user-xyz"
  )

  result <- kalshiR:::.parse_orders(raw)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_equal(result$order_id,  "ord-001")
  expect_equal(result$ticker,    "TICKER-E")
  expect_equal(result$status,    "resting")
  expect_equal(result$count,     10L)
  expect_s3_class(result$created_time, "POSIXct")
  expect_equal(attr(result$created_time, "tzone"), "UTC")

  # Price conversion: cents → dollars
  expect_equal(result$yes_price, 0.35)
  expect_equal(result$no_price,  0.65)
})

test_that("parse_order() handles NULL timestamps gracefully", {
  raw <- list(
    order_id        = "ord-002",
    ticker          = "TICKER-F",
    type            = "market",
    action          = "buy",
    side            = "no",
    status          = "filled",
    yes_price       = NULL,
    no_price        = 40L,
    count           = 1L,
    filled_count    = 1L,
    remaining_count = 0L,
    created_time    = NULL,
    last_update_time = "2024-01-15T13:00:00Z"
  )

  result <- kalshiR:::.parse_orders(raw)

  expect_true(is.na(result$created_time))
  expect_equal(result$status, "filled")
})

test_that("get_orders() returns a tibble", {
  raw_list <- list(
    list(
      order_id = "o1", ticker = "T1", type = "limit", action = "buy",
      side = "yes", status = "resting", yes_price = 30L, no_price = 70L,
      count = 5L, filled_count = 0L, remaining_count = 5L,
      created_time = "2024-01-10T10:00:00Z",
      last_update_time = "2024-01-10T10:00:01Z"
    ),
    list(
      order_id = "o2", ticker = "T2", type = "limit", action = "buy",
      side = "no", status = "canceled", yes_price = 55L, no_price = 45L,
      count = 2L, filled_count = 0L, remaining_count = 2L,
      created_time = "2024-01-11T09:00:00Z",
      last_update_time = "2024-01-11T09:05:00Z"
    )
  )

  result <- purrr::map_dfr(raw_list, kalshiR:::.parse_orders)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_true(all(c("order_id", "ticker", "status", "yes_price") %in% names(result)))
})

# ── integration tests ─────────────────────────────────────────────────────────

test_that("get_orders() returns a tibble on demo", {
  skip_if_not_demo()

  result <- get_orders(creds = demo_creds())

  expect_s3_class(result, "tbl_df")
  # May be empty if no orders have been placed; just check columns exist
  expect_true(all(c("order_id", "ticker", "type", "side", "status",
                    "count", "filled_count", "remaining_count",
                    "yes_price", "no_price",
                    "created_time", "last_update_time") %in% names(result)))
})

test_that("get_orders() filters by status correctly on demo", {
  skip_if_not_demo()

  resting  <- get_orders(status = "resting",  creds = live_creds())
  canceled <- get_orders(status = "canceled", creds = live_creds())
  filled   <- get_orders(status = "filled",   creds = live_creds())

  # Each should be a tibble (possibly empty)
  expect_s3_class(resting,  "tbl_df")
  expect_s3_class(canceled, "tbl_df")
  expect_s3_class(filled,   "tbl_df")

  # If non-empty, verify status column matches filter
  if (nrow(resting)  > 0) expect_true(all(resting$status  == "resting"))
  if (nrow(canceled) > 0) expect_true(all(canceled$status == "canceled"))
  if (nrow(filled)   > 0) expect_true(all(filled$status   == "filled"))
})

test_that("full order lifecycle works on demo: create → get → cancel", {
  skip_if_not_demo()

  # Pick the first available market
  markets <- get_markets(status = "open", limit = 1L, creds = live_creds())
  skip_if(nrow(markets) == 0, "No open markets on demo")

  ticker <- markets$ticker[[1]]

  # ── 1. Place a low-probability limit order unlikely to fill immediately ──
  order <- create_order(
    ticker      = ticker,
    side        = "yes",
    action      = "buy",
    type        = "limit",
    count       = 1L,
    yes_price = 1L,            # 1 cent — extremely unlikely to fill
    creds       = live_creds()
  )

  expect_s3_class(order, "tbl_df")
  expect_equal(nrow(order), 1L)
  expect_equal(order$ticker, ticker)
  expect_true(order$status %in% c("resting", "pending"))

  order_id <- order$order_id[[1]]
  expect_false(is.na(order_id))
  expect_true(nchar(order_id) > 0L)

  # ── 2. Retrieve the order by ID ──────────────────────────────────────────
  fetched <- get_order(order_id = order_id, creds = live_creds())

  expect_s3_class(fetched, "tbl_df")
  expect_equal(nrow(fetched), 1L)
  expect_equal(fetched$order_id, order_id)
  expect_equal(fetched$ticker,   ticker)

  # ── 3. Verify the order appears in get_orders() ──────────────────────────
  all_resting <- get_orders(status = "resting", creds = live_creds())
  expect_true(order_id %in% all_resting$order_id)

  # ── 4. Cancel the order ──────────────────────────────────────────────────
  canceled <- cancel_order(order_id = order_id, creds = live_creds())

  expect_s3_class(canceled, "tbl_df")
  expect_equal(nrow(canceled), 1L)
  expect_equal(canceled$order_id, order_id)
  expect_equal(canceled$status,   "canceled")

  # ── 5. Confirm it no longer appears in resting orders ────────────────────
  resting_after <- get_orders(status = "resting", creds = live_creds())
  expect_false(order_id %in% resting_after$order_id)
})

test_that("amend_order() updates count on demo", {
  skip_if_not_demo()

  markets <- get_markets(status = "open", limit = 1L, creds = live_creds())
  skip_if(nrow(markets) == 0, "No open markets on demo")

  ticker <- markets$ticker[[1]]

  # Place initial order with count = 3
  order <- create_order(
    ticker      = ticker,
    side        = "yes",
    type        = "limit",
    count       = 3L,
    limit_price = 1L,
    creds       = live_creds()
  )

  order_id <- order$order_id[[1]]
  on.exit(
    tryCatch(
      cancel_order(order_id = order_id, creds = live_creds()),
      error = function(e) invisible(NULL)
    ),
    add = TRUE
  )

  # Amend to count = 5
  amended <- amend_order(
    order_id    = order_id,
    count       = 5L,
    limit_price = 1L,            # price required for amend on Kalshi
    creds       = live_creds()
  )

  expect_s3_class(amended, "tbl_df")
  expect_equal(amended$order_id, order_id)
  # After amendment the new count should be reflected
  expect_equal(amended$count, 5L)
})

test_that("decrease_order() reduces remaining count on demo", {
  skip_if_not_demo()

  markets <- get_markets(status = "open", limit = 1L, creds = live_creds())
  skip_if(nrow(markets) == 0, "No open markets on demo")

  ticker <- markets$ticker[[1]]

  order <- create_order(
    ticker      = ticker,
    side        = "yes",
    type        = "limit",
    count       = 4L,
    limit_price = 1L,
    creds       = live_creds()
  )

  order_id <- order$order_id[[1]]
  on.exit(
    tryCatch(
      cancel_order(order_id = order_id, creds = live_creds()),
      error = function(e) invisible(NULL)
    ),
    add = TRUE
  )

  decreased <- decrease_order(
    order_id  = order_id,
    reduce_by = 2L,
    creds     = live_creds()
  )

  expect_s3_class(decreased, "tbl_df")
  expect_equal(decreased$order_id,        order_id)
  expect_equal(decreased$remaining_count, 2L)
})

test_that("batch_create_orders() places multiple orders on demo", {
  skip_if_not_demo()

  markets <- get_markets(status = "open", limit = 2L, creds = live_creds())
  skip_if(nrow(markets) < 2L, "Need at least 2 open markets for batch test")

  orders_spec <- list(
    list(
      ticker      = markets$ticker[[1]],
      side        = "yes",
      type        = "limit",
      count       = 1L,
      limit_price = 1L
    ),
    list(
      ticker      = markets$ticker[[2]],
      side        = "yes",
      type        = "limit",
      count       = 1L,
      limit_price = 1L
    )
  )

  result <- batch_create_orders(orders = orders_spec, creds = live_creds())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_true(all(c("order_id", "ticker", "status") %in% names(result)))

  # Clean up — cancel each order that was successfully placed
  placed_ids <- result$order_id[!is.na(result$order_id)]
  for (oid in placed_ids) {
    tryCatch(
      cancel_order(order_id = oid, creds = live_creds()),
      error = function(e) invisible(NULL)
    )
  }
})

test_that("batch_cancel_orders() cancels multiple orders on demo", {
  skip_if_not_demo()

  markets <- get_markets(status = "open", limit = 2L, creds = live_creds())
  skip_if(nrow(markets) < 2L, "Need at least 2 open markets for batch cancel test")

  # Place two orders to cancel
  o1 <- create_order(
    ticker = markets$ticker[[1]], side = "yes", type = "limit",
    count = 1L, limit_price = 1L, creds = live_creds()
  )
  o2 <- create_order(
    ticker = markets$ticker[[2]], side = "yes", type = "limit",
    count = 1L, limit_price = 1L, creds = live_creds()
  )

  ids <- c(o1$order_id[[1]], o2$order_id[[1]])

  result <- batch_cancel_orders(order_ids = ids, creds = live_creds())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_true(all(result$order_id %in% ids))
  # All returned statuses should indicate cancellation
  expect_true(all(result$status == "canceled"))
})

test_that("create_order() with client_order_id is idempotent on demo", {
  skip_if_not_demo()

  markets <- get_markets(status = "open", limit = 1L, creds = live_creds())
  skip_if(nrow(markets) == 0, "No open markets on demo")

  cid <- paste0("test-idem-", format(Sys.time(), "%Y%m%d%H%M%S"))

  order1 <- create_order(
    ticker          = markets$ticker[[1]],
    side            = "yes",
    type            = "limit",
    count           = 1L,
    limit_price     = 1L,
    client_order_id = cid,
    creds           = live_creds()
  )

  order2 <- create_order(
    ticker          = markets$ticker[[1]],
    side            = "yes",
    type            = "limit",
    count           = 1L,
    limit_price     = 1L,
    client_order_id = cid,          # same client ID → same order
    creds           = live_creds()
  )

  # Both calls should return the same order ID
  expect_equal(order1$order_id, order2$order_id)

  # Clean up
  tryCatch(
    cancel_order(order_id = order1$order_id[[1]], creds = live_creds()),
    error = function(e) invisible(NULL)
  )
})

test_that("price columns are in dollars (0–1 range) for limit orders", {
  skip_if_not_demo()

  markets <- get_markets(status = "open", limit = 1L, creds = live_creds())
  skip_if(nrow(markets) == 0, "No open markets on demo")

  order <- create_order(
    ticker      = markets$ticker[[1]],
    side        = "yes",
    type        = "limit",
    count       = 1L,
    limit_price = 25L,             # 25 cents
    creds       = live_creds()
  )

  on.exit(
    tryCatch(
      cancel_order(order_id = order$order_id[[1]], creds = live_creds()),
      error = function(e) invisible(NULL)
    ),
    add = TRUE
  )

  # yes_price should be 0.25, not 25
  expect_true(order$yes_price >= 0 & order$yes_price <= 1,
              label = "yes_price is in dollar (probability) units")
  expect_equal(order$yes_price, 0.25)
})
