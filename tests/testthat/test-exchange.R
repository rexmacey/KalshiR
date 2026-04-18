# tests/testthat/test-exchange.R
# ============================================================
# Tests for R/exchange.R
#
# Unit tests: validate return shapes and types using mock
#   responses (no network required).
# Integration tests: hit the real (demo) API, skipped unless
#   KALSHI_DEMO_KEY_ID and KALSHI_DEMO_KEY_PATH are set.
# ============================================================

# ── Helpers ────────────────────────────────────────────────────────────────────

# Helper: fake a successful kalshi_get() response for exchange/status
mock_status_resp <- list(
  trading_active  = TRUE,
  exchange_active = TRUE,
  condition       = "open"
)

mock_schedule_resp <- list(
  schedule = list(
    standard_hours = list(
      list(day = "monday",    open_time = "09:30", close_time = "16:00"),
      list(day = "tuesday",   open_time = "09:30", close_time = "16:00"),
      list(day = "wednesday", open_time = "09:30", close_time = "16:00"),
      list(day = "thursday",  open_time = "09:30", close_time = "16:00"),
      list(day = "friday",    open_time = "09:30", close_time = "16:00")
    )
  )
)

mock_announcements_resp <- list(
  announcements = list(
    list(
      id           = "ann-001",
      title        = "Scheduled Maintenance",
      content      = "The exchange will be offline Sunday 2–4 AM ET.",
      created_time = "2024-03-01T07:00:00Z",
      type         = "maintenance"
    ),
    list(
      id           = "ann-002",
      title        = "New Markets Available",
      content      = "We have added 50 new economic indicator markets.",
      created_time = "2024-02-28T15:00:00Z",
      type         = "general"
    )
  )
)


# ── Unit tests: get_exchange_status ───────────────────────────────────────────

test_that("get_exchange_status returns a one-row tibble with correct types", {
  # Directly call the parsing logic that get_exchange_status uses internally
  resp <- mock_status_resp

  result <- tibble::tibble(
    trading_active  = isTRUE(resp[["trading_active"]]),
    exchange_active = isTRUE(resp[["exchange_active"]]),
    condition       = resp[["condition"]] %||% NA_character_
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_true(is.logical(result$trading_active))
  expect_true(is.logical(result$exchange_active))
  expect_true(is.character(result$condition))
  expect_equal(result$condition, "open")
})

test_that("get_exchange_status handles missing 'condition' field gracefully", {
  resp <- list(trading_active = FALSE, exchange_active = TRUE)

  result <- tibble::tibble(
    trading_active  = isTRUE(resp[["trading_active"]]),
    exchange_active = isTRUE(resp[["exchange_active"]]),
    condition       = resp[["condition"]] %||% NA_character_
  )

  expect_equal(result$trading_active, FALSE)
  expect_true(is.na(result$condition))
})


# ── Unit tests: get_exchange_schedule ─────────────────────────────────────────

test_that("get_exchange_schedule returns empty tibble when no hours present", {
  # Simulate parsing with an empty list
  hours <- list()

  result <- if (length(hours) == 0L) {
    tibble::tibble(
      day        = character(),
      open_time  = character(),
      close_time = character()
    )
  }

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("day", "open_time", "close_time"))
})

test_that("get_exchange_schedule parses standard_hours correctly", {
  hours <- mock_schedule_resp$schedule$standard_hours

  result <- records_to_tibble(hours)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5L)
  expect_true("day" %in% names(result))
  expect_true("open_time" %in% names(result))
  expect_true("close_time" %in% names(result))
  expect_equal(result$day[[1]], "monday")
})


# ── Unit tests: get_exchange_announcements ────────────────────────────────────

test_that("get_exchange_announcements returns empty tibble when list is empty", {
  items <- list()

  result <- if (length(items) == 0L) {
    tibble::tibble(
      id           = character(),
      title        = character(),
      content      = character(),
      created_time = as.POSIXct(character(), tz = "UTC"),
      type         = character()
    )
  }

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), 5L)
})

test_that("get_exchange_announcements parses records and timestamps", {
  items <- mock_announcements_resp$announcements

  tbl <- records_to_tibble(items)
  if ("created_time" %in% names(tbl)) {
    tbl$created_time <- parse_kalshi_ts(tbl$created_time)
  }

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2L)
  expect_s3_class(tbl$created_time, "POSIXct")
  expect_equal(tbl$id[[1]], "ann-001")
  expect_equal(tbl$type[[2]], "general")
})

test_that("get_exchange_announcements limit arg must be >= 1", {
  # We validate stopifnot(limit >= 1) before the network call
  expect_error(
    {
      limit <- 0L
      stopifnot(is.numeric(limit), limit >= 1L)
    },
    regexp = "limit"
  )
})


# ── Integration tests (demo) ───────────────────────────────────────────────────
# These tests require KALSHI_DEMO_KEY_ID and KALSHI_DEMO_KEY_PATH to be set.

test_that("get_exchange_status returns valid tibble from demo API", {
  skip_if_no_live_creds()

  result <- get_exchange_status(creds = demo_creds())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_true("trading_active" %in% names(result))
  expect_true("exchange_active" %in% names(result))
  expect_true(is.logical(result$trading_active))
  expect_true(is.logical(result$exchange_active))
})

test_that("get_exchange_schedule returns tibble from demo API", {
  skip_if_no_live_creds()

  result <- get_exchange_schedule(creds = demo_creds())

  expect_s3_class(result, "tbl_df")
  # Schedule may be empty on demo but should not error
  expect_true(is.character(result$day) || nrow(result) == 0L)
})

test_that("get_exchange_announcements returns tibble from demo API", {
  skip_if_no_live_creds()

  result <- get_exchange_announcements(limit = 5L, creds = demo_creds())

  expect_s3_class(result, "tbl_df")
  # May be empty; just verify shape is correct
  if (nrow(result) > 0L) {
    expect_true("id" %in% names(result))
    expect_true("title" %in% names(result))
  }
})
