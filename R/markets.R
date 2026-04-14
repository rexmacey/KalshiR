#' @title Kalshi Market Data Functions
#'
#' @description
#' Functions for accessing live and current market data on Kalshi, including
#' series information, events, individual markets, orderbooks, recent trades,
#' and candlestick (OHLC) price history.
#'
#' All of these endpoints are publicly accessible without authentication for
#' basic use. Pass `creds` if you have credentials set up (e.g. to use the
#' demo environment).
#'
#' ## Key concepts
#' - **Series**: A collection of related markets (e.g. "NYC High Temperature").
#' - **Event**: A single instance within a series (e.g. "NYC High Temp on Jan 5").
#' - **Market**: A single binary YES/NO question within an event. This is what
#'   you trade.
#' - **Ticker**: A unique string identifier for a series, event, or market.
#'
#' @name markets
NULL


# ---------------------------------------------------------------------------
# Series
# ---------------------------------------------------------------------------

#' Get information about a single series
#'
#' Retrieves metadata for a Kalshi series, such as its title, category,
#' frequency, and settlement rules.
#'
#' @param series_ticker `character(1)`. The series ticker, e.g. `"KXHIGHNY"`.
#' @param creds A `kalshi_credentials` object or `NULL` for unauthenticated
#'   access (uses global creds if set, otherwise unauthenticated).
#'
#' @return A one-row `tibble` with series metadata fields including:
#'   `ticker`, `title`, `category`, `frequency`, `settlement_sources`.
#'
#' @examples
#' \dontrun{
#' get_series("KXHIGHNY")
#' }
#'
#' @export
get_series <- function(series_ticker, creds = NULL) {
  if (!is.character(series_ticker) || length(series_ticker) != 1) {
    cli::cli_abort("{.arg series_ticker} must be a single character string.")
  }

  resp <- kalshi_get(
    endpoint      = paste0("/series/", series_ticker),
    creds         = creds,
    authenticated = !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))
  )

  series <- resp[["series"]]
  if (is.null(series)) {
    cli::cli_abort("No series data returned for ticker {.val {series_ticker}}.")
  }

  records_to_tibble(list(series))
}


#' Get a list of series
#'
#' Retrieves a paginated list of all Kalshi series, optionally filtered by
#' category or search term.
#'
#' @param category `character(1)` or `NULL`. Filter by category, e.g.
#'   `"Politics"`, `"Economics"`, `"Sports"`.
#' @param limit `integer(1)`. Maximum number of results per page. Default 100.
#' @param all_pages `logical(1)`. If `TRUE`, automatically fetches all pages.
#'   If `FALSE` (default), returns only the first page.
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A `tibble` with one row per series.
#'
#' @examples
#' \dontrun{
#' get_series_list()
#' get_series_list(category = "Economics", all_pages = TRUE)
#' }
#'
#' @export
get_series_list <- function(category  = NULL,
                            limit     = 100L,
                            all_pages = FALSE,
                            creds     = NULL) {

  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  query <- list(
    category = category,
    limit    = as.integer(limit)
  )

  if (all_pages) {
    records <- kalshi_paginate(
      endpoint      = "/series",
      result_key    = "series",
      query         = query,
      creds         = creds,
      authenticated = use_auth
    )
  } else {
    resp    <- kalshi_get("/series", query = query, creds = creds, authenticated = use_auth)
    records <- resp[["series"]]
  }

  records_to_tibble(records)
}


# ---------------------------------------------------------------------------
# Events
# ---------------------------------------------------------------------------

#' Get a list of events
#'
#' Returns events — groupings of related markets — optionally filtered by
#' series, status, or category.
#'
#' @param series_ticker `character(1)` or `NULL`. Filter to a specific series.
#' @param status `character(1)` or `NULL`. One of `"open"`, `"closed"`,
#'   `"settled"`, or `NULL` for all.
#' @param category `character(1)` or `NULL`. Filter by category.
#' @param limit `integer(1)`. Results per page. Default 100.
#' @param all_pages `logical(1)`. Fetch all pages? Default `FALSE`.
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A `tibble` with one row per event.
#'
#' @examples
#' \dontrun{
#' get_events(series_ticker = "KXHIGHNY", status = "open")
#' get_events(category = "Politics", all_pages = TRUE)
#' }
#'
#' @export
get_events <- function(series_ticker = NULL,
                       status        = NULL,
                       category      = NULL,
                       limit         = 100L,
                       all_pages     = FALSE,
                       creds         = NULL) {

  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  query <- list(
    series_ticker = series_ticker,
    status        = status,
    category      = category,
    limit         = as.integer(limit)
  )

  if (all_pages) {
    records <- kalshi_paginate(
      endpoint      = "/events",
      result_key    = "events",
      query         = query,
      creds         = creds,
      authenticated = use_auth
    )
  } else {
    resp    <- kalshi_get("/events", query = query, creds = creds, authenticated = use_auth)
    records <- resp[["events"]]
  }

  records_to_tibble(records)
}


#' Get a single event by ticker
#'
#' @param event_ticker `character(1)`. The event ticker.
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A one-row `tibble` with event metadata.
#'
#' @examples
#' \dontrun{
#' get_event("KXHIGHNY-23NOV07")
#' }
#'
#' @export
get_event <- function(event_ticker, creds = NULL) {
  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  resp  <- kalshi_get(paste0("/events/", event_ticker), creds = creds, authenticated = use_auth)
  event <- resp[["event"]]

  if (is.null(event)) {
    cli::cli_abort("No event data returned for ticker {.val {event_ticker}}.")
  }

  records_to_tibble(list(event))
}


# ---------------------------------------------------------------------------
# Markets
# ---------------------------------------------------------------------------

#' Get a list of markets
#'
#' Returns individual binary markets, optionally filtered by series, event,
#' status, category, or ticker prefix.
#'
#' @param series_ticker `character(1)` or `NULL`. Filter by series.
#' @param event_ticker `character(1)` or `NULL`. Filter by event.
#' @param status `character(1)` or `NULL`. One of `"open"`, `"closed"`,
#'   `"settled"`.
#' @param category `character(1)` or `NULL`. Filter by category.
#' @param tickers `character` or `NULL`. Vector of specific market tickers
#'   to retrieve (passed as comma-separated string).
#' @param limit `integer(1)`. Results per page (max 1000). Default 100.
#' @param all_pages `logical(1)`. Fetch all pages? Default `FALSE`.
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A `tibble` with one row per market. Key columns include `ticker`,
#'   `title`, `status`, `yes_bid`, `yes_ask`, `no_bid`, `no_ask`, `volume`,
#'   `open_interest`, `close_time`, `expiration_time`.
#'
#' @examples
#' \dontrun{
#' # All open markets for a series
#' get_markets(series_ticker = "KXHIGHNY", status = "open")
#'
#' # Specific tickers
#' get_markets(tickers = c("KXHIGHNY-23NOV07-T60", "KXHIGHNY-23NOV07-T65"))
#' }
#'
#' @export
get_markets <- function(series_ticker = NULL,
                        event_ticker  = NULL,
                        status        = NULL,
                        category      = NULL,
                        tickers       = NULL,
                        limit         = 100L,
                        all_pages     = FALSE,
                        creds         = NULL) {

  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  # tickers can be a vector — API expects comma-separated
  tickers_str <- if (!is.null(tickers)) paste(tickers, collapse = ",") else NULL

  query <- list(
    series_ticker = series_ticker,
    event_ticker  = event_ticker,
    status        = status,
    category      = category,
    tickers       = tickers_str,
    limit         = as.integer(limit)
  )

  if (all_pages) {
    records <- kalshi_paginate(
      endpoint      = "/markets",
      result_key    = "markets",
      query         = query,
      creds         = creds,
      authenticated = use_auth
    )
  } else {
    resp    <- kalshi_get("/markets", query = query, creds = creds, authenticated = use_auth)
    records <- resp[["markets"]]
  }

  tbl <- records_to_tibble(records)

  # Tidy up common numeric fields
  tbl <- tidy_market_tibble(tbl)

  tbl
}


#' Get a single market by ticker
#'
#' @param market_ticker `character(1)`. The market ticker.
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A one-row `tibble` with market details.
#'
#' @examples
#' \dontrun{
#' get_market("KXHIGHNY-23NOV07-T60")
#' }
#'
#' @export
get_market <- function(market_ticker, creds = NULL) {
  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  resp   <- kalshi_get(paste0("/markets/", market_ticker), creds = creds, authenticated = use_auth)
  market <- resp[["market"]]

  if (is.null(market)) {
    cli::cli_abort("No market data returned for ticker {.val {market_ticker}}.")
  }

  tbl <- records_to_tibble(list(market))
  tidy_market_tibble(tbl)
}


# ---------------------------------------------------------------------------
# Orderbook
# ---------------------------------------------------------------------------

#' Get the orderbook for a market
#'
#' Returns the current bid/ask levels for both YES and NO sides of a market.
#'
#' Note: Kalshi's orderbook is unique to binary markets. The API only returns
#' bids (not asks) because YES bids and NO bids are complementary — a YES bid
#' at 60¢ implies a NO ask at 40¢.
#'
#' @param market_ticker `character(1)`. The market ticker.
#' @param depth `integer(1)` or `NULL`. Maximum number of price levels to
#'   return per side. `NULL` returns all levels.
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A `tibble` with columns `side` (`"yes"` or `"no"`), `price_cents`,
#'   `price_dollars`, `quantity`.
#'
#' @examples
#' \dontrun{
#' get_orderbook("KXHIGHNY-23NOV07-T60")
#' get_orderbook("KXHIGHNY-23NOV07-T60", depth = 5)
#' }
#'
#' @export
get_orderbook <- function(market_ticker, depth = NULL, creds = NULL) {
  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  query <- list(depth = depth)

  resp <- kalshi_get(
    endpoint      = paste0("/markets/", market_ticker, "/orderbook"),
    query         = query,
    creds         = creds,
    authenticated = use_auth
  )

  ob <- resp[["orderbook_fp"]] %||% resp[["orderbook"]]
  if (is.null(ob)) {
    cli::cli_abort("No orderbook data returned for {.val {market_ticker}}.")
  }

  # Parse YES side: list of [price_dollars, quantity] pairs
  yes_rows <- parse_ob_side(ob[["yes_dollars"]] %||% ob[["yes"]], "yes")
  no_rows  <- parse_ob_side(ob[["no_dollars"]]  %||% ob[["no"]],  "no")

  tbl <- dplyr::bind_rows(yes_rows, no_rows)
  tbl$market_ticker <- market_ticker
  tbl <- dplyr::select(tbl, market_ticker, side, price_cents, price_dollars, quantity)

  tbl
}


#' Parse one side of the orderbook into a tibble
#' @param levels A list of [price, quantity] pairs.
#' @param side `character(1)`. `"yes"` or `"no"`.
#' @keywords internal
parse_ob_side <- function(levels, side) {
  if (is.null(levels) || length(levels) == 0) {
    return(tibble::tibble(
      side          = character(),
      price_cents   = numeric(),
      price_dollars = numeric(),
      quantity      = numeric()
    ))
  }

  rows <- purrr::map(levels, function(pair) {
    # API returns [price_in_dollars_as_string_or_numeric, quantity]
    # price is 0-100 cents scale
    price <- as.numeric(pair[[1]]) * 100  # convert dollar fraction to cents
    qty   <- as.numeric(pair[[2]])
    list(
      side          = side,
      price_cents   = price,
      price_dollars = as.numeric(pair[[1]]),
      quantity      = qty
    )
  })

  dplyr::bind_rows(rows)
}


# ---------------------------------------------------------------------------
# Trades
# ---------------------------------------------------------------------------

#' Get recent trades for a market or series
#'
#' Returns a history of executed trades. Without filters, returns the most
#' recent trades across all markets.
#'
#' @param ticker `character(1)` or `NULL`. Filter to a specific market ticker.
#' @param min_ts `integer(1)` or `NULL`. Filter to trades after this Unix
#'   timestamp (seconds).
#' @param max_ts `integer(1)` or `NULL`. Filter to trades before this Unix
#'   timestamp (seconds).
#' @param limit `integer(1)`. Results per page. Default 100.
#' @param all_pages `logical(1)`. Fetch all pages? Default `FALSE`.
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A `tibble` with columns including `trade_id`, `ticker`, `price`,
#'   `count`, `side`, `created_time`.
#'
#' @examples
#' \dontrun{
#' get_trades(ticker = "KXHIGHNY-23NOV07-T60")
#' }
#'
#' @export
get_trades <- function(ticker    = NULL,
                       min_ts    = NULL,
                       max_ts    = NULL,
                       limit     = 100L,
                       all_pages = FALSE,
                       creds     = NULL) {

  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  query <- list(
    ticker = ticker,
    min_ts = min_ts,
    max_ts = max_ts,
    limit  = as.integer(limit)
  )

  if (all_pages) {
    records <- kalshi_paginate(
      endpoint      = "/markets/trades",
      result_key    = "trades",
      query         = query,
      creds         = creds,
      authenticated = use_auth
    )
  } else {
    resp    <- kalshi_get("/markets/trades", query = query, creds = creds, authenticated = use_auth)
    records <- resp[["trades"]]
  }

  tbl <- records_to_tibble(records)

  # Parse timestamp
  if ("created_time" %in% names(tbl)) {
    tbl$created_time <- parse_kalshi_ts(tbl$created_time)
  }

  tbl
}


# ---------------------------------------------------------------------------
# Candlesticks
# ---------------------------------------------------------------------------

#' Get candlestick (OHLC) price history for a market
#'
#' Returns OHLC (open/high/low/close) price data for a market at a given
#' resolution. Useful for charting and time-series analysis.
#'
#' For **settled** markets, use [get_historical_candlesticks()] instead —
#' this endpoint only covers active/recent markets.
#'
#' @param series_ticker `character(1)`. The series ticker.
#' @param market_ticker `character(1)`. The specific market ticker.
#' @param start_ts `integer(1)`. Start of the time range as a Unix timestamp
#'   (seconds).
#' @param end_ts `integer(1)`. End of the time range as a Unix timestamp
#'   (seconds).
#' @param period_interval `integer(1)`. Candle width in minutes. Typical
#'   values: `1`, `5`, `60`, `1440` (daily).
#' @param creds A `kalshi_credentials` object or `NULL`.
#'
#' @return A `tibble` with columns: `end_period_ts`, `open`, `high`, `low`,
#'   `close`, `volume`, `open_interest`.
#'
#' @examples
#' \dontrun{
#' # Last 24 hours of 60-min candles
#' now   <- as.integer(Sys.time())
#' start <- now - 86400L
#' get_market_candlesticks("KXHIGHNY", "KXHIGHNY-23NOV07-T60", start, now, 60)
#' }
#'
#' @export
get_market_candlesticks <- function(series_ticker,
                                    market_ticker,
                                    start_ts,
                                    end_ts,
                                    period_interval = 60L,
                                    creds           = NULL) {

  use_auth <- !is.null(creds) || !is.null(get0("creds", envir = .kalshi_env))

  query <- list(
    start_ts        = as.integer(start_ts),
    end_ts          = as.integer(end_ts),
    period_interval = as.integer(period_interval)
  )

  endpoint <- paste0("/series/", series_ticker,
                     "/markets/", market_ticker,
                     "/candlesticks")

  resp    <- kalshi_get(endpoint, query = query, creds = creds, authenticated = use_auth)
  records <- resp[["candlesticks"]]

  tbl <- records_to_tibble(records)

  # Convert timestamp column
  if ("end_period_ts" %in% names(tbl)) {
    tbl$end_period_ts <- as.POSIXct(as.numeric(tbl$end_period_ts), origin = "1970-01-01", tz = "UTC")
  }

  tbl
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Tidy a raw market tibble
#'
#' Converts price columns from cents to dollars, parses timestamp columns,
#' and ensures consistent column types.
#'
#' @param tbl A `tibble` of raw market records.
#' @return Tidied `tibble`.
#' @keywords internal
tidy_market_tibble <- function(tbl) {
  if (nrow(tbl) == 0) return(tbl)

  # Timestamp columns
  ts_cols <- c("close_time", "expiration_time", "open_time",
                "latest_expiration_time", "expected_expiration_time")
  for (col in intersect(ts_cols, names(tbl))) {
    tbl[[col]] <- parse_kalshi_ts(tbl[[col]])
  }

  tbl
}
