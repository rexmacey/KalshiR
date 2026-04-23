# R/historical.R
# Historical data endpoints
#
# Kalshi splits data into "current" (recent) and "historical" (older).
# Use get_historical_cutoff() to find the timestamp boundary, then call
# the appropriate endpoint family.
#
# All functions require authentication.

# ---------------------------------------------------------------------------
# get_historical_cutoff()
# ---------------------------------------------------------------------------

#' Get the historical data cutoff timestamp
#'
#' Kalshi maintains two data stores: a live/recent store and a cold historical
#' store. This endpoint returns the oldest timestamp available in the live
#' store. Data older than the cutoff must be retrieved from the historical
#' endpoints (prefixed `/historical/`).
#'
#' @param creds Credentials object or `NULL` for global.
#'
#' @return A one-row tibble:
#'   \describe{
#'     \item{cutoff_time}{The oldest available timestamp in the live store
#'       (POSIXct UTC). Data before this should use historical endpoints.}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' cutoff <- get_historical_cutoff(creds = creds)
#' # Use historical endpoints if your target date < cutoff$cutoff_time
#' }
get_historical_cutoff <- function(creds = NULL) {
  creds <- creds %||% kalshi_get_auth()
  resp  <- kalshi_get("/historical/cutoff", creds = creds)

  tibble::tibble(
    cutoff_time = parse_kalshi_ts(
      resp[["cutoff_time"]] %||% resp[["cutoff"]] %||% NA_character_
    )
  )
}


# ---------------------------------------------------------------------------
# get_historical_markets()
# ---------------------------------------------------------------------------

#' Get historical market snapshots
#'
#' Returns market metadata snapshots from the historical data store. Useful
#' for reconstructing what markets existed and their parameters at a given
#' time in the past.
#'
#' @param event_ticker  Filter by parent event ticker.
#' @param series_ticker Filter by series ticker.
#' @param status        Filter by market status at the historical time.
#'   One of `"open"`, `"closed"`, `"settled"`.
#' @param min_close_ts  Only return markets that closed after this time
#'   (POSIXct or ISO-8601 string).
#' @param max_close_ts  Only return markets that closed before this time.
#' @param limit         Rows per page (default 100).
#' @param cursor        Pagination cursor.
#' @param all_pages     Fetch all pages automatically.
#' @param creds         Credentials object or `NULL` for global.
#'
#' @return A tibble of historical market records. Columns mirror
#'   [get_markets()] output but reflect the historical snapshot.
#'
#' @export
#' @examples
#' \dontrun{
#' hist_markets <- get_historical_markets(
#'   series_ticker = "INXD",
#'   status        = "settled",
#'   all_pages     = TRUE,
#'   creds         = creds
#' )
#' }
get_historical_markets <- function(event_ticker  = NULL,
                                   series_ticker = NULL,
                                   status        = NULL,
                                   min_close_ts  = NULL,
                                   max_close_ts  = NULL,
                                   limit         = 100L,
                                   cursor        = NULL,
                                   all_pages     = FALSE,
                                   creds         = NULL) {
  creds <- creds %||% kalshi_get_auth()

  params <- list(limit = as.integer(limit))
  if (!is.null(event_ticker))  params$event_ticker  <- event_ticker
  if (!is.null(series_ticker)) params$series_ticker <- series_ticker
  if (!is.null(status))        params$status        <- status
  if (!is.null(min_close_ts))  params$min_close_ts  <- .to_kalshi_ms(min_close_ts)
  if (!is.null(max_close_ts))  params$max_close_ts  <- .to_kalshi_ms(max_close_ts)

  if (all_pages) {
    rows <- kalshi_paginate("/historical/markets", params,
                            list_key = "markets", creds = creds)
  } else {
    if (!is.null(cursor)) params$cursor <- cursor
    resp <- kalshi_get("/historical/markets", params = params, creds = creds)
    rows <- resp[["markets"]] %||% list()
  }

  .parse_historical_markets(rows)
}

#' @keywords internal
.parse_historical_markets <- function(rows) {
  if (length(rows) == 0L) {
    return(.empty_market_tibble())
  }

  tibble::tibble(
    ticker              = purrr::map_chr(rows, "ticker",        .default = NA_character_),
    event_ticker        = purrr::map_chr(rows, "event_ticker",  .default = NA_character_),
    series_ticker       = purrr::map_chr(rows, "series_ticker", .default = NA_character_),
    title               = purrr::map_chr(rows, "title",         .default = NA_character_),
    status              = purrr::map_chr(rows, "status",        .default = NA_character_),
    result              = purrr::map_chr(rows, "result",        .default = NA_character_),
    yes_price           = price_to_prob(
      purrr::map_int(rows, "yes_bid", .default = NA_integer_)),
    no_price            = price_to_prob(
      purrr::map_int(rows, "no_bid",  .default = NA_integer_)),
    open_time           = parse_kalshi_ts(
      purrr::map_chr(rows, "open_time",   .default = NA_character_)),
    close_time          = parse_kalshi_ts(
      purrr::map_chr(rows, "close_time",  .default = NA_character_)),
    expiration_time     = parse_kalshi_ts(
      purrr::map_chr(rows, "expiration_time", .default = NA_character_)),
    volume              = purrr::map_int(rows, "volume",        .default = NA_integer_),
    open_interest       = purrr::map_int(rows, "open_interest", .default = NA_integer_),
    liquidity           = cents_to_dollars(
      purrr::map_int(rows, "liquidity",   .default = 0L))
  )
}

#' @keywords internal
.empty_market_tibble <- function() {
  tibble::tibble(
    ticker          = character(),
    event_ticker    = character(),
    series_ticker   = character(),
    title           = character(),
    status          = character(),
    result          = character(),
    yes_price       = double(),
    no_price        = double(),
    open_time       = .POSIXct(double(), tz = "UTC"),
    close_time      = .POSIXct(double(), tz = "UTC"),
    expiration_time = .POSIXct(double(), tz = "UTC"),
    volume          = integer(),
    open_interest   = integer(),
    liquidity       = double()
  )
}


# ---------------------------------------------------------------------------
# get_historical_candlesticks()
# ---------------------------------------------------------------------------

#' Get historical OHLC candlestick data for a market
#'
#' Returns price candlestick data from the historical store. For recent data
#' use [get_market_candlesticks()] instead.
#'
#' The Kalshi API requires a `series_ticker` AND `ticker` for this endpoint
#' (both are mandatory).
#'
#' @param series_ticker Series ticker (e.g. `"INXD"`). Required.
#' @param ticker        Market ticker (e.g. `"INXD-23DEC29-T4000"`). Required.
#' @param start_ts      Start of the candlestick window (POSIXct or ISO-8601).
#'   Required.
#' @param end_ts        End of the candlestick window. Required.
#' @param period_interval Period length in minutes. Common values: 1, 60, 1440.
#'   Defaults to 60 (1 hour).
#' @param creds         Credentials object or `NULL` for global.
#'
#' @return A tibble with one row per candle:
#'   \describe{
#'     \item{end_period_ts}{Candle close time (POSIXct UTC)}
#'     \item{open_price}{Opening yes-price (0–1)}
#'     \item{high_price}{Highest yes-price (0–1)}
#'     \item{low_price}{Lowest yes-price (0–1)}
#'     \item{close_price}{Closing yes-price (0–1)}
#'     \item{volume}{Contracts traded in period}
#'     \item{open_interest}{Open interest at period close}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' candles <- get_historical_candlesticks(
#'   series_ticker   = "INXD",
#'   ticker          = "INXD-23DEC29-T4000",
#'   start_ts        = as.POSIXct("2023-12-01", tz = "UTC"),
#'   end_ts          = as.POSIXct("2023-12-29", tz = "UTC"),
#'   period_interval = 1440L,
#'   creds           = creds
#' )
#' }
get_historical_candlesticks <- function(series_ticker,
                                        ticker,
                                        start_ts,
                                        end_ts,
                                        period_interval = 60L,
                                        creds           = NULL) {
  creds <- creds %||% kalshi_get_auth()

  # Both series_ticker and ticker are required by the API
  if (missing(series_ticker) || is.null(series_ticker))
    rlang::abort("`series_ticker` is required for historical candlesticks.")
  if (missing(ticker) || is.null(ticker))
    rlang::abort("`ticker` is required for historical candlesticks.")
  if (missing(start_ts) || is.null(start_ts))
    rlang::abort("`start_ts` is required for historical candlesticks.")
  if (missing(end_ts) || is.null(end_ts))
    rlang::abort("`end_ts` is required for historical candlesticks.")

  path <- paste0("/historical/series/", series_ticker,
                 "/markets/", ticker, "/candlesticks")

  params <- list(
    start_ts        = .to_kalshi_ms(start_ts),
    end_ts          = .to_kalshi_ms(end_ts),
    period_interval = as.integer(period_interval)
  )

  resp  <- kalshi_get(path, params = params, creds = creds)
  rows  <- resp[["candlesticks"]] %||% list()

  .parse_candlesticks(rows)
}

#' @keywords internal
.parse_candlesticks <- function(rows) {
  if (length(rows) == 0L) {
    return(tibble::tibble(
      end_period_ts = .POSIXct(double(), tz = "UTC"),
      open_price    = double(),
      high_price    = double(),
      low_price     = double(),
      close_price   = double(),
      volume        = integer(),
      open_interest = integer()
    ))
  }

  # Candles have a nested "price" object: {open, high, low, close}
  .price_field <- function(rows, field) {
    purrr::map_int(rows, function(r) {
      p <- r[["price"]] %||% list()
      p[[field]] %||% NA_integer_
    })
  }

  tibble::tibble(
    end_period_ts = parse_kalshi_ts(
      purrr::map_chr(rows, "end_period_ts",
                     .default = NA_character_)),
    open_price    = price_to_prob(.price_field(rows, "open")),
    high_price    = price_to_prob(.price_field(rows, "high")),
    low_price     = price_to_prob(.price_field(rows, "low")),
    close_price   = price_to_prob(.price_field(rows, "close")),
    volume        = purrr::map_int(rows, "volume",        .default = NA_integer_),
    open_interest = purrr::map_int(rows, "open_interest", .default = NA_integer_)
  )
}


# ---------------------------------------------------------------------------
# get_historical_fills()
# ---------------------------------------------------------------------------

#' Get historical fill (execution) records
#'
#' Like [get_fills()] but queries the cold historical data store. Use
#' [get_historical_cutoff()] to determine which endpoint to use.
#'
#' @param ticker    Optional market ticker filter.
#' @param order_id  Optional order ID filter.
#' @param min_ts    Earliest fill time (POSIXct or ISO-8601).
#' @param max_ts    Latest fill time.
#' @param limit     Rows per page (default 100).
#' @param cursor    Pagination cursor.
#' @param all_pages Fetch all pages automatically.
#' @param creds     Credentials object or `NULL` for global.
#'
#' @return A tibble with the same schema as [get_fills()].
#'
#' @export
#' @examples
#' \dontrun{
#' old_fills <- get_historical_fills(
#'   min_ts    = as.POSIXct("2023-01-01", tz = "UTC"),
#'   max_ts    = as.POSIXct("2023-06-01", tz = "UTC"),
#'   all_pages = TRUE,
#'   creds     = creds
#' )
#' }
get_historical_fills <- function(ticker    = NULL,
                                 order_id  = NULL,
                                 min_ts    = NULL,
                                 max_ts    = NULL,
                                 limit     = 100L,
                                 cursor    = NULL,
                                 all_pages = FALSE,
                                 creds     = NULL) {
  creds <- creds %||% kalshi_get_auth()

  params <- list(limit = as.integer(limit))
  if (!is.null(ticker))   params$ticker   <- ticker
  if (!is.null(order_id)) params$order_id <- order_id
  if (!is.null(min_ts))   params$min_ts   <- .to_kalshi_ms(min_ts)
  if (!is.null(max_ts))   params$max_ts   <- .to_kalshi_ms(max_ts)

  if (all_pages) {
    rows <- kalshi_paginate("/historical/portfolio/fills", params,
                            list_key = "fills", creds = creds)
  } else {
    if (!is.null(cursor)) params$cursor <- cursor
    resp <- kalshi_get("/historical/portfolio/fills", params = params, creds = creds)
    rows <- resp[["fills"]] %||% list()
  }

  # Reuse the same parser as live fills — same schema
  .parse_fills(rows)
}


# ---------------------------------------------------------------------------
# get_historical_orders()
# ---------------------------------------------------------------------------

#' Get historical order records
#'
#' Like [get_orders()] but queries the historical data store.
#'
#' @param ticker       Optional market ticker filter.
#' @param event_ticker Optional event ticker filter.
#' @param min_ts       Earliest order creation time.
#' @param max_ts       Latest order creation time.
#' @param status       Order status filter: `"resting"`, `"canceled"`,
#'   `"executed"`, `"pending"`.
#' @param limit        Rows per page (default 100).
#' @param cursor       Pagination cursor.
#' @param all_pages    Fetch all pages automatically.
#' @param creds        Credentials object or `NULL` for global.
#'
#' @return A tibble with the same schema as [get_orders()].
#'
#' @export
#' @examples
#' \dontrun{
#' old_orders <- get_historical_orders(
#'   status    = "executed",
#'   min_ts    = as.POSIXct("2023-01-01", tz = "UTC"),
#'   all_pages = TRUE,
#'   creds     = creds
#' )
#' }
get_historical_orders <- function(ticker       = NULL,
                                  event_ticker = NULL,
                                  min_ts       = NULL,
                                  max_ts       = NULL,
                                  status       = NULL,
                                  limit        = 100L,
                                  cursor       = NULL,
                                  all_pages    = FALSE,
                                  creds        = NULL) {
  creds <- creds %||% kalshi_get_auth()

  params <- list(limit = as.integer(limit))
  if (!is.null(ticker))       params$ticker       <- ticker
  if (!is.null(event_ticker)) params$event_ticker <- event_ticker
  if (!is.null(min_ts))       params$min_ts       <- .to_kalshi_ms(min_ts)
  if (!is.null(max_ts))       params$max_ts       <- .to_kalshi_ms(max_ts)
  if (!is.null(status))       params$status       <- status

  if (all_pages) {
    rows <- kalshi_paginate("/historical/portfolio/orders", params,
                            list_key = "orders", creds = creds)
  } else {
    if (!is.null(cursor)) params$cursor <- cursor
    resp <- kalshi_get("/historical/portfolio/orders",
                       params = params, creds = creds)
    rows <- resp[["orders"]] %||% list()
  }

  # Reuse the order parser from orders.R
  .parse_orders(rows)
}


# ---------------------------------------------------------------------------
# get_historical_trades()
# ---------------------------------------------------------------------------

#' Get historical public trade data for a market
#'
#' Returns the public trade tape from the historical store for a given
#' market. Each row is an individual anonymous trade (not your own — for
#' your fills see [get_historical_fills()]).
#'
#' @param ticker   Market ticker. Required.
#' @param min_ts   Earliest trade time (POSIXct or ISO-8601).
#' @param max_ts   Latest trade time.
#' @param limit    Rows per page (default 100).
#' @param cursor   Pagination cursor.
#' @param all_pages Fetch all pages automatically.
#' @param creds    Credentials object or `NULL` for global.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{trade_id}{Unique trade identifier}
#'     \item{ticker}{Market ticker}
#'     \item{side}{`"yes"` or `"no"`}
#'     \item{count}{Contracts traded}
#'     \item{yes_price}{Yes-side execution price (0–1)}
#'     \item{no_price}{No-side execution price (0–1)}
#'     \item{created_time}{Trade timestamp (POSIXct UTC)}
#'     \item{taker_side}{`"yes"` or `"no"` — which side was the taker}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' trades <- get_historical_trades(
#'   ticker    = "INXD-23DEC29-T4000",
#'   min_ts    = as.POSIXct("2023-12-01", tz = "UTC"),
#'   all_pages = TRUE,
#'   creds     = creds
#' )
#' }
get_historical_trades <- function(ticker,
                                  min_ts    = NULL,
                                  max_ts    = NULL,
                                  limit     = 100L,
                                  cursor    = NULL,
                                  all_pages = FALSE,
                                  creds     = NULL) {
  if (missing(ticker) || is.null(ticker))
    rlang::abort("`ticker` is required for get_historical_trades().")

  creds <- creds %||% kalshi_get_auth()

  params <- list(ticker = ticker, limit = as.integer(limit))
  if (!is.null(min_ts)) params$min_ts <- .to_kalshi_ms(min_ts)
  if (!is.null(max_ts)) params$max_ts <- .to_kalshi_ms(max_ts)

  if (all_pages) {
    rows <- kalshi_paginate("/historical/markets/trades", params,
                            list_key = "trades", creds = creds)
  } else {
    if (!is.null(cursor)) params$cursor <- cursor
    resp <- kalshi_get("/historical/markets/trades",
                       params = params, creds = creds)
    rows <- resp[["trades"]] %||% list()
  }

  .parse_historical_trades(rows)
}

#' @keywords internal
.parse_historical_trades <- function(rows) {
  if (length(rows) == 0L) {
    return(tibble::tibble(
      trade_id     = character(),
      ticker       = character(),
      side         = character(),
      count        = integer(),
      yes_price    = double(),
      no_price     = double(),
      created_time = .POSIXct(double(), tz = "UTC"),
      taker_side   = character()
    ))
  }

  tibble::tibble(
    trade_id     = purrr::map_chr(rows, "trade_id",    .default = NA_character_),
    ticker       = purrr::map_chr(rows, "ticker",      .default = NA_character_),
    side         = purrr::map_chr(rows, "side",        .default = NA_character_),
    count        = purrr::map_int(rows, "count",       .default = NA_integer_),
    yes_price    = price_to_prob(
      purrr::map_int(rows, "yes_price", .default = NA_integer_)),
    no_price     = price_to_prob(
      purrr::map_int(rows, "no_price",  .default = NA_integer_)),
    created_time = parse_kalshi_ts(
      purrr::map_chr(rows, "created_time", .default = NA_character_)),
    taker_side   = purrr::map_chr(rows, "taker_side",  .default = NA_character_)
  )
}
