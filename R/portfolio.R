# R/portfolio.R
# Portfolio endpoints: balance, positions, fills, settlements
#
# All functions require authentication.
# Prices returned from the API as integer cents are converted to dollars.
# Timestamps are converted to POSIXct UTC.

# ---------------------------------------------------------------------------
# get_balance()
# ---------------------------------------------------------------------------

#' Get your current Kalshi account balance
#'
#' Returns a one-row tibble with available balance, portfolio value, and
#' bonus balance (all converted from cents to dollars).
#'
#' @param creds A credentials object from [kalshi_auth()]. If `NULL`, uses
#'   the globally stored credentials set by [kalshi_set_auth()].
#'
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{balance}{Cash available to trade (dollars)}
#'     \item{portfolio_value}{Current value of open positions (dollars)}
#'     \item{bonus_balance}{Promotional / bonus balance (dollars)}
#'     \item{total_value}{`balance + portfolio_value` (dollars)}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' creds <- kalshi_auth(key_id = Sys.getenv("KALSHI_KEY_ID"),
#'                      private_key = Sys.getenv("KALSHI_PRIVATE_KEY"))
#' get_balance(creds = creds)
#' }
get_balance <- function(creds = NULL) {
  creds <- creds %||% kalshi_get_auth()

  resp <- kalshi_get("/portfolio/balance", creds = creds)

  bal <- resp[["balance"]]

  tibble::tibble(
    balance        = cents_to_dollars(bal[["available_balance_cents"]] %||%
                                        bal[["balance"]] %||% 0L),
    portfolio_value = cents_to_dollars(bal[["portfolio_value_cents"]] %||%
                                         bal[["portfolio_value"]] %||% 0L),
    bonus_balance  = cents_to_dollars(bal[["bonus_balance_cents"]] %||%
                                        bal[["bonus_balance"]] %||% 0L),
    total_value    = balance + portfolio_value
  )
}


# ---------------------------------------------------------------------------
# get_positions()
# ---------------------------------------------------------------------------

#' Get your current open positions
#'
#' Returns a tibble of all open market positions for the authenticated user.
#' Use `all_pages = TRUE` to retrieve every position automatically.
#'
#' @param limit   Maximum rows per page (default 100, max 1000).
#' @param cursor  Pagination cursor from a previous call. Leave `NULL` to
#'   start from the beginning.
#' @param all_pages If `TRUE`, automatically fetches all pages and returns
#'   a single combined tibble. Ignores `cursor` when `TRUE`.
#' @param creds A credentials object from [kalshi_auth()]. If `NULL`, uses
#'   the globally stored credentials.
#'
#' @return A tibble with one row per open position. Columns include:
#'   \describe{
#'     \item{ticker}{Market ticker symbol}
#'     \item{event_ticker}{Parent event ticker}
#'     \item{side}{`"yes"` or `"no"`}
#'     \item{position}{Net contract count (positive = long)}
#'     \item{market_exposure_cents}{Dollar value at risk}
#'     \item{realized_pnl}{Realised profit/loss in dollars}
#'     \item{unrealized_pnl}{Unrealised profit/loss in dollars}
#'     \item{total_traded}{Total contracts traded in this market}
#'     \item{fees_paid}{Total fees paid in dollars}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' positions <- get_positions(all_pages = TRUE, creds = creds)
#' }
get_positions <- function(limit     = 100L,
                          cursor    = NULL,
                          all_pages = FALSE,
                          creds     = NULL) {
  creds <- creds %||% kalshi_get_auth()

  params <- list(limit = as.integer(limit))

  if (all_pages) {
    rows <- kalshi_paginate(
      path       = "/portfolio/positions",
      params     = params,
      list_key   = "market_positions",
      creds      = creds
    )
  } else {
    if (!is.null(cursor)) params$cursor <- cursor
    resp <- kalshi_get("/portfolio/positions", query = params, creds = creds)
    rows <- resp[["market_positions"]] %||% list()
  }

  .parse_positions(rows)
}

#' @keywords internal
.parse_positions <- function(rows) {
  if (length(rows) == 0L) {
    return(tibble::tibble(
      ticker               = character(),
      event_ticker         = character(),
      side                 = character(),
      position             = integer(),
      market_exposure      = double(),
      realized_pnl         = double(),
      unrealized_pnl       = double(),
      total_traded         = integer(),
      fees_paid            = double()
    ))
  }

  tibble::tibble(
    ticker           = purrr::map_chr(rows, "ticker",           .default = NA_character_),
    event_ticker     = purrr::map_chr(rows, "event_ticker",     .default = NA_character_),
    side             = purrr::map_chr(rows, "side",             .default = NA_character_),
    position         = purrr::map_int(rows, "position",         .default = NA_integer_),
    # API returns these as cent integers; convert
    market_exposure  = cents_to_dollars(
      purrr::map_int(rows, "market_exposure_cents", .default = 0L)),
    realized_pnl     = cents_to_dollars(
      purrr::map_int(rows, "realized_pnl",   .default = 0L)),
    unrealized_pnl   = cents_to_dollars(
      purrr::map_int(rows, "unrealized_pnl", .default = 0L)),
    total_traded     = purrr::map_int(rows, "total_traded",     .default = NA_integer_),
    fees_paid        = cents_to_dollars(
      purrr::map_int(rows, "fees_paid",      .default = 0L))
  )
}


# ---------------------------------------------------------------------------
# get_fills()
# ---------------------------------------------------------------------------

#' Get your trade fills (executions)
#'
#' Returns a tibble of fill records — individual trade executions against
#' your orders. Fills represent the "done" portion of an order.
#'
#' @param ticker  Optional market ticker to filter fills.
#' @param order_id Optional order ID to filter fills.
#' @param min_ts  Optional earliest fill timestamp (POSIXct or character
#'   ISO-8601). Converted to milliseconds internally.
#' @param max_ts  Optional latest fill timestamp.
#' @param limit   Rows per page (default 100).
#' @param cursor  Pagination cursor.
#' @param all_pages Fetch all pages automatically.
#' @param creds   Credentials object or `NULL` for global.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{fill_id}{Unique fill identifier}
#'     \item{order_id}{The order this fill belongs to}
#'     \item{ticker}{Market ticker}
#'     \item{side}{`"yes"` or `"no"`}
#'     \item{action}{`"buy"` or `"sell"`}
#'     \item{count}{Contracts filled}
#'     \item{yes_price}{Yes-side price in dollars (0–1)}
#'     \item{no_price}{No-side price in dollars (0–1)}
#'     \item{is_taker}{`TRUE` if your order was the liquidity taker}
#'     \item{created_time}{Fill timestamp (POSIXct UTC)}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' fills <- get_fills(ticker = "INXD-23DEC29-T4000", all_pages = TRUE,
#'                    creds = creds)
#' }
get_fills <- function(ticker    = NULL,
                      order_id  = NULL,
                      min_ts    = NULL,
                      max_ts    = NULL,
                      limit     = 100L,
                      cursor    = NULL,
                      all_pages = FALSE,
                      creds     = NULL) {
  creds <- creds %||% kalshi_get_auth()

  params <- list(limit = as.integer(limit))
  if (!is.null(ticker))   params$ticker    <- ticker
  if (!is.null(order_id)) params$order_id  <- order_id
  if (!is.null(min_ts))   params$min_ts    <- .to_kalshi_ms(min_ts)
  if (!is.null(max_ts))   params$max_ts    <- .to_kalshi_ms(max_ts)

  if (all_pages) {
    rows <- kalshi_paginate("/portfolio/fills", params,
                            list_key = "fills", creds = creds)
  } else {
    if (!is.null(cursor)) params$cursor <- cursor
    resp <- kalshi_get("/portfolio/fills", query = params, creds = creds)
    rows <- resp[["fills"]] %||% list()
  }

  .parse_fills(rows)
}

#' @keywords internal
.parse_fills <- function(rows) {
  if (length(rows) == 0L) {
    return(tibble::tibble(
      fill_id      = character(),
      order_id     = character(),
      ticker       = character(),
      side         = character(),
      action       = character(),
      count        = integer(),
      yes_price    = double(),
      no_price     = double(),
      is_taker     = logical(),
      created_time = .POSIXct(double(), tz = "UTC")
    ))
  }

  tibble::tibble(
    fill_id      = purrr::map_chr(rows, "fill_id",   .default = NA_character_),
    order_id     = purrr::map_chr(rows, "order_id",  .default = NA_character_),
    ticker       = purrr::map_chr(rows, "ticker",    .default = NA_character_),
    side         = purrr::map_chr(rows, "side",      .default = NA_character_),
    action       = purrr::map_chr(rows, "action",    .default = NA_character_),
    count        = purrr::map_int(rows, "count",     .default = NA_integer_),
    # prices are integer cents [1,99] → convert to 0-1 probability
    yes_price    = price_to_prob(purrr::map_int(rows, "yes_price", .default = NA_integer_)),
    no_price     = price_to_prob(purrr::map_int(rows, "no_price",  .default = NA_integer_)),
    is_taker     = purrr::map_lgl(rows, "is_taker",  .default = NA),
    created_time = parse_kalshi_ts(
      purrr::map_chr(rows, "created_time", .default = NA_character_))
  )
}


# ---------------------------------------------------------------------------
# get_settlements()
# ---------------------------------------------------------------------------

#' Get your market settlement history
#'
#' Returns a tibble of settled (resolved) positions, showing the final
#' payout for each resolved market you participated in.
#'
#' @param limit   Rows per page (default 100).
#' @param cursor  Pagination cursor.
#' @param all_pages Fetch all pages automatically.
#' @param creds   Credentials object or `NULL` for global.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{market_result}{`"yes"` or `"no"` — the winning side}
#'     \item{ticker}{Market ticker}
#'     \item{settled_time}{When the market settled (POSIXct UTC)}
#'     \item{revenue}{Payout received in dollars}
#'     \item{realized_pnl}{Profit or loss in dollars}
#'     \item{no_count}{No contracts held at settlement}
#'     \item{yes_count}{Yes contracts held at settlement}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' settlements <- get_settlements(all_pages = TRUE, creds = creds)
#' dplyr::filter(settlements, realized_pnl > 0)
#' }
get_settlements <- function(limit     = 100L,
                            cursor    = NULL,
                            all_pages = FALSE,
                            creds     = NULL) {
  creds <- creds %||% kalshi_get_auth()

  params <- list(limit = as.integer(limit))

  if (all_pages) {
    rows <- kalshi_paginate("/portfolio/settlements", params,
                            list_key = "settlements", creds = creds)
  } else {
    if (!is.null(cursor)) params$cursor <- cursor
    resp <- kalshi_get("/portfolio/settlements", query = params, creds = creds)
    rows <- resp[["settlements"]] %||% list()
  }

  .parse_settlements(rows)
}

#' @keywords internal
.parse_settlements <- function(rows) {
  if (length(rows) == 0L) {
    return(tibble::tibble(
      market_result = character(),
      ticker        = character(),
      settled_time  = .POSIXct(double(), tz = "UTC"),
      revenue       = double(),
      realized_pnl  = double(),
      no_count      = integer(),
      yes_count     = integer()
    ))
  }

  tibble::tibble(
    market_result = purrr::map_chr(rows, "market_result", .default = NA_character_),
    ticker        = purrr::map_chr(rows, "ticker",        .default = NA_character_),
    settled_time  = parse_kalshi_ts(
      purrr::map_chr(rows, "settled_time", .default = NA_character_)),
    revenue       = cents_to_dollars(
      purrr::map_int(rows, "revenue",      .default = 0L)),
    realized_pnl  = cents_to_dollars(
      purrr::map_int(rows, "realized_pnl", .default = 0L)),
    no_count      = purrr::map_int(rows, "no_count",      .default = NA_integer_),
    yes_count     = purrr::map_int(rows, "yes_count",     .default = NA_integer_)
  )
}


# ---------------------------------------------------------------------------
# Internal helper: convert R timestamp → Kalshi millisecond integer string
# ---------------------------------------------------------------------------

#' @keywords internal
.to_kalshi_ms <- function(ts) {
  if (inherits(ts, c("POSIXct", "POSIXlt"))) {
    as.character(round(as.numeric(ts) * 1000))
  } else {
    # Assume ISO-8601 string; parse first
    parsed <- as.POSIXct(ts, tz = "UTC")
    as.character(round(as.numeric(parsed) * 1000))
  }
}
