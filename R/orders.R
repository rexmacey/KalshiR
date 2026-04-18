# R/orders.R
# Order management endpoints for kalshiR
#
# Covers the full order lifecycle:
#   create → amend/decrease → cancel
# Plus batch operations and order queries.
#
# All prices supplied by the user should be in CENTS (0–99) to match the
# Kalshi API convention. The returned tibbles convert prices to dollars for
# readability.

#' Create a new order
#'
#' Submits a new buy or sell order for a market. Returns the created order.
#'
#' @param ticker Character. Market ticker symbol (required).
#' @param side Character. `"yes"` or `"no"` (required).
#' @param action Character. `"buy"` or `"sell"` (required).
#' @param count Integer. Number of contracts (required).
#' @param type Character. Order type: `"limit"` or `"market"`. Default
#'   `"limit"`.
#' @param yes_price Integer. Limit price in cents for a YES contract (0–99).
#'   Required for limit orders. For a NO order, supply `no_price` instead.
#' @param no_price Integer. Limit price in cents for a NO contract (0–99).
#' @param expiration_ts POSIXct or integer. Order expiration time (epoch ms).
#'   NULL means GTC (Good Till Cancelled).
#' @param client_order_id Character. Optional idempotency key (UUID). If
#'   supplied and the same key is resubmitted, the API returns the original
#'   order without creating a duplicate.
#' @param creds Credentials object or NULL.
#'
#' @return A one-row tibble describing the created order. Prices in dollars,
#'   timestamps as POSIXct UTC.
#'
#' @details
#' For limit orders you must supply either `yes_price` OR `no_price` (they
#' must sum to 100). The API will reject orders where both are supplied and
#' inconsistent.
#'
#' Kalshi uses cents (1–99) for limit prices; do **not** pass dollar values
#' here.
#'
#' @examples
#' \dontrun{
#' # Buy 10 YES contracts at 45 cents each
#' create_order(
#'   ticker    = "INXD-24DEC31-T5000",
#'   side      = "yes",
#'   action    = "buy",
#'   count     = 10L,
#'   yes_price = 45L
#' )
#'
#' # Market buy — no price required
#' create_order(
#'   ticker = "INXD-24DEC31-T5000",
#'   side   = "yes",
#'   action = "buy",
#'   count  = 5L,
#'   type   = "market"
#' )
#' }
#'
#' @export
create_order <- function(ticker,
                         side,
                         action,
                         count,
                         type              = "limit",
                         yes_price         = NULL,
                         no_price          = NULL,
                         expiration_ts     = NULL,
                         client_order_id   = NULL,
                         creds             = NULL) {
  if (is.null(ticker) || !nzchar(ticker)) {
    cli::cli_abort("{.arg ticker} must be a non-empty string.")
  }
  creds <- creds %||% kalshi_get_auth()

  # --- Input validation -------------------------------------------------------
  side   <- match.arg(side,   c("yes", "no"))
  action <- match.arg(action, c("buy", "sell"))
  type   <- match.arg(type,   c("limit", "market"))

  if (type == "limit" && is.null(yes_price) && is.null(no_price)) {
    cli::cli_abort(
      "Limit orders require {.arg yes_price} or {.arg no_price} in cents."
    )
  }

  if (!is.null(yes_price) && (yes_price < 1 || yes_price > 99)) {
    cli::cli_abort("yes_price {.arg yes_price} must be between 1 and 99 cents.")
  }
  if (!is.null(no_price) && (no_price < 1 || no_price > 99)) {
    cli::cli_abort("no_price {.arg no_price} must be between 1 and 99 cents.")
  }
  if (count <= 0) {
    cli::cli_abort("Count {count} must be greater than zero.")
  }
  # --- Build request body -----------------------------------------------------
  body <- .build_order_body(
    ticker          = ticker,
    side            = side,
    action          = action,
    type            = type,
    count           = count,
    yes_price       = yes_price,
    no_price        = no_price,
    expiration_ts   = expiration_ts,
    client_order_id = client_order_id
  )

  resp <- kalshi_post("/portfolio/orders", body = body, creds = creds)
  order <- resp[["order"]] %||% resp  # API wraps in "order" key
  .parse_orders(list(order))
}

# R/build_order_body.R

.build_order_body <- function(ticker,
                              side,
                              action,
                              type,
                              count,
                              yes_price       = NULL,
                              no_price        = NULL,
                              expiration_ts   = NULL,
                              client_order_id = NULL) {
  body <- list(
    ticker          = ticker,
    side            = side,
    action          = action,
    count           = as.integer(count),
    type            = type,
    yes_price       = if (!is.null(yes_price)) as.integer(yes_price) else NULL,
    no_price        = if (!is.null(no_price))  as.integer(no_price)  else NULL,
    expiration_ts   = .as_epoch_ms(expiration_ts),
    client_order_id = client_order_id
  )
  purrr::compact(body)
}

#' Cancel an order
#'
#' Cancels an open order by its order ID. Returns the cancelled order.
#'
#' @param order_id Character. The order ID to cancel (required).
#' @param creds Credentials object or NULL.
#'
#' @return A one-row tibble describing the cancelled order.
#'
#' @examples
#' \dontrun{
#' cancel_order("ord_01HXXXXXXXXXXXXXXXXXXXXXX")
#' }
#'
#' @export
cancel_order <- function(order_id, creds = NULL) {
  if (is.null(order_id) || !nzchar(order_id)) {
    cli::cli_abort("order_id {order_id} must be non-empty string.")
  }
  creds <- creds %||% kalshi_get_auth()

  resp  <- kalshi_delete(
    paste0("/portfolio/orders/", order_id),
    creds = creds
  )
  order <- resp[["order"]] %||% resp
  .parse_orders(list(order))
}

#' Get orders
#'
#' Returns orders for the authenticated user. Supports filtering by market,
#' status, and time range.
#'
#' @param ticker Character. Filter to a specific market ticker.
#' @param event_ticker Character. Filter to markets within a specific event.
#' @param status Character. Filter by order status: `"resting"`,
#'   `"canceled"`, `"executed"`, `"pending"`. NULL returns all.
#' @param min_ts POSIXct or integer. Earliest order creation timestamp.
#' @param max_ts POSIXct or integer. Latest order creation timestamp.
#' @param limit Integer. Results per page (max 1000). Default 100.
#' @param cursor Character. Pagination cursor.
#' @param all_pages Logical. Fetch all pages? Default FALSE.
#' @param creds Credentials object or NULL.
#'
#' @return A tibble of orders with columns:
#'   \describe{
#'     \item{order_id}{Unique order identifier.}
#'     \item{ticker}{Market ticker symbol.}
#'     \item{event_ticker}{Parent event ticker.}
#'     \item{side}{`"yes"` or `"no"`.}
#'     \item{action}{`"buy"` or `"sell"`.}
#'     \item{type}{`"limit"` or `"market"`.}
#'     \item{status}{Current order status.}
#'     \item{count}{Original number of contracts.}
#'     \item{remaining_count}{Unfilled contracts.}
#'     \item{yes_price}{Limit price in dollars.}
#'     \item{no_price}{Implied NO price in dollars.}
#'     \item{created_time}{POSIXct UTC — order creation time.}
#'     \item{expiration_time}{POSIXct UTC — expiration time (if set).}
#'   }
#'
#' @examples
#' \dontrun{
#' # All resting orders
#' get_orders(status = "resting")
#'
#' # All orders for a market
#' get_orders(ticker = "INXD-24DEC31-T5000", all_pages = TRUE)
#' }
#'
#' @export
get_orders <- function(ticker       = NULL,
                       event_ticker = NULL,
                       status       = NULL,
                       min_ts       = NULL,
                       max_ts       = NULL,
                       limit        = 100L,
                       cursor       = NULL,
                       all_pages    = FALSE,
                       creds        = NULL) {
  creds <- creds %||% kalshi_get_auth()

  params <- list(
    ticker       = ticker,
    event_ticker = event_ticker,
    status       = status,
    min_ts       = .as_epoch_ms(min_ts),
    max_ts       = .as_epoch_ms(max_ts),
    limit        = as.integer(limit),
    cursor       = cursor
  )

  if (all_pages) {
    rows <- kalshi_paginate(
      endpoint = "/portfolio/orders",
      params   = params,
      list_key = "orders",
      creds    = creds
    )
    return(.parse_orders(rows))
  }

  resp <- kalshi_get("/portfolio/orders", params = params, creds = creds)
  rows <- resp[["orders"]] %||% list()
  .parse_orders(rows)
}

#' Get a single order
#'
#' Returns the full details for a specific order.
#'
#' @param order_id Character. The order ID to retrieve (required).
#' @param creds Credentials object or NULL.
#'
#' @return A one-row tibble describing the order.
#'
#' @examples
#' \dontrun{
#' get_order("ord_01HXXXXXXXXXXXXXXXXXXXXXX")
#' }
#'
#' @export
get_order <- function(order_id, creds = NULL) {
  if (is.null(order_id) || !nzchar(order_id)) {
    cli::cli_abort("order_id {.arg order_id} must be a non-empty string.")
  }
  creds <- creds %||% kalshi_get_auth()

  resp  <- kalshi_get(
    paste0("/portfolio/orders/", order_id),
    creds = creds
  )
  order <- resp[["order"]] %||% resp
  .parse_orders(list(order))
}

#' Amend an existing order
#'
#' Replaces the price and/or quantity of a resting limit order.
#' The order must be in `"resting"` status.
#'
#' @param order_id Character. Order ID to amend (required).
#' @param count Integer. New total count for the order. Supply the full
#'   intended quantity, not the delta.
#' @param yes_price Integer. New YES limit price in cents (1–99).
#' @param no_price Integer. New NO limit price in cents (1–99).
#' @param creds Credentials object or NULL.
#'
#' @return A one-row tibble describing the amended order.
#'
#' @examples
#' \dontrun{
#' # Change price to 50 cents
#' amend_order("ord_01HXXXXXXXXXXXXXXXXXXXXXX", yes_price = 50L)
#' }
#'
#' @export
amend_order <- function(order_id,
                        count     = NULL,
                        yes_price = NULL,
                        no_price  = NULL,
                        creds     = NULL) {
  creds <- creds %||% kalshi_get_auth()

  if (is.null(order_id) || !nzchar(order_id)) {
    cli::cli_abort("order_id {order_id} must be non-empty string.")
  }
  if (is.null(count) && is.null(yes_price) && is.null(no_price)) {
    cli::cli_abort(
      "Supply at least one of {.arg count}, {.arg yes_price}, or {.arg no_price}."
    )
  }

  body <- purrr::compact(list(
    count     = if (!is.null(count))     as.integer(count)     else NULL,
    yes_price = if (!is.null(yes_price)) as.integer(yes_price) else NULL,
    no_price  = if (!is.null(no_price))  as.integer(no_price)  else NULL
  ))

  resp  <- kalshi_post(
    paste0("/portfolio/orders/", order_id, "/amend"),
    body  = body,
    creds = creds
  )
  order <- resp[["order"]] %||% resp
  .parse_orders(list(order))
}

#' Decrease an order's quantity
#'
#' Reduces the remaining count of a resting order without fully cancelling
#' it. The `reduce_by` count must be less than the current remaining count
#' (to fully cancel, use [cancel_order()]).
#'
#' @param order_id Character. Order ID (required).
#' @param reduce_by Integer. Number of contracts to remove from the order.
#' @param creds Credentials object or NULL.
#'
#' @return A one-row tibble describing the updated order.
#'
#' @examples
#' \dontrun{
#' decrease_order("ord_01HXXXXXXXXXXXXXXXXXXXXXX", reduce_by = 3L)
#' }
#'
#' @export
decrease_order <- function(order_id, reduce_by, creds = NULL) {
  creds <- creds %||% kalshi_get_auth()

  if (is.null(order_id) || !nzchar(order_id)) {
    cli::cli_abort("order_id {order_id} must be non-empty string.")
  }
  if (reduce_by <= 0) {
    cli::cli_abort("reduce_by {reduce_by} must be greater than zero.")
  }

  body <- list(reduce_by = as.integer(reduce_by))

  resp  <- kalshi_post(
    paste0("/portfolio/orders/", order_id, "/decrease"),
    body  = body,
    creds = creds
  )
  order <- resp[["order"]] %||% resp
  .parse_orders(list(order))
}

#' Batch create orders
#'
#' Submits up to 20 orders in a single API call. More efficient than
#' calling [create_order()] in a loop when placing many orders.
#'
#' @param orders A list of order specification lists. Each element should
#'   be a named list with the same fields accepted by [create_order()]:
#'   `ticker`, `side`, `action`, `count`, `type`, `yes_price`/`no_price`,
#'   and optionally `expiration_ts` and `client_order_id`.
#' @param creds Credentials object or NULL.
#'
#' @return A tibble with one row per submitted order.
#'
#' @details
#' The Kalshi API processes batch orders atomically per order — a failure
#' on one order does not roll back the others. Check the `status` column of
#' the returned tibble to identify any orders that were rejected.
#'
#' @examples
#' \dontrun{
#' orders <- list(
#'   list(
#'     ticker    = "INXD-24DEC31-T5000",
#'     side      = "yes",
#'     action    = "buy",
#'     count     = 5L,
#'     yes_price = 45L
#'   ),
#'   list(
#'     ticker    = "INXD-24DEC31-T4800",
#'     side      = "no",
#'     action    = "buy",
#'     count     = 3L,
#'     no_price  = 60L
#'   )
#' )
#' batch_create_orders(orders)
#' }
#'
#' @export
batch_create_orders <- function(orders, creds = NULL) {
  creds <- creds %||% kalshi_get_auth()

  if (!is.list(orders) || length(orders) == 0) {
    cli::cli_abort("{.arg orders} must be a non-empty list of order specs.")
  }
  if (length(orders) > 20) {
    cli::cli_abort(
      "Kalshi batch endpoint accepts at most 20 orders; got {length(orders)}."
    )
  }

  # Normalise each order spec (coerce prices to integer, drop NULLs)
  orders <- lapply(orders, function(o) {
    o[["count"]]     <- as.integer(o[["count"]])
    o[["yes_price"]] <- if (!is.null(o[["yes_price"]])) as.integer(o[["yes_price"]]) else NULL
    o[["no_price"]]  <- if (!is.null(o[["no_price"]]))  as.integer(o[["no_price"]])  else NULL
    purrr::compact(o)
  })

  body <- list(orders = orders)
  resp <- kalshi_post("/portfolio/orders/batched", body = body, creds = creds)
  rows <- resp[["orders"]] %||% list()
  .parse_orders(rows)
}

#' Batch cancel orders
#'
#' Cancels multiple resting orders in a single API call.
#'
#' @param order_ids Character vector. Order IDs to cancel (max 20).
#' @param creds Credentials object or NULL.
#'
#' @return A tibble with one row per cancelled order.
#'
#' @examples
#' \dontrun{
#' batch_cancel_orders(c(
#'   "ord_01HXXXXXXXXXXXXXXXXXXXXXX",
#'   "ord_01HYYYYYYYYYYYYYYYYYYYY"
#' ))
#' }
#'
#' @export
batch_cancel_orders <- function(order_ids, creds = NULL) {
  creds <- creds %||% kalshi_get_auth()

  if (length(order_ids) == 0) {
    cli::cli_abort("{.arg order_ids} must be a non-empty character vector.")
  }
  if (length(order_ids) > 20) {
    cli::cli_abort(
      "Kalshi batch endpoint accepts at most 20 order IDs; got {length(order_ids)}."
    )
  }

  body <- list(ids = as.list(order_ids))
  resp <- kalshi_delete("/portfolio/orders/batched", body = body, creds = creds)
  rows <- resp[["orders"]] %||% list()
  .parse_orders(rows)
}

# ---------------------------------------------------------------------------
# Internal parser (shared with historical.R — identical implementation)
# ---------------------------------------------------------------------------

#' @keywords internal
.parse_orders <- function(rows) {
  if (length(rows) == 0) return(tibble::tibble())

  tbl <- records_to_tibble(rows)

  ts_cols <- intersect(
    c("created_time", "close_time", "expiration_time", "last_updated_time"),
    names(tbl)
  )
  for (col in ts_cols) {
    tbl[[col]] <- parse_kalshi_ts(tbl[[col]])
  }

  price_cols <- intersect(c("yes_price", "no_price"), names(tbl))
  for (col in price_cols) {
    tbl[[col]] <- cents_to_dollars(tbl[[col]])
  }

  tbl
}

# Epoch conversion helper (mirrors historical.R so orders.R is self-contained)
.as_epoch_ms <- function(x) {
  if (is.null(x))            return(NULL)
  if (inherits(x, "POSIXt")) return(as.integer(as.numeric(x) * 1000))
  as.integer(x)
}
