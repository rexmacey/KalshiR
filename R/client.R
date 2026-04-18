# R/client.R
# Low-level HTTP client for kalshiR
#
# Provides typed wrappers around httr2 request machinery:
#   kalshi_get()    — authenticated GET
#   kalshi_post()   — authenticated POST (JSON body)
#   kalshi_delete() — authenticated DELETE (optional JSON body)
#   kalshi_paginate() — generic cursor-based paginator
#   kalshi_handle_response() — error extraction + parsing

# ---------------------------------------------------------------------------
# Public request functions
# ---------------------------------------------------------------------------

#' Perform an authenticated GET request
#'
#' @param endpoint Character. API path beginning with `/`, e.g. `"/markets"`.
#' @param params Named list of query parameters. NULL values are silently
#'   dropped by httr2.
#' @param creds Credentials object from [kalshi_auth()]. If NULL, falls back
#'   to the globally stored credentials.
#'
#' @return Parsed JSON response as an R list.
#' @keywords internal
kalshi_get <- function(endpoint, params = list(), creds = NULL) {
  creds <- creds %||% kalshi_get_auth()
  req   <- .build_request(endpoint, method = "GET", creds = creds)
  req   <- httr2::req_url_query(req, !!!params)
  resp  <- httr2::req_perform(req)
  kalshi_handle_response(resp)
}

#' Perform an authenticated POST request with a JSON body
#'
#' @param endpoint Character. API path.
#' @param body Named list. Will be serialised to JSON.
#' @param creds Credentials object or NULL.
#'
#' @return Parsed JSON response as an R list.
#' @keywords internal
kalshi_post <- function(endpoint, body = list(), creds = NULL) {
  creds <- creds %||% kalshi_get_auth()
  req   <- .build_request(endpoint, method = "POST", creds = creds)
  req   <- httr2::req_body_json(req, body)
  resp  <- httr2::req_perform(req)
  kalshi_handle_response(resp)
}

#' Perform an authenticated DELETE request
#'
#' @param endpoint Character. API path.
#' @param body Named list or NULL. If non-NULL, serialised as JSON body
#'   (used for batch cancel endpoint).
#' @param creds Credentials object or NULL.
#'
#' @return Parsed JSON response as an R list.
#' @keywords internal
kalshi_delete <- function(endpoint, body = NULL, creds = NULL) {
  creds <- creds %||% kalshi_get_auth()
  req   <- .build_request(endpoint, method = "DELETE", creds = creds)
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }
  resp  <- httr2::req_perform(req)
  kalshi_handle_response(resp)
}

# ---------------------------------------------------------------------------
# Pagination helper
# ---------------------------------------------------------------------------

#' Fetch all pages of a cursor-paginated endpoint
#'
#' Repeatedly calls `kalshi_get()` following the `cursor` field until no
#' further pages are available. Use this inside list functions when
#' `all_pages = TRUE`.
#'
#' @param endpoint Character. API path.
#' @param params Named list of initial query parameters (without `cursor`).
#' @param list_key Character. The key in the response JSON that contains the
#'   list of records, e.g. `"markets"` or `"orders"`.
#' @param creds Credentials object or NULL.
#'
#' @return A combined list of all records across pages.
#' @keywords internal
kalshi_paginate <- function(endpoint, params = list(), list_key, creds = NULL) {
  creds    <- creds %||% kalshi_get_auth()
  all_rows <- list()
  cursor   <- NULL

  repeat {
    page_params          <- params
    page_params[["cursor"]] <- cursor

    resp   <- kalshi_get(endpoint, params = page_params, creds = creds)
    rows   <- resp[[list_key]] %||% list()
    all_rows <- c(all_rows, rows)

    cursor <- resp[["cursor"]]

    # Kalshi signals end-of-pages with NULL or empty-string cursor
    if (is.null(cursor) || identical(cursor, "")) break
  }

  all_rows
}

# ---------------------------------------------------------------------------
# Response handler
# ---------------------------------------------------------------------------

#' Parse an httr2 response and surface API errors
#'
#' Converts the HTTP response to an R list. If the response indicates an
#' error (4xx or 5xx), extracts the Kalshi error message and throws an
#' informative R error via [cli::cli_abort()].
#'
#' @param resp An `httr2_response` object.
#'
#' @return Parsed JSON as a named R list.
#' @keywords internal
kalshi_handle_response <- function(resp) {
  status <- httr2::resp_status(resp)

  if (status >= 400) {
    # Try to parse error body for a helpful message
    body <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) list()
    )

    msg  <- body[["message"]] %||% body[["error"]] %||% "Unknown API error"
    code <- body[["code"]]    %||% ""

    cli::cli_abort(c(
      "Kalshi API error {status}: {msg}",
      if (nzchar(code)) "i" = "Error code: {code}"
    ))
  }

  # 204 No Content — return empty list rather than erroring on empty body
  if (status == 204) return(list())

  httr2::resp_body_json(resp)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Build a signed httr2 request for a given endpoint and HTTP method.
.build_request <- function(endpoint, method, creds) {
  #base_url <- if (creds$demo) KALSHI_DEMO_URL else KALSHI_PROD_URL
  base_url <- creds$base_url
  url      <- paste0(base_url, endpoint)

  # Strip the /trade-api/v2 prefix for the signature path
  sig_path <- paste0("/trade-api/v2", endpoint)

  headers  <- kalshi_auth_headers(
    creds    = creds,
    method   = method,
    path = sig_path
  )

  req <- httr2::request(url)
  req <- httr2::req_method(req, method)
  req <- httr2::req_headers(
    req,
    "KALSHI-ACCESS-KEY"       = headers[["KALSHI-ACCESS-KEY"]],
    "KALSHI-ACCESS-TIMESTAMP" = headers[["KALSHI-ACCESS-TIMESTAMP"]],
    "KALSHI-ACCESS-SIGNATURE" = headers[["KALSHI-ACCESS-SIGNATURE"]],
    "Content-Type"            = "application/json",
    "Accept"                  = "application/json"
  )

  # Retry on 429 (rate-limit) and transient 5xx once before giving up
  req <- httr2::req_retry(req,
                          max_tries = 3L,
                          is_transient = function(resp) {
                            httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
                          }
  )

  req
}
