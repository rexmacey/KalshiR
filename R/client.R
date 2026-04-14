#' @title Kalshi HTTP Client
#'
#' @description
#' Low-level HTTP helpers used by all endpoint functions. These wrap
#' `httr2` requests with Kalshi authentication headers, consistent error
#' handling, and a pagination utility for cursor-based endpoints.
#'
#' You typically do not call these directly — use the higher-level functions
#' in `markets.R`, `portfolio.R`, etc. They are exported so that advanced
#' users can make custom requests if needed.
#'
#' @name client
NULL


# ---------------------------------------------------------------------------
# Base URL for unauthenticated (public) requests. Authenticated requests
# use the URL stored in the credentials object.
# ---------------------------------------------------------------------------
.KALSHI_PUBLIC_URL <- "https://api.elections.kalshi.com/trade-api/v2"


#' Make an authenticated or unauthenticated GET request to the Kalshi API
#'
#' Sends a GET request to the Kalshi REST API. If `creds` is provided (or
#' set globally via [kalshi_set_auth()]), the request is signed and
#' authenticated. Otherwise the public base URL is used and no auth headers
#' are added (suitable for public market data endpoints).
#'
#' @param endpoint `character(1)`. The endpoint path relative to the base
#'   URL, e.g. `"/markets"` or `"/portfolio/balance"`.
#' @param query `list` or `NULL`. Named list of query parameters appended to
#'   the URL. `NULL` values are automatically dropped.
#' @param creds A `kalshi_credentials` object from [kalshi_auth()], or
#'   `NULL` to use global credentials (set via [kalshi_set_auth()]) or fall
#'   back to unauthenticated access.
#' @param authenticated `logical(1)`. If `TRUE` (default when creds are
#'   available), attach auth headers. Set to `FALSE` to force an
#'   unauthenticated request even if credentials are available.
#'
#' @return The parsed JSON response as an R list.
#'
#' @keywords internal
kalshi_get <- function(endpoint,
                       query         = NULL,
                       creds         = NULL,
                       authenticated = TRUE) {

  # -- Resolve credentials ---------------------------------------------------
  if (authenticated && is.null(creds)) {
    creds <- tryCatch(kalshi_get_auth(), error = function(e) NULL)
  }

  use_auth <- authenticated && !is.null(creds)
  base_url <- if (use_auth) creds$base_url else .KALSHI_PUBLIC_URL

  # The signing path must include /trade-api/v2 prefix
  sign_path <- paste0("/trade-api/v2", endpoint)

  # -- Build request ---------------------------------------------------------
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent("kalshiR/0.1.0") |>
    httr2::req_headers("Content-Type" = "application/json")

  # Drop NULL query parameters then append
  if (!is.null(query)) {
    query <- Filter(Negate(is.null), query)
    if (length(query) > 0) {
      req <- httr2::req_url_query(req, !!!query)
    }
  }

  # Add auth headers if authenticated
  if (use_auth) {
    auth_hdrs <- kalshi_auth_headers(creds, "GET", sign_path)
    req <- httr2::req_headers(req, !!!auth_hdrs)
  }

  # -- Send and handle errors ------------------------------------------------
  resp <- req |>
    httr2::req_error(is_error = \(r) FALSE) |>  # handle errors manually
    httr2::req_perform()

  kalshi_handle_response(resp)
}


#' Make an authenticated POST request to the Kalshi API
#'
#' Sends a signed POST request with a JSON body. Always requires credentials.
#'
#' @param endpoint `character(1)`. Endpoint path relative to the base URL.
#' @param body `list`. Request body. Will be serialised to JSON.
#' @param creds A `kalshi_credentials` object, or `NULL` to use global creds.
#'
#' @return The parsed JSON response as an R list.
#'
#' @keywords internal
kalshi_post <- function(endpoint, body = list(), creds = NULL) {

  if (is.null(creds)) creds <- kalshi_get_auth()
  sign_path <- paste0("/trade-api/v2", endpoint)

  auth_hdrs <- kalshi_auth_headers(creds, "POST", sign_path)

  resp <- httr2::request(creds$base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent("kalshiR/0.1.0") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      !!!auth_hdrs
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  kalshi_handle_response(resp)
}


#' Make an authenticated DELETE request to the Kalshi API
#'
#' Sends a signed DELETE request. Used for cancelling orders.
#'
#' @param endpoint `character(1)`. Endpoint path relative to the base URL.
#' @param creds A `kalshi_credentials` object, or `NULL` to use global creds.
#'
#' @return The parsed JSON response as an R list.
#'
#' @keywords internal
kalshi_delete <- function(endpoint, creds = NULL) {

  if (is.null(creds)) creds <- kalshi_get_auth()
  sign_path <- paste0("/trade-api/v2", endpoint)

  auth_hdrs <- kalshi_auth_headers(creds, "DELETE", sign_path)

  resp <- httr2::request(creds$base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent("kalshiR/0.1.0") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      !!!auth_hdrs
    ) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()

  kalshi_handle_response(resp)
}


#' Handle a Kalshi API response
#'
#' Checks the HTTP status code, raises informative errors for 4xx/5xx
#' responses, and parses the JSON body for successful responses.
#'
#' @param resp An `httr2_response` object.
#'
#' @return Parsed JSON as an R list.
#'
#' @keywords internal
kalshi_handle_response <- function(resp) {

  status <- httr2::resp_status(resp)

  # Parse body regardless — error responses also contain useful JSON messages
  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) list()
  )

  # -- Handle errors ---------------------------------------------------------
  if (status == 401) {
    cli::cli_abort(c(
      "Authentication failed (HTTP 401).",
      "i" = "Check your API Key ID and private key file.",
      "i" = "Ensure the timestamp is correct (system clock skew can cause this)."
    ))
  }

  if (status == 403) {
    cli::cli_abort(c(
      "Forbidden (HTTP 403).",
      "i" = "Your API key may not have permission for this endpoint.",
      "x" = kalshi_error_message(body)
    ))
  }

  if (status == 404) {
    cli::cli_abort(c(
      "Resource not found (HTTP 404).",
      "i" = "Check the ticker or ID you provided.",
      "x" = kalshi_error_message(body)
    ))
  }

  if (status == 429) {
    cli::cli_abort(c(
      "Rate limit exceeded (HTTP 429).",
      "i" = "Slow down requests or implement backoff."
    ))
  }

  if (status >= 400) {
    cli::cli_abort(c(
      "Kalshi API error (HTTP {status}).",
      "x" = kalshi_error_message(body)
    ))
  }

  body
}


#' Extract a human-readable error message from a Kalshi error response body
#'
#' @param body Parsed JSON list from a failed response.
#' @return `character(1)` error message, or a generic fallback.
#' @keywords internal
kalshi_error_message <- function(body) {
  # Kalshi error bodies typically have a "message" or "error" field
  msg <- body[["message"]] %||% body[["error"]] %||% body[["detail"]]
  if (!is.null(msg)) as.character(msg) else "No error message returned."
}


#' Fetch all pages of a paginated Kalshi endpoint
#'
#' Kalshi uses cursor-based pagination. Responses include a `cursor` field;
#' pass it as the `cursor` query parameter in the next request to get the
#' next page. This helper loops until no more pages are available and
#' combines all results.
#'
#' @param endpoint `character(1)`. API endpoint path.
#' @param result_key `character(1)`. The key in the response list that holds
#'   the array of results, e.g. `"markets"` or `"trades"`.
#' @param query `list`. Base query parameters (excluding `cursor`).
#' @param creds A `kalshi_credentials` object or `NULL` for global/public.
#' @param authenticated `logical(1)`. Whether to sign the request.
#' @param max_pages `integer(1)`. Safety cap on the number of pages fetched.
#'   Default `100L`. Set to `Inf` to fetch everything.
#'
#' @return A list combining all result items across pages.
#'
#' @keywords internal
kalshi_paginate <- function(endpoint,
                            result_key,
                            query         = list(),
                            creds         = NULL,
                            authenticated = TRUE,
                            max_pages     = 100L) {

  all_results <- list()
  cursor      <- NULL
  page        <- 0L

  repeat {
    page <- page + 1L
    if (page > max_pages) {
      cli::cli_warn("Reached max_pages limit ({max_pages}). Results may be incomplete.")
      break
    }

    # Add cursor to query if we have one
    this_query <- query
    if (!is.null(cursor)) this_query[["cursor"]] <- cursor

    resp <- kalshi_get(
      endpoint      = endpoint,
      query         = this_query,
      creds         = creds,
      authenticated = authenticated
    )

    # Collect results
    page_results <- resp[[result_key]]
    if (!is.null(page_results) && length(page_results) > 0) {
      all_results <- c(all_results, page_results)
    }

    # Check for next cursor
    cursor <- resp[["cursor"]]
    if (is.null(cursor) || nchar(cursor) == 0) break
  }

  all_results
}


# ---------------------------------------------------------------------------
# Utility: NULL-coalescing operator (like %||% in rlang)
# ---------------------------------------------------------------------------
`%||%` <- function(x, y) if (!is.null(x)) x else y
