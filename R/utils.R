#' @title Kalshi Utility Functions
#'
#' @description
#' Internal helpers for tidying API responses into tibbles, converting
#' Kalshi's integer cent prices to dollars, parsing timestamps, and
#' safely extracting nested list fields.
#'
#' @name utils
NULL


#' Convert a list of records to a tibble
#'
#' Takes a list of named lists (as returned by Kalshi's JSON responses after
#' parsing) and coerces it into a flat tibble. Nested list columns are kept
#' as list-columns; scalar fields become regular columns.
#'
#' @param records `list`. A list of named lists, one per row.
#' @param ... Additional arguments passed to [tibble::as_tibble()].
#'
#' @return A `tibble`. Returns an empty tibble if `records` is empty or NULL.
#'
#' @keywords internal
records_to_tibble <- function(records, ...) {
  if (is.null(records) || length(records) == 0) {
    return(tibble::tibble())
  }
  # Each element of records is a named list representing one row.
  # We bind them together row-wise via dplyr::bind_rows which handles
  # mismatched columns gracefully (fills with NA).
  dplyr::bind_rows(
    purrr::map(records, ~ tibble::as_tibble_row(flatten_record(.x)))
  )
}


#' Flatten a single API record for tabular display
#'
#' Recursively flattens a named list one level deep. Nested lists that are
#' themselves records (named lists) are kept as list-columns. Vectors of
#' length > 1 become list-columns.
#'
#' @param record A named list.
#' @return A named list suitable for [tibble::as_tibble_row()].
#' @keywords internal
flatten_record <- function(record) {
  purrr::imap(record, function(val, nm) {
    if (is.null(val)) {
      NA
    } else if (is.list(val)) {
      # Keep as list (will become a list-column)
      list(val)
    } else if (length(val) > 1) {
      list(val)
    } else {
      val
    }
  })
}


#' Convert Kalshi cent integer to dollar numeric
#'
#' Kalshi's API returns monetary values as integers in cents (e.g. a balance
#' of `$12.50` is returned as `1250`). This function divides by 100 and
#' returns a numeric dollar amount.
#'
#' @param cents `numeric`. Integer cent value(s) from the Kalshi API.
#'
#' @return `numeric`. Dollar value(s), rounded to 2 decimal places.
#'
#' @examples
#' cents_to_dollars(1250)  # returns 12.50
#' cents_to_dollars(c(100, 250, 9999))  # returns c(1.00, 2.50, 99.99)
#'
#' @export
cents_to_dollars <- function(cents) {
  round(as.numeric(cents) / 100, 2)
}


#' Parse a Kalshi ISO 8601 timestamp to a POSIXct datetime
#'
#' Kalshi returns timestamps as ISO 8601 strings, e.g.
#' `"2023-11-07T05:31:56Z"`. This function converts them to R `POSIXct`
#' objects in UTC.
#'
#' @param ts `character`. One or more ISO 8601 timestamp strings.
#'
#' @return `POSIXct` in UTC timezone.
#'
#' @examples
#' parse_kalshi_ts("2023-11-07T05:31:56Z")
#'
#' @export
parse_kalshi_ts <- function(ts) {
  as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}


#' Safely extract a field from a nested list
#'
#' Returns `NULL` (or a default) rather than throwing an error if the field
#' path does not exist.
#'
#' @param x A list.
#' @param ... Field names as unquoted symbols or strings forming the path.
#' @param .default Value to return if the path is not found. Default `NA`.
#'
#' @return The value at the specified path, or `.default`.
#'
#' @keywords internal
safe_pluck <- function(x, ..., .default = NA) {
  purrr::pluck(x, ..., .default = .default)
}


#' Convert a Kalshi yes-price (0–100 cents) to a probability
#'
#' Kalshi prices are quoted in cents from 0 to 100, representing the implied
#' probability of the YES outcome. This function converts to a 0–1 probability.
#'
#' @param price_cents `numeric`. Price in cents (0–100).
#'
#' @return `numeric`. Probability between 0 and 1.
#'
#' @examples
#' price_to_prob(65)   # returns 0.65
#' price_to_prob(100)  # returns 1.00
#'
#' @export
price_to_prob <- function(price_cents) {
  as.numeric(price_cents) / 100
}
