# R/utils.R
# ─────────────────────────────────────────────────────────────────────────────
# Shared utility functions used throughout kalshiR.
#
# Exported helpers:
#   cents_to_dollars()   – convert integer cents to numeric dollars
#   parse_kalshi_ts()    – convert Kalshi ISO-8601 strings to POSIXct UTC
#   price_to_prob()      – convert 0-100 cent price to 0-1 probability
#   records_to_tibble()  – safely coerce a list of records to a tibble
#
# Internal helpers:
#   %||%                 – NULL-coalescing operator (re-exported from rlang)
#   .to_kalshi_ms()      – convert timestamp to millisecond string for API
# ─────────────────────────────────────────────────────────────────────────────

# If the left-hand side is NULL, return the right-hand side;
#   otherwise return the left-hand side.
#' @importFrom rlang %||%
#' @export
rlang::`%||%`

# ── Exported helpers ──────────────────────────────────────────────────────────

#' Convert cents to dollars
#'
#' Kalshi returns many monetary values as integer cents. This helper divides
#' by 100 and returns a plain numeric vector.
#'
#' @param x Integer or numeric vector of cent values.
#' @return Numeric vector of dollar values.
#' @export
#' @examples
#' cents_to_dollars(4250L)   # 42.50
#' cents_to_dollars(c(100L, 5000L))
cents_to_dollars <- function(x) {
  if (is.null(x)) return(NA_real_)
  as.numeric(x) / 100
}

#' Parse a Kalshi timestamp string to POSIXct
#'
#' Kalshi timestamps are ISO-8601 strings such as
#' `"2024-11-05T12:00:00Z"` or `"2024-11-05T12:00:00.000Z"`.
#' This function converts them to POSIXct with UTC timezone.
#'
#' Accepts:
#' - Character vectors (single or multiple values)
#' - `NA` / `NULL` (returns `NA` POSIXct)
#' - Already-POSIXct values (passed through unchanged)
#'
#' @param x Character vector of ISO-8601 timestamp strings, or POSIXct.
#' @return POSIXct vector in UTC.
#' @export
#' @examples
#' parse_kalshi_ts("2024-11-05T12:00:00Z")
#' parse_kalshi_ts(NA_character_)
parse_kalshi_ts <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  if (inherits(x, "POSIXct")) return(x)

  # Replace trailing Z with +00:00 for strptime compatibility, then parse
  x_clean <- sub("Z$", "+00:00", as.character(x))
  as.POSIXct(x_clean, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
}

#' Convert a Kalshi cent price to a probability
#'
#' Kalshi "yes" prices range from 1–99 cents and represent the market's
#' implied probability. Divides by 100 to return a 0–1 probability.
#'
#' @param price Integer or numeric; a value in \[1, 99\].
#' @return Numeric in \[0.01, 0.99\].
#' @export
#' @examples
#' price_to_prob(65)   # 0.65
price_to_prob <- function(price) {
  if (is.null(price)) return(NA_real_)
  as.numeric(price) / 100
}

#' Coerce a list of records to a tibble
#'
#' Used internally to turn the list-of-lists that Kalshi returns into a
#' tidy tibble. Handles three edge cases cleanly:
#' - `NULL` input → zero-row tibble
#' - Empty list → zero-row tibble
#' - Single-element list → 1-row tibble (no silent vector recycling)
#'
#' @param records A list of named lists (one per row).
#' @param ... Additional arguments passed to [dplyr::bind_rows()].
#' @return A [tibble::tibble()].
#' @export
records_to_tibble <- function(records) {
  if (length(records) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(
    purrr::map(records, function(r) {
      # Replace NULL fields with NA so bind_rows doesn't drop the column
      r[vapply(r, is.null, logical(1))] <- NA

      # Wrap any field that is a vector/list with length != 1 into a list
      # so it becomes a proper list-column instead of causing recycling errors
      r <- lapply(r, function(field) {
        if (length(field) != 1) {
          list(field)  # wrap in list so it occupies exactly one "cell"
        } else {
          field
        }
      })

      tibble::as_tibble(r)
    })
  )
}
# records_to_tibble <- function(records, ...) {
#   if (is.null(records) || length(records) == 0) {
#     return(tibble::tibble())
#   }
#   # Wrap single records in a list so bind_rows treats them as one row
#   if (is.list(records) && !is.list(records[[1]])) {
#     records <- list(records)
#   }
#   dplyr::bind_rows(
#     purrr::map(records, function(r) {
#       # Replace NULL fields with NA so bind_rows doesn't drop the column
#       r[vapply(r, is.null, logical(1))] <- NA
#       tibble::as_tibble(r)
#     }),
#     ...
#   )
# }

# ── Internal helpers ──────────────────────────────────────────────────────────

#' Convert a timestamp to a Kalshi millisecond string
#'
#' Kalshi time-filter parameters (e.g. `min_ts`, `max_ts`) are expressed as
#' **milliseconds since the Unix epoch** passed as a query-string integer.
#'
#' Accepts:
#' - POSIXct objects
#' - ISO-8601 character strings (parsed via [parse_kalshi_ts()])
#' - Numeric/integer already in milliseconds (passed through)
#' - `NULL` (returns `NULL` so the parameter is simply omitted)
#'
#' @param ts A timestamp in one of the accepted forms, or `NULL`.
#' @return A character string of milliseconds, or `NULL`.
#' @keywords internal
.to_kalshi_ms <- function(ts) {
  if (is.null(ts)) return(NULL)

  if (is.numeric(ts) || is.integer(ts)) {
    # Assume already milliseconds if > 1e10, else seconds — convert
    val <- if (max(abs(ts), na.rm = TRUE) > 1e10) ts else ts * 1000L
    return(as.character(round(val)))
  }

  if (is.character(ts)) {
    # ts <- parse_kalshi_ts(ts)
    ts <- as.POSIXct(ts, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  }

  if (inherits(ts, "POSIXct")) {
    return(as.character(round(as.numeric(ts) * 1000)))
  }

  rlang::abort(
    paste0("Cannot convert object of class '", class(ts)[1], "' to Kalshi ms timestamp."),
    call = NULL
  )
}

#' @import reticulate
#' Access the Kalshi Python module
#' @keywords internal
get_py <- function() {
  get("kalshi_py", envir = .kalshi_py_env)
}

safe_scalar <- function(x, default = NA_real_) {
  if (length(x) == 0 || is.null(x) || is.na(x)) return(default)
  x
}
