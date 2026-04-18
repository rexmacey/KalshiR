# R/exchange.R
# ============================================================
# Exchange-level endpoints: status, schedule, announcements.
#
# These are read-only, unauthenticated-friendly endpoints that
# describe the state of the Kalshi exchange itself rather than
# any specific market or portfolio.
# ============================================================

# ── get_exchange_status ────────────────────────────────────────────────────────

#' Get the current exchange status
#'
#' Returns a one-row tibble indicating whether the Kalshi exchange is currently
#' accepting orders, the trading status, and any maintenance information.
#'
#' @param creds A credentials object from [kalshi_auth()]. If `NULL` (default),
#'   the globally stored credentials set by [kalshi_set_auth()] are used.
#'
#' @return A one-row tibble with columns:
#'   \describe{
#'     \item{trading_active}{Logical. `TRUE` if the exchange is open for trading.}
#'     \item{exchange_active}{Logical. `TRUE` if the exchange is online at all.}
#'     \item{condition}{Character. Human-readable status string (e.g. `"open"`).}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' kalshi_set_auth(kalshi_auth(
#'   key_id      = Sys.getenv("KALSHI_KEY_ID"),
#'   private_key = Sys.getenv("KALSHI_KEY_PATH")
#' ))
#'
#' status <- get_exchange_status()
#' if (status$trading_active) {
#'   message("Exchange is open for trading.")
#' } else {
#'   message("Exchange is currently closed: ", status$condition)
#' }
#' }
get_exchange_status <- function(creds = NULL) {
  creds <- creds %||% kalshi_get_auth()

  resp <- kalshi_get("/exchange/status", creds = creds)

  # The API returns an object with exchange_active, trading_active, condition
  # (field names confirmed against Kalshi v2 API docs)
  tibble::tibble(
    trading_active  = isTRUE(resp[["trading_active"]]),
    exchange_active = isTRUE(resp[["exchange_active"]]),
    condition       = resp[["condition"]] %||% NA_character_
  )
}


# ── get_exchange_schedule ──────────────────────────────────────────────────────

#' Get the exchange trading schedule
#'
#' Returns the standard weekly trading hours for the Kalshi exchange.
#' Each row represents one scheduled trading window.
#'
#' @inheritParams get_exchange_status
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{day}{Character. Day of week (e.g. `"monday"`).}
#'     \item{open_time}{Character. UTC open time (HH:MM format).}
#'     \item{close_time}{Character. UTC close time (HH:MM format).}
#'   }
#'   Returns an empty tibble if no schedule data is available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' schedule <- get_exchange_schedule()
#' print(schedule)
#' # # A tibble: 5 × 3
#' #   day       open_time close_time
#' #   <chr>     <chr>     <chr>
#' # 1 monday    09:30     16:00
#' # ...
#' }
get_exchange_schedule <- function(creds = NULL) {
  creds <- creds %||% kalshi_get_auth()

  resp         <- kalshi_get("/exchange/schedule", creds = creds)
  schedule_obj <- resp[["schedule"]] %||% resp
  hours        <- schedule_obj[["standard_hours"]] %||%
    schedule_obj[["trading_hours"]] %||%
    list()

  empty <- tibble::tibble(
    day        = character(),
    open_time  = character(),
    close_time = character()
  )

  if (length(hours) == 0L) return(empty)

  day_cols <- c("monday", "tuesday", "wednesday", "thursday",
                "friday", "saturday", "sunday")

  # Each element of `hours` is a period block with:
  #   - start_time / end_time  (validity window, ISO strings)
  #   - <day_name>             (list of {open_time, close_time} windows)
  #
  # We iterate over every period block, then every day, then every
  # open/close window within that day.

  rows <- lapply(hours, function(period) {
    present_days <- intersect(day_cols, names(period))
    if (length(present_days) == 0L) return(NULL)

    day_rows <- lapply(present_days, function(day_name) {
      windows <- period[[day_name]]

      # Each window should be a list with open_time / close_time
      win_rows <- lapply(windows, function(w) {
        tibble::tibble(
          day        = day_name,
          open_time  = as.character(w[["open_time"]]  %||% NA_character_),
          close_time = as.character(w[["close_time"]] %||% NA_character_)
        )
      })

      dplyr::bind_rows(win_rows)
    })

    dplyr::bind_rows(day_rows)
  })

  result <- dplyr::bind_rows(rows)

  if (nrow(result) == 0L) return(empty)

  # Return rows in canonical weekday order
  result |>
    dplyr::mutate(day = factor(.data$day, levels = day_cols)) |>
    dplyr::arrange(.data$day) |>
    dplyr::mutate(day = as.character(.data$day))
}


# ── get_exchange_announcements ─────────────────────────────────────────────────

#' Get exchange announcements
#'
#' Retrieves platform-level announcements from Kalshi (e.g. scheduled
#' maintenance, new feature notices, policy updates).
#'
#' @inheritParams get_exchange_status
#' @param limit Integer. Maximum number of announcements to return. Default 50.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Character. Unique announcement ID.}
#'     \item{title}{Character. Short title.}
#'     \item{content}{Character. Full announcement text.}
#'     \item{created_time}{POSIXct (UTC). When the announcement was posted.}
#'     \item{type}{Character. Category (e.g. `"maintenance"`, `"general"`).}
#'   }
#'   Returns an empty tibble if there are no announcements.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' announcements <- get_exchange_announcements(limit = 10L)
#' print(announcements)
#' }
get_exchange_announcements <- function(limit = 50L, creds = NULL) {
  creds <- creds %||% kalshi_get_auth()

  stopifnot(is.numeric(limit), limit >= 1L)

  resp <- kalshi_get(
    "/exchange/announcements",
    params = list(limit = as.integer(limit)),
    creds = creds
  )

  items <- resp[["announcements"]] %||% list()

  if (length(items) == 0L) {
    return(tibble::tibble(
      id           = character(),
      title        = character(),
      content      = character(),
      created_time = as.POSIXct(character(), tz = "UTC"),
      type         = character()
    ))
  }

  tbl <- records_to_tibble(items)

  # Parse timestamps if present
  if ("created_time" %in% names(tbl)) {
    tbl$created_time <- parse_kalshi_ts(tbl$created_time)
  }

  tbl
}

kalshi_is_open <- function(creds = kalshi_creds(), check_time = NULL) {

  resp <- kalshi_get("/exchange/schedule", creds = creds)

  # Use provided time or current UTC time
  if (is.null(check_time)) {
    check_time <- Sys.time()
  }
  attr(check_time, "tzone") <- "UTC"

  now_date   <- as.Date(check_time, tz = "UTC")
  now_time   <- format(check_time, "%H:%M", tz = "UTC")
  now_dow    <- tolower(weekdays(now_date))  # "monday", "tuesday", etc.

  # Check maintenance windows first
  maint_windows <- resp$schedule$maintenance_windows
  if (length(maint_windows) > 0) {
    for (w in maint_windows) {
      maint_start <- as.POSIXct(w$start_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      maint_end   <- as.POSIXct(w$end_time,   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      if (check_time >= maint_start && check_time < maint_end) {
        return(list(
          is_open          = FALSE,
          reason           = "scheduled maintenance",
          maintenance_end  = maint_end,
          check_time_utc   = check_time
        ))
      }
    }
  }

  # Find the applicable standard_hours block (by start_time / end_time)
  std_hours <- resp$schedule$standard_hours
  active_block <- NULL

  for (block in std_hours) {
    block_start <- as.POSIXct(block$start_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    block_end   <- as.POSIXct(block$end_time,   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (check_time >= block_start && check_time < block_end) {
      active_block <- block
      break
    }
  }

  if (is.null(active_block)) {
    return(list(
      is_open        = FALSE,
      reason         = "no active schedule block found",
      check_time_utc = check_time
    ))
  }

  # Get today's windows from the active block
  day_windows <- active_block[[now_dow]]

  if (is.null(day_windows) || length(day_windows) == 0) {
    return(list(
      is_open        = FALSE,
      reason         = paste("no hours defined for", now_dow),
      check_time_utc = check_time
    ))
  }

  # Check if current time falls within any open window
  # Windows with close_time == "00:00" mean "open until midnight" (end of day)
  in_window <- FALSE

  for (win in day_windows) {
    open_time  <- win$open_time   # e.g. "00:00" or "08:00"
    close_time <- win$close_time  # e.g. "07:45" or "00:00"

    # "00:00" as close_time means end of day (midnight)
    if (close_time == "00:00") {
      close_time <- "23:59"
    }

    if (now_time >= open_time && now_time < close_time) {
      in_window <- TRUE
      break
    }
  }

  list(
    is_open        = in_window,
    reason         = if (in_window) "within trading hours" else "outside trading hours",
    day            = now_dow,
    current_time   = now_time,
    windows        = lapply(day_windows, function(w) {
      paste0(w$open_time, " - ", ifelse(w$close_time == "00:00", "24:00", w$close_time))
    }),
    check_time_utc = check_time
  )
}
