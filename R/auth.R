#' @title Kalshi API Authentication
#'
#' @description
#' Functions for loading your Kalshi private key and signing authenticated
#' requests using RSA-PSS with SHA-256, as required by the Kalshi API.
#'
#' The typical workflow is:
#' 1. Call `kalshi_auth()` once at the top of your session to create a
#'    credentials object.
#' 2. Pass that object to any function that calls authenticated endpoints
#'    (portfolio, orders, etc.).
#'
#' @name auth
NULL

# ---------------------------------------------------------------------------
# Internal package environment — stores the active credentials so users
# don't have to thread them through every function call if they prefer a
# global approach.
# ---------------------------------------------------------------------------
.kalshi_env <- new.env(parent = emptyenv())


#' Create a Kalshi credentials object
#'
#' Loads your private key from disk and bundles it with your API Key ID into
#' a credentials object. Pass this object to authenticated API functions, or
#' call [kalshi_set_auth()] to set it globally for the session.
#'
#' @param api_key_id `character(1)`. Your API Key ID, displayed when you create
#'   a key in Kalshi's account settings. Looks like a UUID, e.g.
#'   `"a952bcbe-ec3b-4b5b-b8f9-11dae589608c"`.
#' @param private_key_path `character(1)`. Path to the `.key` PEM file
#'   downloaded when you created your API key.
#' @param env `character(1)`. Which Kalshi environment to target.
#'   `"production"` (default) or `"demo"`. The demo environment is useful
#'   for testing orders without real money.
#'
#' @return A list of class `"kalshi_credentials"` containing:
#'   \describe{
#'     \item{`api_key_id`}{Your API Key ID string.}
#'     \item{`private_key`}{The loaded `openssl` RSA private key object.}
#'     \item{`base_url`}{The resolved base URL for the chosen environment.}
#'     \item{`env`}{The environment name (`"production"` or `"demo"`).}
#'   }
#'
#' @examples
#' \dontrun{
#' creds <- kalshi_auth(
#'   api_key_id       = "a952bcbe-ec3b-4b5b-b8f9-11dae589608c",
#'   private_key_path = "~/.kalshi/kalshi.key"
#' )
#' # Set globally so you don't need to pass creds every time
#' kalshi_set_auth(creds)
#' }
#'
#' @export
kalshi_auth <- function(api_key_id,
                        private_key_path,
                        env = c("production", "demo")) {

  env <- match.arg(env)

  # -- Validate inputs -------------------------------------------------------
  if (!is.character(api_key_id) || nchar(trimws(api_key_id)) == 0) {
    cli::cli_abort("{.arg api_key_id} must be a non-empty string.")
  }

  private_key_path <- normalizePath(private_key_path, mustWork = FALSE)
  if (!file.exists(private_key_path)) {
    cli::cli_abort(c(
      "Private key file not found.",
      "x" = "Path: {.path {private_key_path}}"
    ))
  }

  # -- Load the PEM private key ----------------------------------------------
  py <- get_py()
  private_key <- py$load_private_key(private_key_path)
  # private_key <- tryCatch(
  #   openssl::read_key(private_key_path),
  #   error = function(e) {
  #     cli::cli_abort(c(
  #       "Failed to load private key from {.path {private_key_path}}.",
  #       "x" = conditionMessage(e)
  #     ))
  #   }
  # )

  # -- Resolve base URL ------------------------------------------------------
  base_url <- switch(env,
    production = .kalshi_config$prod_url,
    demo       = .kalshi_config$demo_url
  )

  structure(
    list(
      api_key_id  = api_key_id,
      private_key = private_key,
      base_url    = base_url,
      env         = env
    ),
    class = "kalshi_credentials"
  )
}


#' Set global Kalshi credentials for the session
#'
#' Stores a credentials object in the package environment so that API
#' functions can find it automatically without requiring you to pass `creds`
#' as an argument every time.
#'
#' @param creds A `kalshi_credentials` object created by [kalshi_auth()].
#'
#' @return Invisibly returns `creds`.
#'
#' @examples
#' \dontrun{
#' creds <- kalshi_auth("my-key-id", "~/.kalshi/kalshi.key")
#' kalshi_set_auth(creds)
#' # Now all API calls will use these credentials automatically
#' get_balance()
#' }
#'
#' @export
kalshi_set_auth <- function(creds) {
  if (!inherits(creds, "kalshi_credentials")) {
    cli::cli_abort("{.arg creds} must be a {.cls kalshi_credentials} object from {.fn kalshi_auth}.")
  }
  assign("creds", creds, envir = .kalshi_env)
  cli::cli_alert_success(
    "Kalshi credentials set for {.strong {creds$env}} environment."
  )
  invisible(creds)
}


#' Retrieve the active global credentials
#'
#' Internal helper used by API functions to find credentials. Looks for
#' credentials set via [kalshi_set_auth()]. Raises a helpful error if none
#' are found.
#'
#' @return A `kalshi_credentials` object.
#'
#' @keywords internal
kalshi_get_auth <- function() {
  creds <- get0("creds", envir = .kalshi_env)
  if (is.null(creds)) {
    cli::cli_abort(c(
      "No Kalshi credentials found.",
      "i" = "Call {.fn kalshi_auth} then {.fn kalshi_set_auth} first.",
      "i" = "Or pass a {.arg creds} argument directly to the API function."
    ))
  }
  creds
}


#' Sign a Kalshi API request
#'
#' Produces the RSA-PSS SHA-256 signature required for authenticated Kalshi
#' API calls. The message that is signed is the concatenation of the
#' millisecond timestamp, the HTTP method (uppercase), and the URL path
#' **without** query parameters.
#'
#' This function is called internally by [kalshi_get()] and [kalshi_post()];
#' you typically do not need to call it directly.
#'
#' @param private_key An RSA private key object as returned by
#'   `openssl::read_key()`.
#' @param timestamp_ms `character(1)`. Current time in milliseconds since
#'   epoch, as a string (e.g. `"1703123456789"`).
#' @param method `character(1)`. HTTP method in uppercase: `"GET"`, `"POST"`,
#'   or `"DELETE"`.
#' @param path `character(1)`. The full URL path **including** `/trade-api/v2`
#'   prefix but **excluding** any query string. Example:
#'   `"/trade-api/v2/portfolio/balance"`.
#'
#' @return `character(1)`. Base64-encoded RSA-PSS signature string.
#'
#' @keywords internal
kalshi_sign <- function(private_key, timestamp_ms, method, path) {

  # Strip query parameters — Kalshi signs only the path portion
  path_clean <- strsplit(path, "\\?")[[1]][1]

  # Build the message string: timestamp + METHOD + /path
  message_str <- paste0(timestamp_ms, method, path_clean)

  # Sign with RSA-PSS (SHA-256, salt length = digest length = 32 bytes)
  signature_raw <- openssl::signature_create(
    data      = charToRaw(message_str),
    hash      = openssl::sha256,
    key       = private_key,
    # RSA-PSS padding: openssl package uses rsa_pss_sign under the hood
    # when the key is RSA and we request SHA-256
  )

  # Kalshi expects a standard base64-encoded string (not URL-safe)
  openssl::base64_encode(signature_raw)
}


#' Build authentication headers for a Kalshi request
#'
#' Internal helper that assembles the three required authentication headers:
#' `KALSHI-ACCESS-KEY`, `KALSHI-ACCESS-TIMESTAMP`, and
#' `KALSHI-ACCESS-SIGNATURE`.
#'
#' @param creds A `kalshi_credentials` object.
#' @param method `character(1)`. HTTP method uppercase (`"GET"`, `"POST"`,
#'   `"DELETE"`).
#' @param path `character(1)`. Full path from root including `/trade-api/v2`
#'   prefix, without query string.
#'
#' @return A named `character` vector suitable for use as httr2 headers.
#'
#' @keywords internal
kalshi_auth_headers <- function(creds, method, path) {

  timestamp_ms <- as.character(round(as.numeric(Sys.time()) * 1000))

  signature <- kalshi_sign(
    private_key  = creds$private_key,
    timestamp_ms = timestamp_ms,
    method       = method,
    path         = path
  )

  c(
    "KALSHI-ACCESS-KEY"       = creds$api_key_id,
    "KALSHI-ACCESS-TIMESTAMP" = timestamp_ms,
    "KALSHI-ACCESS-SIGNATURE" = signature
  )
}


#' @export
print.kalshi_credentials <- function(x, ...) {
  cli::cli_h2("Kalshi API Credentials")
  cli::cli_bullets(c(
    "*" = "Environment : {.strong {x$env}}",
    "*" = "Base URL    : {.url {x$base_url}}",
    "*" = "API Key ID  : {.val {x$api_key_id}}",
    "*" = "Private key : {.emph <loaded>}"
  ))
  invisible(x)
}
