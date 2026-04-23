library(httr)
library(openssl)
library(base64enc)

# Configuration
API_KEY_ID <- Sys.getenv("KALSHI_API_KEY")
PRIVATE_KEY_PATH <- Sys.getenv("KALSHI_KEY_PATH")
BASE_URL <- "https://demo-api.kalshi.co/trade-api/v2"
# BASE_URL <- "https://api.elections.kalshi.com/trade-api/v2"

#' Load a PEM-encoded RSA private key from disk
#'
#' @param key_path Path to the .key file
#' @return An openssl private key object
load_private_key <- function(key_path) {
  key_data <- readBin(key_path, what = "raw", n = file.info(key_path)$size)
  private_key <- openssl::read_key(key_data, password = NULL)
  return(private_key)
}

#' Create a base64-encoded RSA-PSS signature for the request
#'
#' @param private_key An openssl private key object
#' @param timestamp  Millisecond Unix timestamp as a string
#' @param method     HTTP method (e.g. "GET", "POST")
#' @param path       URL path without query parameters (e.g. "/trade-api/v2/portfolio/balance")
#' @return Base64-encoded signature string
create_signature <- function(private_key, timestamp, method, path) {
  # Strip any query parameters before signing (keep only the path)
  path_without_query <- strsplit(path, "\?")[[1]][1]
  # Build the message string exactly as the Python version does
  message <- paste0(timestamp, method, path_without_query)
  # Convert message to raw bytes (UTF-8)
  message_raw <- charToRaw(message)
  # Sign using RSA-PSS with SHA-256
  # openssl::signature_create uses PSS padding with SHA-256 by default for RSA keys
  signature_raw <- openssl::signature_create(
    data      = message_raw,
    hash      = openssl::sha256,
    key       = private_key
  )
  # Base64-encode the binary signature and return as a plain string
  signature_b64 <- base64enc::base64encode(signature_raw)
  return(signature_b64)
}

#' Make an authenticated GET request to the Kalshi API
#'
#' @param private_key An openssl private key object
#' @param api_key_id  Your Kalshi API key ID
#' @param path        API endpoint path, e.g. "/portfolio/balance"
#' @param base_url    Base URL for the API (defaults to global BASE_URL)
#' @return An httr response object
kalshi_get <- function(private_key, api_key_id, path, base_url = BASE_URL) {
  # Generate millisecond-precision Unix timestamp as a string
  timestamp <- as.character(floor(as.numeric(Sys.time()) * 1000)) # changed round to floor
  # Build the full URL and extract just the path portion for signing
  full_url  <- paste0(base_url, path)
  parsed    <- httr::parse_url(full_url)
  # Reconstruct the path-only component (no host, no query string)
  sign_path <- parsed$path
  if (!startsWith(sign_path, "/")) {
    sign_path <- paste0("/", sign_path)
  }
  # Create the cryptographic signature
  signature <- create_signature(private_key, timestamp, "GET", sign_path)
  # Build authentication headers required by Kalshi
  auth_headers <- httr::add_headers(
    "KALSHI-ACCESS-KEY"       = api_key_id,
    "KALSHI-ACCESS-SIGNATURE" = signature,
    "KALSHI-ACCESS-TIMESTAMP" = timestamp
  )
  # Execute the GET request and return the full response object
  response <- httr::GET(url = full_url, config = auth_headers)
  return(response)
}

# ---------------------------------------------------------------------------
#   Main script
# ---------------------------------------------------------------------------
# Load private key from disk
private_key <- load_private_key(PRIVATE_KEY_PATH)
# Call the balance endpoint
response <- kalshi_get(private_key, API_KEY_ID, "/portfolio/balance")
# Parse JSON response body
body <- httr::content(response, as = "parsed", type = "application/json")
# Print balance (API returns cents, so divide by 100)

