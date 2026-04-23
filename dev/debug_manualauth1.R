library(openssl)
library(base64enc)
# library(httr)
library(httr2)

# Generate an example private key (in practice, load your actual private key)
private_key <- openssl::rsa_keygen(bits = 2048)

sign_request <- function(private_key, timestamp, method, path, body = "") {
  # Create the message string as Kalshi expects
  # Format: timestamp + method + path + body
  message_string <- paste0(timestamp, method, path, body)

  # Convert message to raw bytes
  message_bytes <- charToRaw(message_string)

  # Create SHA-256 hash of the message
  message_hash <- openssl::sha256(message_bytes)

  # Sign the hash using RSA private key
  # signature_create(data, hash, key, password)
  signature <- openssl::signature_create(
    data = message_bytes,
    hash = openssl::sha256,
    key = private_key
  )

  # Base64 encode the signature
  sig_b64 <- base64enc::base64encode(signature)

  return(sig_b64)
}

# Build authenticated request
make_kalshi_request <- function(private_key, method, path, key_id, body = "") {
  timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))

  sig_b64 <- sign_request(private_key, timestamp, method, path, body)

  headers <- httr::add_headers(
    `KALSHI-ACCESS-KEY`       = key_id,
    `KALSHI-ACCESS-SIGNATURE` = sig_b64,
    `KALSHI-ACCESS-TIMESTAMP` = timestamp,
    `Content-Type`            = "application/json"
  )

  base_url <- "https://trading-api.kalshi.com"
  url <- paste0(base_url, path)

  if (method == "GET") {
    response <- httr::GET(url, headers)
  } else if (method == "POST") {
    response <- httr::POST(url, headers, body = body, encode = "raw")
  }

  return(response)
}

# ---- Example: Load a real private key from a PEM file ----
# private_key <- openssl::read_key("path/to/your/private_key.pem")
# key_id <- "your-kalshi-api-key-id"
# response <- make_kalshi_request(private_key, "GET", "/trade-api/v2/portfolio/balance", key_id)
# httr::content(response)

# Example usage
private_key <- openssl::read_key(Sys.getenv("KALSHI_KEY_PATH"))
timestamp <- as.character(floor(as.numeric(Sys.time()) * 1000))  # milliseconds
method <- "GET"
path <- "/trade-api/v2/portfolio/balance"

signature <- sign_request(private_key, timestamp, method, path)

headers <- add_headers(
  "KALSHI-ACCESS-KEY"       = Sys.getenv("KALSHI_API_KEY"),
  "KALSHI-ACCESS-SIGNATURE" = signature,
  "KALSHI-ACCESS-TIMESTAMP" = timestamp
)

req <- request(paste0("https://demo-api.kalshi.co", path)) |>
  req_headers(
    "KALSHI-ACCESS-KEY"       = Sys.getenv("KALSHI_API_KEY"),
    "KALSHI-ACCESS-TIMESTAMP" = timestamp,
    "KALSHI-ACCESS-SIGNATURE" = signature
  )

resp <- req_perform(req)

balance <- resp_body_json(resp)
