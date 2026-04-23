library(openssl)
library(httr2)
library(jsonlite)

kalshi_base_url <- function(env = c("demo", "production")) {
  env <- match.arg(env)
  if (env == "demo") {
    "https://demo-api.kalshi.com/trade-api/v2"
  } else {
    "https://api.elections.kalshi.com/trade-api/v2"
  }
}

kalshi_sign <- function(private_key_path, timestamp_ms, method, path) {
  key <- openssl::read_key(private_key_path)

  string_to_sign <- paste0(timestamp_ms, toupper(method), path)
  cat(string_to_sign, "\n")
  sig_raw <- openssl::signature_create(
    data = charToRaw(string_to_sign),
    key  = key,
    hash = openssl::sha256
  )

  openssl::base64_encode(sig_raw)
}


kalshi_headers <- function(req, private_key_path, method, path) {
  key_id <- Sys.getenv("KALSHI_API_KEY")
  if (key_id == "") stop("KALSHI_API_KEY environment variable not set")

  timestamp_ms <- as.character(floor(as.numeric(as.POSIXct(Sys.time(), tz = "UTC"))*1000))

  signature <- kalshi_sign(
    private_key_path = private_key_path,
    timestamp_ms = timestamp_ms,
    method = method,
    path = path
  )

  req |>
    httr2::req_headers(
      "KALSHI-ACCESS-KEY"       = key_id,
      "KALSHI-ACCESS-TIMESTAMP" = timestamp_ms,
      "KALSHI-ACCESS-SIGNATURE" = signature
    )
}


kalshi_get <- function(env = c("demo", "production"), private_key_path,
                       endpoint) {
  env <- match.arg(env)
  base <- kalshi_base_url(env)
  url  <- paste0(base, endpoint)

  httr2::request(url) |>
    httr2::req_method("GET") |>
    kalshi_headers(
      private_key_path = private_key_path,
      method = "GET",
      path = endpoint
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

get_balance <- function(env = c("demo", "production"), private_key_path) {
  kalshi_get(
    env = env,
    private_key_path = private_key_path,
    endpoint = "/portfolio/balance")
}

balance <- get_balance(
  env = "production",
  private_key_path = "C:/Users/xer09/OneDrive/Documents/kalshi/marketmaker3.txt"
)

balance

tstamp <- as.character(floor(as.numeric(as.POSIXct(Sys.time(), tz = "UTC"))*1000))
# method = "GET"
private_key_path = "C:/Users/xer09/OneDrive/Documents/kalshi/marketmaker3.txt"
signature <- kalshi_sign(
  private_key_path = private_key_path,
  timestamp_ms = tstamp,
  method = "GET",
  path = "/portfolio/balance"
)


