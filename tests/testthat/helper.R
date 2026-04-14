# tests/testthat/helper.R
# Shared test setup loaded automatically by testthat before each test file.

library(kalshiR)

# A mock credentials object for tests that need one but don't make real calls.
# The private key here is a freshly generated throwaway RSA key for testing only.
# Real tests that hit the network are skipped unless KALSHI_API_KEY_ID and
# KALSHI_KEY_PATH environment variables are set.
mock_creds <- structure(
  list(
    api_key_id  = "test-key-id-00000000-0000-0000-0000-000000000000",
    private_key = NULL,  # Not needed for non-signing tests
    base_url    = "https://api.elections.kalshi.com/trade-api/v2",
    env         = "production"
  ),
  class = "kalshi_credentials"
)

# Helper: skip a test if live API credentials are not configured
skip_if_no_live_creds <- function() {
  testthat::skip_if(
    Sys.getenv("KALSHI_API_KEY_ID") == "",
    message = "Live API credentials not set (KALSHI_API_KEY_ID not found)."
  )
}

# Helper: load live credentials from environment variables
live_creds <- function() {
  key_id   <- Sys.getenv("KALSHI_API_KEY_ID")
  key_path <- Sys.getenv("KALSHI_KEY_PATH")
  kalshi_auth(api_key_id = key_id, private_key_path = key_path)
}
