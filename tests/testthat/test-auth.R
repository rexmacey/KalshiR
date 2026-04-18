# tests/testthat/test-auth.R
# Tests for kalshi_auth(), kalshi_set_auth(), kalshi_sign(), and helpers.

test_that("kalshi_auth() errors on missing key file", {
  expect_error(
    kalshi_auth(
      api_key_id       = "fake-id",
      private_key_path = "/nonexistent/path/key.pem"
    ),
    regexp = "not found"
  )
})

test_that("kalshi_auth() errors on empty api_key_id", {
  expect_error(
    kalshi_auth(
      api_key_id       = "",
      private_key_path = tempfile()
    ),
    regexp = "non-empty"
  )
})

test_that("kalshi_auth() resolves production URL correctly", {
  # Create a minimal fake key file (we test loading separately)
  key_file <- tempfile(fileext = ".key")

  # Generate a real RSA key for testing
  skip_if_not_installed("openssl")
  key <- openssl::rsa_keygen(bits = 2048L)
  openssl::write_pem(key, key_file)
  on.exit(unlink(key_file))

  creds <- kalshi_auth(
    api_key_id       = "test-id",
    private_key_path = key_file,
    env              = "production"
  )

  expect_s3_class(creds, "kalshi_credentials")
  expect_equal(creds$env, "production")
  expect_match(creds$base_url, "api.elections.kalshi.com")
  expect_equal(creds$api_key_id, "test-id")
})

test_that("kalshi_auth() resolves demo URL correctly", {
  skip_if_not_installed("openssl")
  key_file <- tempfile(fileext = ".key")
  key <- openssl::rsa_keygen(bits = 2048L)
  openssl::write_pem(key, key_file)
  on.exit(unlink(key_file))

  creds <- kalshi_auth("test-id", key_file, env = "demo")

  expect_equal(creds$env, "demo")
  expect_match(creds$base_url, "demo-api.kalshi.co")
})

test_that("kalshi_set_auth() stores credentials globally", {
  skip_if_not_installed("openssl")
  key_file <- tempfile(fileext = ".key")
  key <- openssl::rsa_keygen(bits = 2048L)
  openssl::write_pem(key, key_file)
  on.exit({
    unlink(key_file)
    # Clean up global state
    rm("creds", envir = kalshiR:::.kalshi_env)
  })

  creds <- kalshi_auth("test-id", key_file)
  kalshi_set_auth(creds)

  retrieved <- kalshiR:::kalshi_get_auth()
  expect_equal(retrieved$api_key_id, "test-id")
})

test_that("kalshi_get_auth() errors when no credentials are set", {
  # Ensure no credentials are present
  if (exists("creds", envir = kalshiR:::.kalshi_env)) {
    rm("creds", envir = kalshiR:::.kalshi_env)
  }

  expect_error(
    kalshiR:::kalshi_get_auth(),
    regexp = "No Kalshi credentials found"
  )
})

test_that("kalshi_set_auth() rejects non-credentials objects", {
  expect_error(
    kalshi_set_auth(list(foo = "bar")),
    regexp = "kalshi_credentials"
  )
})

test_that("print.kalshi_credentials() works without error", {
  skip_if_not_installed("openssl")
  key_file <- tempfile(fileext = ".key")
  key <- openssl::rsa_keygen(bits = 2048L)
  openssl::write_pem(key, key_file)
  on.exit(unlink(key_file))

  creds <- kalshi_auth("test-id", key_file)
  # expect_output(print(creds), "production")
  # expect_output(print(creds), "test-id")
  # cli writes to the message stream, not stdout
  msg <- testthat::capture_messages(print(creds))
  full_output <- paste(msg, collapse = "")
  expect_match(full_output, "production", fixed = TRUE)
  expect_match(full_output, "test-id",    fixed = TRUE)
})

test_that("kalshi_sign() produces a base64 string", {
  skip_if_not_installed("openssl")
  key <- openssl::rsa_keygen(bits = 2048L)

  sig <- kalshiR:::kalshi_sign(
    private_key  = key,
    timestamp_ms = "1703123456789",
    method       = "GET",
    path         = "/trade-api/v2/portfolio/balance"
  )

  expect_type(sig, "character")
  expect_true(nchar(sig) > 10)
  # base64 characters only
  expect_match(sig, "^[A-Za-z0-9+/=]+$")
})

test_that("kalshi_sign() strips query params before signing", {
  skip_if_not_installed("openssl")
  key <- openssl::rsa_keygen(bits = 2048L)

  sig1 <- kalshiR:::kalshi_sign(key, "12345", "GET",
                                 "/trade-api/v2/portfolio/orders")
  sig2 <- kalshiR:::kalshi_sign(key, "12345", "GET",
                                 "/trade-api/v2/portfolio/orders?limit=5&cursor=abc")

  # Both should produce the same signature (query stripped)
  expect_equal(sig1, sig2)
})
