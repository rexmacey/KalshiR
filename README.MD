---
editor_options: 
  markdown: 
    wrap: 72
---

# kalshiR <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/your-org/kalshiR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/your-org/kalshiR/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

**kalshiR** is a tidy R interface to the [Kalshi](https://kalshi.com)
prediction market API. It supports live and historical market data,
portfolio management, and order execution. All functions return
[tibbles](https://tibble.tidyverse.org/), timestamps are `POSIXct`
(UTC), and prices are in dollars.

> **Note:** The base URL `api.elections.kalshi.com` serves **all**
> Kalshi markets, not just elections.

------------------------------------------------------------------------
## Design Notes

Pagination: All list functions accept all_pages = TRUE to automatically
follow cursors. Use with care on large result sets.
Timestamps: All time arguments accept POSIXct objects or ISO-8601
strings ("2024-01-15T14:30:00Z").
Prices: Kalshi API prices are integer cents (1–99). kalshiR accepts
integer cents in order functions and returns cents for market prices.
Account balances are converted to dollars.
Rate limits: kalshiR does not add artificial delays. Kalshi's rate limits
are enforced server-side; httr2 will surface 429 errors as R conditions.
------------------------------------------------------------------------
Dependencies
kalshiR requires the following packages (all available on CRAN):

```
install.packages(c("httr2", "openssl", "tibble", "dplyr", "purrr", "rlang", "cli"))
```

Authentication
Kalshi uses RSA-PSS (SHA-256) request signing. You need:

Your API key ID from kalshi.com/profile/api
Your private key (.pem file downloaded when you created the API key)

library(kalshiR)

# Create a credentials object
```
creds <- kalshi_auth(
  key_id      = "your-api-key-id",
  private_key = "~/.ssh/kalshi_private_key.pem"  # path or key string
)
```

# Store globally for the session (so you don't pass creds= everywhere)
```
kalshi_set_auth(creds)
```
For the demo environment (paper trading):
```
demo_creds <- kalshi_auth(
  key_id      = "your-demo-key-id",
  private_key = "~/.ssh/kalshi_demo_key.pem",
  env         = "demo"
)
kalshi_set_auth(demo_creds)
```

Credentials can also be set via environment variables and loaded with
kalshi_auth_from_env(). See vignette("getting-started") for details.

## Quick Start
```
library(kalshiR)
library(dplyr)

# --- Auth (once per session) -------------------------------------------
kalshi_set_auth(kalshi_auth(
  key_id      = Sys.getenv("KALSHI_KEY_ID"),
  private_key = Sys.getenv("KALSHI_KEY_PATH")
))

# --- Market data -------------------------------------------------------
# Browse open markets
markets <- get_markets(status = "open", limit = 20L)
glimpse(markets)

# Orderbook for a specific market
ob <- get_orderbook("INXD-23DEC29-B4900")
print(ob$yes)   # data frame of yes-side levels
print(ob$no)    # data frame of no-side levels

# Recent trades
trades <- get_trades("INXD-23DEC29-B4900", limit = 50L)

# Candlestick chart (requires ggplot2)
candles <- get_market_candlesticks(
  series_ticker = "INXD",
  ticker        = "INXD-23DEC29-B4900",
  period_interval = 60L   # 60-minute candles
)

library(ggplot2)
ggplot(candles, aes(x = end_period_ts)) +
  geom_linerange(aes(ymin = price_low, ymax = price_high), colour = "grey50") +
  geom_segment(aes(xend = end_period_ts,
                   y = price_open, yend = price_close,
                   colour = price_close >= price_open), linewidth = 2) +
  scale_colour_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                      guide = "none") +
  labs(title = "INXD-23DEC29-B4900", x = NULL, y = "Price (cents)") +
  theme_minimal()

# --- Portfolio ---------------------------------------------------------
balance   <- get_balance()
positions <- get_positions(settled = FALSE)
fills     <- get_fills(limit = 100L)

# --- Orders ------------------------------------------------------------
# Place a limit order (1 contract, yes at 35 cents)
order <- create_order(
  ticker    = "INXD-23DEC29-B4900",
  side      = "yes",
  type      = "limit",
  count     = 1L,
  yes_price = 35L
)

# Check its status
get_order(order$order_id)

# Cancel it
cancel_order(order$order_id)

# --- Exchange status ---------------------------------------------------
get_exchange_status()
```

## Function Reference

```
Auth
Function             Description    
------------------ | ------------    
kalshi_auth()      | Create a credentials object    
kalshi_set_auth()  | Store credentials globally    
kalshi_get_auth()  | Retrieve stored credentials    
```

```
Markets
Function             Description    
------------------ | ------------    
kalshi_auth()      | Create a credentials object    
kalshi_set_auth()  | Store credentials globally    
kalshi_get_auth()  | Retrieve stored credentials    
```
