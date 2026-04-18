# kalshiR Development Progress

## Package Goal
A well-commented, tidy R interface to the Kalshi prediction market API
supporting: market data (live + historical), portfolio/balance/positions,
and order execution. Returns tibbles throughout.

## Design Decisions
- **HTTP client**: `httr2` (modern, pipeable, good error handling)
- **Crypto signing**: `openssl` R package (RSA-PSS SHA-256)
- **Output style**: All functions return tibbles; prices in cents are
  converted to dollars where appropriate; timestamps become POSIXct UTC
- **Auth model**: `kalshi_auth()` creates a credentials object;
  `kalshi_set_auth()` stores it globally for the session; all functions
  accept an optional `creds` argument for explicit passing
- **Pagination**: Internal `kalshi_paginate()` helper; all list functions
  have `all_pages` argument
- **Environments**: Production (`api.elections.kalshi.com`) and Demo
  (`demo-api.kalshi.co`) — configurable at auth time
- **Encoding note**: Kalshi's base URL uses `api.elections.kalshi.com`
  but serves ALL markets, not just elections

## Session 1 — Completed

### Files Created
| File | Status | Notes |
|------|--------|-------|
| `DESCRIPTION` | ✅ Done | Imports: httr2, openssl, tibble, dplyr, purrr, rlang, cli |
| `LICENSE` | ✅ Done | MIT |
| `R/auth.R` | ✅ Done | `kalshi_auth()`, `kalshi_set_auth()`, `kalshi_get_auth()`, `kalshi_sign()`, `kalshi_auth_headers()` |
| `R/client.R` | ✅ Done | `kalshi_get()`, `kalshi_post()`, `kalshi_delete()`, `kalshi_paginate()`, `kalshi_handle_response()` |
| `R/utils.R` | ✅ Done | `cents_to_dollars()`, `parse_kalshi_ts()`, `price_to_prob()`, `records_to_tibble()` |
| `R/markets.R` | ✅ Done | `get_series()`, `get_series_list()`, `get_events()`, `get_event()`, `get_markets()`, `get_market()`, `get_orderbook()`, `get_trades()`, `get_market_candlesticks()` |
| `tests/testthat/helper.R` | ✅ Done | Shared test setup, `mock_creds`, `skip_if_no_live_creds()`, `live_creds()` |
| `tests/testthat/test-auth.R` | ✅ Done | 9 tests covering signing, loading, global state |
| `tests/testthat/test-markets.R` | ✅ Done | Unit tests for utils + parse logic; live integration tests (skipped without creds) |
| `tests/testthat.R` | ✅ Done | testthat entry point |
| `vignettes/getting-started.Rmd` | ✅ Done | Auth setup, first requests, demo vs production |

## Session 2 — Completed

### Files Created / Updated
| File | Status | Notes |
|------|--------|-------|
| `R/orders.R` | ✅ Done | `create_order()`, `cancel_order()`, `get_orders()`, `get_order()`, `amend_order()`, `decrease_order()`, `batch_create_orders()`, `batch_cancel_orders()` |
| `tests/testthat/test-orders.R` | ✅ Done | 12 unit tests + 6 integration tests |

## Session 3 — Completed

### Files Created / Updated
| File | Status | Notes |
|------|--------|-------|
| `R/portfolio.R` | ✅ Done | `get_balance()`, `get_positions()`, `get_fills()`, `get_settlements()` |
| `R/historical.R` | ✅ Done | `get_historical_cutoff()`, `get_historical_markets()`, `get_historical_candlesticks()`, `get_historical_fills()`, `get_historical_orders()`, `get_historical_trades()` |
| `tests/testthat/test-portfolio.R` | ✅ Done | 9 unit tests + 4 integration tests |
| `tests/testthat/test-historical.R` | ✅ Done | 9 unit tests + 4 integration tests |

### Key Design Details Confirmed in Session 3

**`portfolio.R` internals:**
- `.to_kalshi_ms(ts)` converts POSIXct or ISO-8601 string to millisecond
  integer string (used by fills, orders, and historical endpoints)
- `get_positions()` list key: `"market_positions"` (not `"positions"`)
- `get_fills()` list key: `"fills"`
- `get_settlements()` list key: `"settlements"`
- Balance field names on API: `available_balance_cents`, `portfolio_value_cents`,
  `bonus_balance_cents` (with fallbacks for alternate field names)

**`historical.R` internals:**
- `get_historical_candlesticks()` path:
  `/historical/series/{series_ticker}/markets/{ticker}/candlesticks`
- Candle price fields are in a nested `price` object: `{open, high, low, close}`
- `get_historical_fills()` reuses `.parse_fills()` from `portfolio.R`
- `get_historical_orders()` reuses `.parse_orders()` from `orders.R`
- `get_historical_trades()` list key: `"trades"`
- `get_historical_markets()` list key: `"markets"`

**Cross-file dependencies (for R CMD CHECK / load order):**
- `historical.R` calls `.parse_fills()` (defined in `portfolio.R`)
- `historical.R` calls `.parse_orders()` (defined in `orders.R`)
- All files use `.to_kalshi_ms()` defined in `portfolio.R`
  → Consider moving `.to_kalshi_ms()` to `utils.R` if load-order issues arise

## Session 4 — TODO

### Files to Build
| File | Priority | Notes |
|------|----------|-------|
| `vignettes/trading.Rmd` | 🔴 High | Order lifecycle (create → monitor → cancel), positions, fills; show `get_balance()` before/after |
| `vignettes/market-data.Rmd` | 🔴 High | Candlesticks + ggplot2 example, historical vs live, pagination patterns |
| `README.md` | 🔴 High | Badges (R CMD CHECK, lifecycle), install instructions, 15-line quick-start |
| `R/exchange.R` | 🟡 Medium | `get_exchange_status()`, `get_exchange_schedule()`, `get_exchange_announcements()` |
| `tests/testthat/test-exchange.R` | 🟡 Medium | Small — exchange endpoints are read-only and simple |

### Later / Nice to Have
| Item | Notes |
|------|-------|
| `NAMESPACE` | Auto-generated by `devtools::document()` — run after all R files done |
| httptest2 fixtures | Record live API calls for fully offline mock tests |
| WebSocket support | Separate vignette; requires `websocket` package, not httr2 |
| `pkgdown` site | Auto-docs; straightforward once all `@export` tags are in place |

## Key API Notes (for reference)
- **Base URL**: `https://api.elections.kalshi.com/trade-api/v2` (production)
- **Demo URL**: `https://demo-api.kalshi.co/trade-api/v2`
- **Auth headers**: `KALSHI-ACCESS-KEY`, `KALSHI-ACCESS-TIMESTAMP` (ms),
  `KALSHI-ACCESS-SIGNATURE`
- **Signature**: RSA-PSS SHA-256 of `{timestamp_ms}{METHOD}{/trade-api/v2/path}`
  (no query params in the signed string)
- **Prices**: Integer cents (0–100 for market prices; larger integers for
  dollar balances)
- **Pagination**: Cursor-based; responses include `cursor` field for next page
- **Historical cutoff**: Use `/historical/cutoff` to determine which endpoints
  to query for old data

## Session 4 — Completed

### Files Created / Updated
| File | Status | Notes |
|------|--------|-------|
| `README.md` | ✅ Done | Badges, install, function reference table, quick-start |
| `R/exchange.R` | ✅ Done | `get_exchange_status()`, `get_exchange_schedule()`, `get_exchange_announcements()` |
| `tests/testthat/test-exchange.R` | ✅ Done | 7 unit tests + 3 integration tests (demo) |
| `vignettes/market-data.Rmd` | ✅ Done | Candlesticks + ggplot2, pagination patterns, historical vs live |
| `vignettes/trading.Rmd` | ✅ Done | Full order lifecycle, positions, fills, batch ops, error handling |

### Key Design Details Confirmed in Session 4

**`exchange.R` response shapes:**
- `get_exchange_status()` → fields: `trading_active`, `exchange_active`, `condition`
- `get_exchange_schedule()` → nested under `response$schedule$standard_hours`;
  fallback keys `trading_hours`; returns empty tibble if absent
- `get_exchange_announcements()` → list key `"announcements"`; `created_time`
  parsed to POSIXct via `parse_kalshi_ts()`

**`%||%` operator usage:**
- Used throughout `exchange.R` for `NULL`-safe field access
- Defined in `utils.R` (or `rlang::%||%`) — confirm one canonical definition
  exists before running `devtools::document()`

## Session 5 — TODO

### Files to Build / Finalize
| File | Priority | Notes |
|------|----------|-------|
| `NAMESPACE` | 🔴 High | Run `devtools::document()` after confirming all `@export` tags present |
| `.github/workflows/R-CMD-check.yaml` | 🔴 High | Standard usethis workflow; required for README badge |
| `R/utils.R` (review) | 🟡 Medium | Confirm `%||%` is exported or re-exported from rlang; move `.to_kalshi_ms()` here if load-order issues found |
| `README.md` (polish) | 🟡 Medium | Replace `your-org` placeholder; add real badge URLs once repo is on GitHub |
| `pkgdown` config (`_pkgdown.yml`) | 🟢 Low | Group functions by topic in reference page |
| httptest2 fixtures | 🟢 Low | Record demo API calls for fully offline CI tests |
| WebSocket vignette | 🟢 Low | Requires `websocket` package; separate from httr2 flow |

### Known Issues / Checks Needed Before CRAN-readiness
1. **Load order**: `historical.R` calls `.parse_fills()` (portfolio.R) and
   `.parse_orders()` (orders.R). R loads files alphabetically, so
   `historical.R` loads before `orders.R` and `portfolio.R` — this is fine
   at runtime (all loaded before user calls anything) but double-check with
   `devtools::check()`.
2. **`%||%` duplication**: Ensure `%||%` is defined exactly once (suggest
   importing from `rlang` via `@importFrom rlang %||%` in `utils.R`).
3. **`records_to_tibble()` robustness**: Test with NULL and length-1 lists
   to ensure no silent coercion errors.
4. **Demo vs production URL**: Verify `get_exchange_schedule()` and
   `get_exchange_announcements()` exist on demo endpoint (they may be
   production-only).
