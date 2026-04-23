load_all()
library(dplyr)
library(ggplot2)
py <- get_py()

env <- "production"
if (env == "demo") {
  creds <- kalshi_auth(Sys.getenv("KALSHI_DEMO_API_KEY"), Sys.getenv("KALSHI_DEMO_KEY_PATH"), "demo")
} else {
  creds <- kalshi_auth(Sys.getenv("KALSHI_API_KEY"), Sys.getenv("KALSHI_KEY_PATH"), "production")
}
kalshi_set_auth(creds)
get_balance()
mkts <- get_markets(series_ticker = "KXINXY", status = "open")

mkta<- mkts %>% filter(event_ticker == toupper("kxinxy-26dec31h1600"))
mkt <- get_market(toupper("KXINXY-26DEC31H1600-B7500"))
glimpse(mkt)

ob <- get_orderbook(toupper("KXINXY-26DEC31H1600-B7500"))

hist_mkt <- get_historical_markets(event_ticker = toupper("kxinxy-26dec31h1600"))

hist_markets <- get_historical_markets(
     series_ticker = "KXINXY",
     status        = "settled",
     all_pages     = TRUE,
     creds         = creds
   )
