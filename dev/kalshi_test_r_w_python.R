load_all()
py <- get_py()
credsl <-  kalshi_auth(Sys.getenv("KALSHI_API_KEY"), Sys.getenv("KALSHI_KEY_PATH"), "production")
responsel = py$get(credsl$private_key, credsl$api_key_id, "/portfolio/balance", credsl$base_url)
responsel$status_code
credsd <-  kalshi_auth(Sys.getenv("KALSHI_DEMO_API_KEY"), Sys.getenv("KALSHI_DEMO_KEY_PATH"), "demo")
responsed = py$get(credsd$private_key, credsd$api_key_id, "/portfolio/balance", credsd$base_url)
responsed$status_code

respl <- responsel$json()
respd <- responsed$json()
