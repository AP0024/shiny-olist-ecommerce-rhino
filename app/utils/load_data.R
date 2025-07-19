load_olist_data <- function(path = "data") {
  files <- list(
    orders = "olist_orders_dataset.csv",
    customers = "olist_customers_dataset.csv",
    order_items = "olist_order_items_dataset.csv",
    products = "olist_products_dataset.csv",
    payments = "olist_order_payments_dataset.csv",
    geolocation = "olist_geolocation_dataset.csv",
    sellers = "olist_sellers_dataset.csv",
    reviews = "olist_order_reviews_dataset.csv"
  )
  
  data <- lapply(files, function(file) {
    full_path <- file.path(path, file)
    cat("Loading:", full_path, "\n")
    readr::read_csv(full_path)
  })
  names(data) <- names(files)
  
  cat("✅ All datasets loaded successfully.\n")
  return(data)
}
