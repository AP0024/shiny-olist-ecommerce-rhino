load_olist_data <- function(path = "data") {
  files <- c(
    orders = "olist_orders_dataset.csv",
    customers = "olist_customers_dataset.csv",
    order_items = "olist_order_items_dataset.csv",
    products = "olist_products_dataset.csv",
    payments = "olist_order_payments_dataset.csv",
    sellers = "olist_sellers_dataset.csv",
    reviews = "olist_order_reviews_dataset.csv"
  )
  
  lapply(files, function(file) readr::read_csv(file.path(path, file)))
}
