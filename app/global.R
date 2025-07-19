# 📦 Required packages
packages <- c(
  "shiny", "shiny.semantic", "shiny.router",
  "dplyr", "tidyverse", "dtplyr", "readr", "lubridate",
  "randomForest", "xgboost", "Metrics",
  "dygraphs", "xts", "RColorBrewer",
  "plotly", "reactable", "reactablefmtr", "leaflet", "DT","highcharter"
)

# 📥 Auto install & load
install_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
invisible(lapply(packages, install_and_load))

# 📂 Source modules and utilities
source("modules/orders_server.R")      # order module
source("modules/orders_ui.R")      # order module
source("utils/load_data.R")     # load_olist_data()

# 🔀 Define routing
router <- router_ui(
  route("orders", orders_ui("orders_module")),
  default = orders_ui("orders_module")
)

# --------------------------------------
# 📊 Orders - Insight Helper Functions
# --------------------------------------

get_aov_insight_text <- function(percent_single_item, avg_price, aov,
                                 high_single_threshold = 75,
                                 significant_diff = 40) {
  insight <- c()
  
  # Base metrics
  insight <- append(insight, c(
    paste0("🔢 ", percent_single_item, "% of your customers are buying only one item per order."),
    paste0("🧾 Average product price: $", avg_price),
    paste0("💰 Average order value (AOV): $", aov)
  ))
  
  high_price_diff <- (aov - avg_price) >= significant_diff
  low_price_diff <- (avg_price - aov) >= significant_diff
  
  if (percent_single_item >= high_single_threshold) {
    insight <- append(insight, "⚠️ Most customers are buying only one item per order.")
    
    if (high_price_diff) {
      insight <- append(insight, c(
        "🔍 AOV is much higher than the average product price — customers are buying high-ticket single items.",
        "📌 This suggests a strong premium product line."
      ))
    } else {
      insight <- append(insight, c(
        "⚠️ AOV is close to average product price — mostly low-value single-item purchases.",
        "💡 Consider bundling or discounts to increase AOV."
      ))
    }
    insight <- append(insight, "🟠 Status: Needs Improvement")
    
  } else {
    insight <- append(insight, "✅ There's a good mix of single and multi-item orders.")
    
    if (aov >= avg_price * 1.3) {
      insight <- append(insight, c(
        "📈 AOV is significantly above the average price — customers likely buying bundles or multiple items.",
        "🚀 Great job! Maintain or enhance with upsells."
      ))
      insight <- append(insight, "🟢 Status: Healthy")
    } else {
      insight <- append(insight, c(
        "📉 AOV is not substantially higher than average price.",
        "🔍 Multi-item orders may not be translating into higher value.",
        "🛠️ Consider stronger upsell/cross-sell offers."
      ))
      insight <- append(insight, "🟡 Status: Moderate")
    }
  }
  
  paste(insight, collapse = "\n")
}

# --------------------------------------
# 🔁 Forecast Helper Functions
# --------------------------------------

predict_recursive <- function(model, type, lags, data, future_dates, h) {
  preds <- numeric(h)
  
  for (i in 1:h) {
    new_row <- tibble(ds = future_dates[i])
    
    for (lag in lags) {
      lag_name <- paste0("lag_", lag)
      new_row[[lag_name]] <- if (nrow(data) >= lag) data$y[nrow(data) - lag + 1] else NA
    }
    
    if (type == "lm" || type == "rf") {
      preds[i] <- predict(model, newdata = new_row)
    } else if (type == "xgb") {
      mat <- as.matrix(new_row %>% select(starts_with("lag_")))
      preds[i] <- predict(model, xgb.DMatrix(mat))
    }
    
    data <- bind_rows(data, tibble(ds = future_dates[i], y = preds[i]))
  }
  
  preds
}

get_accuracy <- function(actual, predicted, model_name) {
  tibble(
    Model = model_name,
    MAE = mean(abs(actual - predicted), na.rm = TRUE),
    RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  )
}
