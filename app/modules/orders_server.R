# modules/orders server



orders_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    
    #########Data------------
    orders <- reactive({
      lazy_dt(data()$orders)
    })
    
    payments <- reactive({
      lazy_dt(data()$payments)
    })
    
    order_items <- reactive({
      lazy_dt(data()$order_items)
    })
    customers <- reactive({
      lazy_dt(data()$customers)
    })
    
    geolocation <- reactive({
      lazy_dt(data()$geolocation)
    })
    
    # Join orders and payments data
    order_payments_dt <- reactive({
      orders() %>%
        inner_join(payments(), by = "order_id")
    })
    # Join orders and customers for location data
    
    orders_with_location <- reactive({
      orders() %>%
        inner_join(customers(), by = "customer_id")
    })
    
    geo_coords <- reactive({
      
      geolocation() %>%
        group_by(geolocation_city) %>%
        summarise(
          lat = mean(geolocation_lat, na.rm = TRUE),
          lng = mean(geolocation_lng, na.rm = TRUE),
          .groups = "drop"
        )
      
    })
    
    # Count orders per city
    order_counts_bycity <- reactive({
      orders_with_location() %>%
        group_by(customer_city) %>%
        summarise(order_count = n(), .groups = "drop") %>%
        arrange(desc(order_count)) %>%
        slice_head(n = 5)
    })
    #Merge top cities with their coordinate
    top_5_cities_with_coords <- reactive({
      order_counts_bycity() %>%
        left_join(geo_coords(),
                  by = c("customer_city" = "geolocation_city")) %>%
        arrange(desc(order_count)) %>%
        mutate(
          order_count = as.numeric(order_count),  # ensure it's numeric
          rank = as.numeric(dense_rank(desc(order_count))),  # ensure rank is numeric
          stars = case_when(
            rank == 1 ~ "★★★★★",
            rank == 2 ~ "★★★★☆",
            rank == 3 ~ "★★★☆☆",
            rank == 4 ~ "★★☆☆☆",
            TRUE      ~ "★☆☆☆☆"
          ),
          popup_html = paste0(
            "<b>🏆 Rank ", rank, ": ", customer_city, "</b><br>",
            "Orders: ", order_count, "<br>",
            "<div style='color:orange;font-size:16px;'>", stars, "</div>"
          )
        )%>%
        
        as_tibble() 
    })
    
    #####Payment Installmnet Order value---------
    
    # --- Step 1: Compute total order value ---
    order_values <- reactive({
      order_items() %>%
        group_by(order_id) %>%
        summarise(order_value = sum(price + freight_value), .groups = "drop") %>% as_tibble()
      
    })
    #- Step 2: Get max installments per order ---
    order_payment_info <- reactive({
      payments() %>%
        group_by(order_id) %>%
        summarise(installments = max(payment_installments), .groups = "drop") %>%
        inner_join(order_values(), by = "order_id")%>% as_tibble()
    })
    # #-: Correlation between order value and installments ---
    #   cor_result <- cor(order_payment_info$installments, 
    #                     order_payment_info$order_value,
    #                     method = "spearman")
    # --- Step 3: Summary by Installments ---
    summaryinstallment_table <- reactive({
      order_payment_info() %>%
        group_by(installments) %>%
        summarise(
          avg_order_value = round(mean(order_value), 2),
          n_orders = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(installments))
    })
    
    #--------Average Order Value ---- 
    # Order-level analysis
    order_analysis <- reactive({
      order_payments_dt() %>%
        group_by(order_id) %>%
        summarise(
          total_price = sum(payment_value, na.rm = TRUE),
          item_count = n(),
          avg_item_price = mean(payment_value, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    # Overall metrics
    overall_summary <- reactive({
      order_analysis() %>%
        summarise(
          total_orders = n(),
          single_item_orders = sum(item_count == 1),
          AOV = mean(total_price),
          avg_product_price = mean(avg_item_price)
        ) %>%
        mutate(
          percent_single_item = 100 * single_item_orders / total_orders,
          percent_single_item = round(percent_single_item, 1),
          AOV = round(AOV, 2),
          avg_product_price = round(avg_product_price, 2)
        ) %>% as_tibble()
    })
    
    #-------End Average Order Value ---- 
    
    #########KPI-------------    
    orders_data_monthly <- reactive({
      orders() %>% 
        mutate(order_month = floor_date(as.Date(order_purchase_timestamp), unit = "month")) %>%
        group_by(order_month) %>%
        summarise(total_orders = n(), .groups = "drop") %>%
        arrange(order_month) %>%  # ensure chronological order
        mutate(cumulative_orders = cumsum(total_orders)) %>%
        as_tibble()
    })
    
    output$orders_dygraph <- renderDygraph({
      orders_xts <- xts(orders_data_monthly()$total_orders, order.by = orders_data_monthly()$order_month)
      dygraph(orders_xts, main = NULL) %>%
        dyOptions(
          fillGraph = TRUE,
          colors = "steelblue",
          drawAxesAtZero = FALSE,
          drawGrid = FALSE,
          axisLineColor = "transparent",
          axisLabelColor = "transparent"
        ) %>%
        dyAxis("x", drawGrid = FALSE, axisLabelFormatter = 'function(d) { return "" }') %>%
        dyAxis("y", drawGrid = FALSE, axisLabelFormatter = 'function(d) { return "" }')
    })
    
    #Deviation df -----------------------
    deviation_df <- reactive({
      orders() %>%
        mutate(order_month = floor_date(as.Date(order_purchase_timestamp), unit = "month")) %>%
        group_by(order_month) %>%
        summarise(total_orders = n(), .groups = "drop") %>%
        arrange(order_month) %>% mutate(
          month_label = format(order_month, "%Y-%m"),
          mean_orders = mean(total_orders),
          deviation = total_orders - mean_orders,
          direction = ifelse(deviation >= 0, "Above Mean", "Below Mean"),
          month_label = factor(month_label, levels = unique(format(order_month, "%Y-%m")))) %>% as_tibble()
    })
    
    
    
    output$devitationSummary <- renderDT({
      
      click_data <- event_data("plotly_click",source = "deviation_click_plotly")
      selected_data <- event_data("plotly_selected",source = "deviation_click_plotly")
      
      
      df <- deviation_df() %>%
        select(order_month, total_orders, mean_orders, deviation) %>%
        mutate(deviation=round(deviation,2))%>%
        rename(
          "Date" = order_month,
          "Total Orders" = total_orders,
          "Avg Order" = mean_orders,
          "Orders vs Avg" = deviation
        )
      
      if (!is.null(click_data)) {
        df <- df %>% filter(Date %in% click_data$x)
      } else if(!is.null(selected_data)){
        df <- df %>% filter(Date %in% unique(selected_data$x))
      }else{
        df
      }
      
      
      datatable(
        df,
        options = list(pageLength = 5)  # 👈 Set to 10, or any number you prefer
      )
      
    })
    
    # selected_rows_devitationSummary <- reactive({
    #   input$devitationSummary_rows_selected
    # })
    
    
    # filtered_deviation_df <- reactive({
    #   if (is.null(selected_rows_devitationSummary()) || length(selected_rows_devitationSummary()) == 0) {
    #     deviation_df()  # Return all if nothing selected
    #   } else {
    #     deviation_df()[selected_rows_devitationSummary(), ]
    #   }
    # })

output$deviation_orders_highcharter <- renderHighchart({
  df <- deviation_df()

  # Set color based on deviation
  df <- df %>%
    mutate(
      color = ifelse(deviation >= 0, "#008000", "#FF0000")  # Green or Red
    )

  highchart() %>%
    hc_add_dependency("modules/lollipop.js") %>%
    hc_chart(type = "lollipop") %>%
    hc_xAxis(
      categories = df$month_label,
      title = list(text = "Month"),
      lineWidth = 0,
      tickWidth = 0
    ) %>%
    hc_yAxis(
      title = list(text = "Deviation from Mean"),
      plotLines = list(
        list(value = 0, color = "#000000", width = 1, dashStyle = "ShortDash")
      )
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = paste0(
        "<b>Month:</b> {point.category}<br>",
        "<b>Total Orders:</b> {point.total}<br>",
        "<b>Deviation:</b> {point.y}"
      )
    ) %>%
    hc_plotOptions(lollipop = list(
      stemWidth = 2,
      colorByPoint = FALSE,
      marker = list(radius = 6)
    )) %>%
    hc_add_series(
      name = "Deviation",
      type = "lollipop",
      data = purrr::pmap(df, function(month_label, total_orders, deviation, direction, mean_orders, color, ...) {
        list(
          y = deviation,
          color = color,
          total = total_orders
        )
      })
    )
})

    
    
    # output$devitation_orders_plotly <- renderPlotly({
    #   plot_ly(
    #     data = deviation_df(),
    #     x = ~order_month,
    #     y = ~deviation,
    #     type = 'bar',
    #     color = ~direction,
    #     colors = c("Below Mean" = "#FF3030", "Above Mean" = "#008080"),
    #     hoverinfo = "text",
    #     text = ~paste("Month:", month_label,
    #                   "<br>Total Orders:", total_orders,
    #                   "<br>Deviation:", round(deviation, 2)),
    #     source = "deviation_click_plotly"  
    #     
    #   ) %>%
    #     layout(
    #       title = NULL,
    #       annotations = list(),
    #       xaxis = list(title = ""),
    #       yaxis = list(visible = FALSE),  # 👈 Hide Y-axis
    #       bargap = 0.1
    #       # dragmode = "select",  # ✅ enables selection mode
    #       # selectdirection = "any"
    #     )
    # })
    #####---------Total Order KPI---------
    output$total_orders <- renderText({
      orders() %>% 
        summarise(total_orders = n()) %>%
        as_tibble() %>%
        pull()
    })
    
    #####---------End TotalOrders---------
    
    #####---------Cancellation Rate---------
    output$order_cancellation_rate <- renderText({
      orders() %>%
        mutate(is_cancelled = order_status == "canceled") %>%
        summarise(
          total_orders = n_distinct(order_id),
          cancelled_orders = n_distinct(order_id[is_cancelled])
        ) %>%
        mutate(
          cancellation_rate = round((cancelled_orders / total_orders) * 100, 2)
        ) %>% as_tibble() %>%
        pull(cancellation_rate)
    })
    #####---------End Cancellation Rate---------
    
    #####---------avg_delivery_days---------
    output$avg_delivery_days <- renderText({
      orders() %>%
        mutate(delivery_days = as.numeric(difftime(
          order_delivered_customer_date,
          order_purchase_timestamp,
          units = "days"
        ))) %>%
        summarise(avg_days = mean(delivery_days, na.rm = TRUE)) %>%
        as_tibble() %>%
        pull(avg_days)
    })
    #####---------End avg_delivery_days---------
    
    ####-----AOV Gauge----------
    output$aov_gauge <- renderPlotly({
      
      aov_val <- overall_summary()$AOV
      avg_price <- overall_summary()$avg_product_price
      green_threshold <- 0.75 * avg_price
      yellow_threshold <- 0.5 * avg_price
      
      gauge_color <- if (aov_val >= green_threshold) {
        "green"
      } else if (aov_val >= yellow_threshold) {
        "yellow"
      } else {
        "red"
      }
      
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = aov_val,
        # title = list(text = "", font = list(size = 18)),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = avg_price,
                     increasing = list(color = "red"),
                     decreasing = list(color = "green")),
        gauge = list(
          axis = list(range = list(NULL, avg_price + 50)),
          bar = list(color = gauge_color),
          steps = list(
            list(range = c(0, yellow_threshold), color = "#f03b20"),       # Red zone
            list(range = c(yellow_threshold, green_threshold), color = "#ffffb2"), # Yellow zone
            list(range = c(green_threshold, avg_price + 50), color = "#a1dab4")    # Green zone
          ),
          threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = avg_price
          )
        )
      )
    })
    
    
    
    output$aov_insight <- renderText({
      
      get_aov_insight_text(percent_single_item = overall_summary()$percent_single_item, 
                           avg_price = overall_summary()$avg_product_price,
                           aov = overall_summary()$AOV,
                           high_single_threshold = 75,
                           significant_diff = 40)
    })
    ####-----END AOV Gauge--------
    
    
    ####Top 5 cities by Orders-----------    
    output$order_city_map <- renderLeaflet({
      
      leaflet(
        top_5_cities_with_coords()
      ) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addLabelOnlyMarkers(
          lng = ~lng,
          lat = ~lat,
          label = ~lapply(popup_html, HTML),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = FALSE,
            style = list(
              "background-color" = "white",
              "border-radius" = "5px",
              "padding" = "5px",
              "text-align" = "center"
            )
          )
        )
      
    })
    
    
    ######Forecast Orders Using ML dygraph-----------
    
    
    
    monthly_orders <- reactive({
      raw_orders <- orders()
      validate(
        need(!is.null(raw_orders) && nrow(raw_orders) > 0, "No order data available.")
      )
      raw_orders %>%
        filter(order_status == "delivered") %>%
        mutate(ds = lubridate::floor_date(as.Date(order_purchase_timestamp), "month")) %>%
        count(ds, name = "y") %>%
        mutate(
          year = lubridate::year(ds),
          month = lubridate::month(ds)
        ) %>%
        arrange(ds) %>% as_tibble()
    })
    
    ###Input Tracker Inititalization
    input_tracker <- reactiveValues(
      horizon = 5,
      lags = 3,
      models = c("lm", "rf", "xgb")
    )
    ###Update tracker only when button is clicked
    observeEvent(input$submit_forecast, {
      
      input_tracker$horizon <- input$forecast_horizon
      input_tracker$lags <- input$forecast_lags
      input_tracker$models <- input$model_choice
    })
    comparison_df <- reactive({
      req(input_tracker$horizon, input_tracker$lags, input_tracker$models)
      
      tryCatch({
        
        df <- monthly_orders()
        lags <- 1:input_tracker$lags
        
        all_sets <- unlist(
          lapply(1:min(3, length(lags)), function(k) combn(lags, k, simplify = FALSE)),
          recursive = FALSE
        )
        
        h <- input_tracker$horizon
        all_comparisons <- list()
        warning_shown <- FALSE  # 👈 Track if notification is shown
        
        
        for (lags in all_sets) {
          df <- monthly_orders()
          # Check if 'y' column exists and is numeric
          if (!"y" %in% names(df) || !is.numeric(df$y)) {
            if (!warning_shown) {
              showNotification("❌ Error: 'y' column missing or not numeric.", type = "error")
              warning_shown <- TRUE
            }
            next
          }
          
          # ── Create Lags Safely ──
          tryCatch({
            for (lag in lags) {
              df[[paste0("lag_", lag)]] <- as.numeric(dplyr::lag(df$y, lag))
            }
          }, error = function(e) {
            if (!warning_shown) {
              showNotification(
                paste("❌ Lag Error:", e$message),
                type = "error"
              )
              warning_shown <<- TRUE
            }
            next
          })
          
          
          df <- tidyr::drop_na(df)
          # ✅ Check: Are lag columns numeric?
          lag_cols <- df %>% dplyr::select(starts_with("lag_"))
          if (!all(sapply(lag_cols, is.numeric))) {
            if (!warning_shown) {
              showNotification(
                "❌ Forecast aborted: One or more lag variables are not numeric. Check your data.",
                type = "error", duration = 8
              )
              warning_shown <- TRUE  # 👈 Prevent multiple notifications
            }
            next  # ⛔ Skip to next iteration
          }
          train <- df %>% filter(year <= 2017)
          if (nrow(train) == 0) next
          
          X <- train %>% select(starts_with("lag_")) %>% as.matrix()
          y <- train$y
          
          model_lm <- model_rf <- model_xgb <- NULL
          
          if ("lm" %in% input_tracker$models) {
            model_lm <- lm(y ~ ., data = train %>% select(y, starts_with("lag_")))
          }
          if ("rf" %in% input_tracker$models) {
            model_rf <- randomForest(y ~ ., data = train %>% select(y, starts_with("lag_")), ntree = 200)
          }
          if ("xgb" %in% input_tracker$models) {
            model_xgb <- xgboost(data = xgb.DMatrix(X, label = y), nrounds = 80,
                                 objective = "reg:squarederror", verbose = 0)
          }
          
          last_data <- df %>% filter(year == 2017) %>% tail(max(lags))
          future_dates <- seq.Date(from = max(last_data$ds) %m+% months(1), by = "month", length.out = h)
          
          preds <- list()
          if (!is.null(model_lm)) {
            preds$Linear <- predict_recursive(model_lm, "lm", lags, last_data, future_dates, h)
          }
          if (!is.null(model_rf)) {
            preds$RandomForest <- predict_recursive(model_rf, "rf", lags, last_data, future_dates, h)
          }
          if (!is.null(model_xgb)) {
            preds$XGBoost <- predict_recursive(model_xgb, "xgb", lags, last_data, future_dates, h)
          }
          
          test_data <- monthly_orders() %>% filter(year == 2018, month <= 8) %>% head(h)
          actuals <- test_data$y
          test_dates <- test_data$ds
          
          # ❗ Only show once
          if (length(actuals) < h && !warning_shown) {
            showNotification(
              paste0("⚠️ Forecast period is ", h, 
                     " months, but only ", length(actuals), 
                     " months of test data are available."),
              type = "warning"
            )
            warning_shown <- TRUE
          }
          
          for (model in names(preds)) {
            all_comparisons[[paste(model, paste(lags, collapse = "_"), sep = "_")]] <- tibble(
              Date = test_dates,
              Actual = actuals,
              Predicted = round(preds[[model]], 2)[seq_along(actuals)],  # Match actual size
              ErrorRate = round(abs(actuals - preds[[model]][seq_along(actuals)]) / actuals * 100, 2),
              Model = model,
              Lags = paste(lags, collapse = ",")
            )
          }
        }
        
        bind_rows(all_comparisons)
        
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    })
    
    
    
    best_models_reactive <- reactive ({
      comparison_df() %>%
        group_by(Model, Lags) %>%
        summarise(AvgError = round(mean(ErrorRate),2), .groups = "drop") 
    })
    output$order_forecast_dygraph_plot <- renderDygraph({
      
      
      best_preds <- comparison_df() %>%
        inner_join(best_models_reactive(), by = c("Model", "Lags")) %>%
        mutate(Type = paste0(Model, " (Lag ", Lags, ")")) %>%
        select(Date, Value = Predicted, Type)
      
      historical <- monthly_orders() %>%
        filter(year <= 2017) %>%
        select(Date = ds, Value = y) %>%
        mutate(Type = "Historical")
      
      actual_df <- monthly_orders() %>%
        filter(year == 2018, month <= 8) %>%
        select(Date = ds, Value = y) %>%
        mutate(Type = "Actual_2018")
      
      plot_data <- bind_rows(historical, actual_df, best_preds)
      
      ribbon_data <- best_preds %>%
        group_by(Date) %>%
        summarise(ymin = min(Value), ymax = max(Value), .groups = "drop")
      
      wide_data <- plot_data %>%
        pivot_wider(names_from = Type, values_from = Value) %>%
        left_join(ribbon_data, by = "Date") %>%
        arrange(Date)
      
      plot_columns <- names(wide_data)[!(names(wide_data) %in% c("Date", "ymin", "ymax"))]
      ts_data <- xts(wide_data[, plot_columns], order.by = wide_data$Date)
      
      n_series <- length(plot_columns)
      color_palette <- c("gray20")
      available_colors <- brewer.pal(min(8, n_series - 1), "Dark2")
      color_palette <- c(color_palette, available_colors)
      
      if (length(color_palette) < n_series) {
        extra_colors <- colorRampPalette(brewer.pal(8, "Set3"))(n_series - length(color_palette))
        color_palette <- c(color_palette, extra_colors)
      }
      
      dy <- dygraph(ts_data)
      for (i in seq_along(plot_columns)) {
        dy <- dy %>% dySeries(plot_columns[i], label = plot_columns[i], color = color_palette[i])
      }
      
      dy %>%
        dyOptions(drawPoints = FALSE, strokeWidth = 2) %>%
        dyLegend(show = "follow", width = 400)
    })
    
    # Download handler
    output$model_download_results <- downloadHandler(
      filename = function() {
        paste("ModelResults", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(comparison_df(), file, row.names = FALSE)
      }
    )
    output$best_models_table <-  renderReactable({
      reactable(
        best_models_reactive() %>% arrange(AvgError),
        bordered = FALSE,
        striped = TRUE,
        highlight = TRUE,
        fullWidth = TRUE,
        defaultPageSize = 3,
        theme = reactableTheme(
          borderColor = "#eee",
          stripedColor = "#f9f9f9",
          highlightColor = "#f1f1f1",
          cellPadding = "8px 12px",
          style = list(fontFamily = "system-ui", fontSize = "14px")
        )
      )
    })
    
    ######Summary Installatment Table---------
    output$spearman_correlation <- renderText({
      cor_result <- cor(
        order_payment_info()$installments,
        order_payment_info()$order_value,
        method = "spearman",
        use = "complete.obs"
      )
      
      paste("Spearman Correlation (Installments vs Order Value):", round(cor_result, 3))
    })
    output$summary_installment_table <- renderReactable({
      reactable(summaryinstallment_table(),
                columns = list(
                  installments = colDef(name = "Installments"),
                  avg_order_value = colDef(name = "Avg Order Value ($)", align = "right"),
                  n_orders = colDef(name = "Order Count", align = "center")
                ),
                bordered = TRUE,
                highlight = TRUE,
                defaultPageSize = 5,
                paginationType = "jump"
      )
      
    })
    
    
    
    ######-End of Server------------
  })
  
  
}
