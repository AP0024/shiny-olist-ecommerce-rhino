# modules/orders.R

orders_ui <- function(id) {
  ns <- NS(id)
  grid(
    grid_template(
      default = list(
        areas = rbind(
          c("header"),
          c("main")
        ),
        rows_height = c("auto", "auto"),
        cols_width  = c("1fr")
      )
    ),
    
    container_style = "padding: 20px; border: 3px solid #e0e0e0;",
    
    # ────────────────
    # HEADER: KPI Cards
    header = div(
      class = "ui stackable doubling three column grid",
      style = "margin-bottom: 30px;",
      
      lapply(
        list(
          list(title = "\U0001F6D2 Total Orders", output_id = "total_orders"),
          list(title = "\U0001F6AB Cancellation Rate", output_id = "order_cancellation_rate"),
          list(title = "\u23f1 Avg Delivery Time (Days)", output_id = "avg_delivery_days")
        ),
        function(card) {
          div(
            class = "column",
            div(
              class = "ui raised segment glass-segment info-card",
              div(
                style = "font-size: 14px; color: #00000; margin-bottom: 10px;",
                h4(card$title)
              ),
              div(
                style = "font-size: 14px; font-weight: bold;",
                textOutput(ns(card$output_id))
              )
              # Conditionally add a line chart only for Total Orders
            )
          )
        }
      )
    ),
    
    
    # ────────────────
    # MAIN GRID (Nested)
    main = div(
      style = "display: flex; flex-direction: column; gap: 20px;",
      # ─── Top Row (Gauge + Insight) ───
      grid(
        grid_template(
          default = list(
            areas = rbind(c("top_left", "top_right")),
            cols_width = c("1fr", "2fr"),
            rows_height = c("300px")
          )
        ),
        container_style = "gap: 20px;",
        area_styles = list(
          top_left  = "border: 2px solid #e0e0e0; padding: 15px;",
          top_right = "border: 2px solid #e0e0e0; padding: 15px;"
        ),
        
        top_left =div(
          class = "ui segment glass-segment",
          h4("Average Order Value"),
          br(),
          plotlyOutput(ns("aov_gauge"), height = "200px")
        ),
        
        top_right = div(
          class = "ui segment glass-segment",
          div(
            class = "ui fluid styled accordion",
            div(class = "title active", tags$i(class = "dropdown icon"), "🧠 AOV Insight"),
            div(class = "content active", textOutput(ns("aov_insight")), style = "padding-top: 10px;")
          )
        )
      ),
      # ✅ Deviation Orders Plot (Placed After Top, Before Middle)
      div(class = "row",
          div(class = "ui segment glass-segment",
              div(class = "row",
                  h4("Deviation from Mean Orders"),
                  
                  # plotlyOutput(ns("devitation_orders_plotly")),
                  highchartOutput(ns("deviation_orders_highcharter")),
                  
                  DTOutput(ns("devitationSummary"))
                  
              )
          )
      ),
      # ─── Middle  ───
      grid(
        grid_template(
          default = list(
            areas = rbind(c("middle_left", "middle_right")),
            cols_width = c("1fr", "1fr"),
            rows_height = c("auto")
          )
        ),
        container_style = "gap: 20px;",
        area_styles = list(
          middle_left  = "border: 2px solid #e0e0e0; padding: 15px;",
          middle_right = "border: 2px solid #e0e0e0; padding: 15px;"
        ),
        middle_left = div(
          class = "ui segment glass-segment",
          h4("Do higher-value orders tend to have more installments?"),
          textOutput(ns("spearman_correlation")),
          br(),
          reactableOutput(ns("summary_installment_table")),
        ),
        middle_right = div(
          class = "ui segment glass-segment",
          h4("Top 5 Cities by Orders"),
          leafletOutput(ns("order_city_map"), height = 300)
        )
      ),
      
      
      # ─── Bottom  ───
      
      
      
      grid(
        grid_template(
          default = list(
            areas = rbind(c("bottom_left", "bottom_right")),
            cols_width = c("1fr", "2fr"),
            rows_height = c("auto")
          )
        ),
        container_style = "gap: 20px;",
        area_styles = list(
          bottom_left  = "border: 2px solid #e0e0e0; padding: 20px;",
          bottom_right = "border: 2px solid #e0e0e0; padding: 20px;"
        ),
        
        # LEFT AREA: Inputs + Title
        bottom_left = div(
          class = "ui segment glass-segment",
          h4("Forecast Model Inputs"),
          div(class = "ui form",
              div(style = "display: flex; flex-direction: column; gap: 16px;",
                  
                  # Forecast Horizon
                  div(class = "field",
                      tags$label("Forecast Horizon"),
                      numericInput(ns("forecast_horizon"), label = NULL, value = 5, min = 1, max = 30)
                  ),
                  
                  # Lags
                  div(class = "field",
                      tags$label("Lags"),
                      numericInput(ns("forecast_lags"), label = NULL, value = 3, min = 1, max = 5)
                  ),
                  
                  # Model Selection
                  div(class = "field",
                      tags$label("Model"),
                      multiple_checkbox(ns("model_choice"), label = NULL,
                                        choices = list(
                                          "Linear Regression" = "lm",
                                          "Random Forest" = "rf",
                                          "XGBoost" = "xgb"
                                        ),
                                        selected = c("lm","rf", "xgb")
                      )
                  ),
                  
                  # Submit Button
                  div(class = "field",
                      actionButton(ns("submit_forecast"), "Forecast", class = "ui primary button")
                  )
              )
          )
        ),
        
        # RIGHT AREA: Plot + Table
        bottom_right = div(
          class = "ui segment glass-segment",
          
          # Plot
          div(
            h4("Forecast Output"),
            dygraphOutput(ns("order_forecast_dygraph_plot"), height = "120px")
          ),
          
          # Table
          div(
            style = "margin-top: 30px;",
            
            # Flex row with space between: Title on left, Button on right
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              
              h4("Model Comparison Table"),
              
              downloadButton(ns("model_download_results"), "Export Results")
            ),
            
            # Reactable table below
            reactableOutput(ns("best_models_table"))
          )
          
        )
      )
      
      
      
    )
    #####End of Main Grid-----
  )
}


