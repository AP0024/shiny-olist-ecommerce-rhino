# modules/orders.R

orders_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "ui three column grid",
        div(class = "column",
            div(class = "ui segment glass-segment info-card",
                h4("Total Orders"),
                textOutput(ns("total_orders"))
            )
        ),
        div(class = "column",
            div(class = "ui segment glass-segment info-card",
                h4("Unique Customers"),
                textOutput(ns("unique_customers"))
            )
        ),
        div(class = "column",
            div(class = "ui segment glass-segment info-card",
                h4("Avg Delivery Time (Days)"),
                textOutput(ns("avg_delivery_days"))
            )
        )
    ),
    
    div(class = "glass-segment",
        h4("📈 Orders per Month"),
        plotOutput(ns("orders_trend"))
    ),
    
    div(class = "glass-segment",
        h4("📋 Order Details"),
        dataTableOutput(ns("orders_table"))
    )
  )
}

orders_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$total_orders <- renderText({
      nrow(data())
    })
    
    output$unique_customers <- renderText({
      length(unique(data()$customer_id))
    })
    
    output$avg_delivery_days <- renderText({
      mean(as.numeric(difftime(data()$order_delivered_customer_date,
                               data()$order_purchase_timestamp,
                               units = "days")), na.rm = TRUE)
    })
    
    output$orders_trend <- renderPlot({
      data() %>%
        mutate(month = format(as.Date(order_purchase_timestamp), "%Y-%m")) %>%
        group_by(month) %>%
        summarise(orders = n()) %>%
        ggplot(aes(x = month, y = orders)) +
        geom_line(color = "#2185D0") +
        theme_minimal() +
        labs(x = "Month", y = "Number of Orders")
    })
    
    output$orders_table <- renderDataTable({
      data()
    })
  })
}
