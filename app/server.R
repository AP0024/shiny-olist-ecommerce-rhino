server <- function(input, output, session) {

  # Session end cleanup
  onSessionEnded(function() {
    cat("Session ended. Cleaning up...\n")
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    gc()
  })
  
  # Load data reactively
  data <- reactive({
    load_olist_data()
  })
  
  # Track which sidebar tab is active
  current_tab <- reactiveVal("orders")
  
  observeEvent(input$menu_orders, {
    current_tab("orders")
  })
  
  observeEvent(input$menu_customers, {
    current_tab("customers")
  })
  
  observeEvent(input$menu_payments, {
    current_tab("payments")
  })
  
  observeEvent(input$menu_reports, {
    current_tab("reports")
  })
  
  # Output main content UI based on selected tab
  output$main_content <- renderUI({
    switch(current_tab(),
           "orders" = tagList(
             div(class = "demo",
                 h4("\U0001F4E6 Orders Overview"),
                 orders_ui("orders_module")
             )
           ),
           "customers" = div(class = "ui segment", h4("Customers"), p("This is a placeholder for customers.")),
           "payments" = div(class = "ui segment", h4("Payments"), p("This is a placeholder for payments.")),
           "reports" = div(class = "ui segment", h4("Reports"), p("This is a placeholder for reports."))
    )
  })
  
  # Module servers
  orders_server("orders_module", data)
  # customers_server("customers_module", reactive(data()$customers))
  # payments_server("payments_module", reactive(data()$payments))
}
