ui <- semanticPage(
  title = "Olist Dashboard",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css")
  ),
  # Sidebar
  div(
    style = "width: 220px; height: 100vh; position: fixed; left: 0; top: 0; background-color: #f4f4f4; padding: 30px 20px; box-shadow: 4px 0 20px rgba(0, 0, 0, 0.1);",
    h3("\U0001F4CA Olist Dashboard"),
    div(class = "ui vertical menu",
        actionLink("menu_orders", "\U0001F4E6 Orders", class = "item"),
        actionLink("menu_customers", "\U0001F465 Customers", class = "item"),
        actionLink("menu_payments", "\U0001F4B3 Payments", class = "item"),
        actionLink("menu_reports", "\U0001F4C8 Reports", class = "item")
    )
  ),
  
  # Main Panel - Conditional Display
  div(
    style = "margin-left: 250px; padding: 40px;",
    uiOutput("main_content")
  )
)
