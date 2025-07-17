ui <- semanticPage(
  title = "Olist Dashboard (Glassmorphism)",
  
  # Glassmorphism CSS
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #c3cfe2, #c3cfe2);
        font-family: 'Segoe UI', sans-serif;
        margin: 0;
      }

      .glass-sidebar {
        width: 220px;
        height: 100vh;
        position: fixed;
        left: 0;
        top: 0;
        background: rgba(255, 255, 255, 0.1);
        backdrop-filter: blur(10px);
        -webkit-backdrop-filter: blur(10px);
        padding: 30px 20px;
        box-shadow: 4px 0 20px rgba(0, 0, 0, 0.1);
      }

      .glass-sidebar h3 {
        color: #333;
        font-weight: bold;
      }

      .nav-item {
        padding: 12px 10px;
        color: #222;
        border-radius: 10px;
        transition: all 0.2s ease-in-out;
        cursor: pointer;
      }

      .nav-item:hover {
        background: rgba(255, 255, 255, 0.3);
      }

      .main-content {
        margin-left: 250px;
        padding: 40px;
      }

      .glass-card {
        background: rgba(255, 255, 255, 0.15);
        backdrop-filter: blur(12px);
        -webkit-backdrop-filter: blur(12px);
        border-radius: 16px;
        padding: 25px;
        margin-bottom: 30px;
        box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
        border: 1px solid rgba(255, 255, 255, 0.3);
      }

      .glass-card h4 {
        color: #222;
        font-weight: 600;
      }
    "))
  ),
  
  # Sidebar
  div(class = "glass-sidebar",
      h3("📊 Olist Dashboard"),
      div(class = "nav-item", "📦 Orders"),
      div(class = "nav-item", "👥 Customers"),
      div(class = "nav-item", "💳 Payments"),
      div(class = "nav-item", "📈 Reports")
  ),
  
  # Main Panel
  div(class = "main-content",
      div(class = "glass-card",
          h4("📦 Orders Overview"),
          orders_ui("orders_module")  # Module to be defined in server + modules/orders.R
      ),
      div(class = "glass-card",
          h4("📈 Weekly Trends"),
          plotOutput("trend_plot")  # Example placeholder
      )
  )
)

