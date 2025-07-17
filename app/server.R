server <- function(input, output, session) {
  
  onSessionEnded(function() {
    cat("Session ended. Cleaning up...\n")
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    gc()
  })
  
  
  
  data <- reactive({
    load_olist_data()
  })
  
  orders_server("orders_module", reactive(data()$orders))
}