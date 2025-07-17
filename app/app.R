# app/app.R
onStart <- function() {
  cat("Session starting... clearing global environment.\n")
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  gc()
}

# Load any global settings
source("global.R")

# Load utility functions
source("utils/load_data.R")  # relative path since you're inside 'app/'

# Load UI and server
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
