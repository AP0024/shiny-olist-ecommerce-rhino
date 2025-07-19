# app/app.R


# Load any global settings
source("global.R")
# Load UI and server
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
