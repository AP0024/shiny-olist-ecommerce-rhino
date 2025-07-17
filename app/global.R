# List of required packages
packages <- c("shiny", "dplyr", "readr", "rmarkdown","shiny.semantic","tidyverse")

# Function to install if missing and then load
install_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Apply to all packages
invisible(lapply(packages, install_and_load))


# Load reusable functions
source("utils/load_data.R")

# Load all datasets
# data <- load_olist_data()

#######Load Modules-------------
source("modules/orders.R")
