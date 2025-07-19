#######------ Install Renv----------
# "renv makes sure your R project always works the same — no matter when or where you run it."
install.packages("renv")
#######------ Install Rhino Framework For Shiny APP----------
install.packages("rhino")
#######------ Initialize Rhino----------
rhino::init()



####Create data folder to store the data-----------
dir.create("data")

# The utils/ folder is used to store reusable helper functions 
# so your app code stays clean, organized, and easy to manage.

# Purpose	Example	Why It Helps
# ✅ Reuse logic	load_olist_data() to load all CSVs	Avoid rewriting code in multiple places
# ✅ Clean separation	get_top_categories() used in different modules	Keeps business logic out of server.R or global.R
# ✅ Reporting helpers	generate_summary_table(data) for PDF reports	Enables easy report customization
# ✅ Formatting functions	format_currency() or format_date()	Useful for UI output consistency
# ✅ Centralize constants	Store KPI thresholds, color palettes, etc.	Update in one place, affect whole app

dir.create("app/utils")

#####-------All Packages in global.R
file.create("app/global.R")

####_RUnEVERYTHING----------
file.create("app/app.R")
#######-----UI Layout-------------

file.create("app/ui.R")
#######-----Backend Logic---------
file.create("app/server.R")

#######-----Create Modules Folders---------

dir.create("app/modules")
file.create("app/modules/orders.R")





#######------ Initiating Renv for Maintaining the Version of Package ----------
# renv::init()

#######------ Snapshot pacakges Renv for Maintaining the Version of Package ----------
renv::snapshot()

######------ For teammates/deploy:------------
# renv::restore()



#git branch

#switch
# git checkout -b feature-branch-name   # Create & switch
# # OR
# git checkout feature-branch-name      # If it already exists
# git add .
# git commit -m "Your commit message describing the changes"
# git push origin feature-branch-name
# git checkout master
# git pull origin master
# git merge feature-branch-name
# git push origin master




