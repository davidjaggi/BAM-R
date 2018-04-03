source("Installer.R")

ui <- dashboardPage(
  # Start DashBoard Header
  header, # end header
  # start sidebar
  sidebar, # end sidebar
  # start body
  dashboardBody(
    tabItems(is, oos, comparison) # end tabitems
  ) # end body
) # end ui
