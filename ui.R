# git push -u origin master
source('Installer.R')

ui <- dashboardPage(
  # Start DashBoard Header
  dashboardHeader(title = 'BAM'), # end header
  # start sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem('Timeseries', tabName = 'timeSeries'),
      menuItem('Tests', tabName = 'tests'),
      menuItem('Forecast', tabName = 'forecast')
    ) # end menu
  ), # end sidebar
  # start body
  dashboardBody(
    tabItems(
      tabItem('data'),
      tabItem('tests'),
      tabItem('forecast')
    ) # end tabitems
  ) # end body
) # end ui