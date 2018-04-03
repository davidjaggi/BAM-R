sidebar <- dashboardSidebar(sidebarMenu(
  menuItem('In-Sample', tabName = 'is'),
  menuItem('Out of Sample', tabName = 'oos'),
  menuItem('Comparison', tabName = 'comparison')
) # end menu
)