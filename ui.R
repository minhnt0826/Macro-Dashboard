ui <- 
dashboardPage(
  dashboardHeader(title = "Macro Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fed Surveys", tabName = "fed_surveys"),
      menuItem("Employment", tabName = "employment")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "fed_surveys",fedSurveysUI("fed"))
    )
  )
)
