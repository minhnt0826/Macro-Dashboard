ui <- 
dashboardPage(
  dashboardHeader(title = "Macro Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fed Surveys", tabName = "fed_surveys"),
      menuItem("Employment", tabName = "employment"),
      menuItem("Monetary", tabName = "monetary"),
      menuItem("Growth", tabName = "growth"),
      menuItem("Housing", tabName = "housing"),
      menuItem("Inflation", tabName = "inflation")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "fed_surveys", fedSurveysUI("fed")),
      tabItem(tabName = "employment", employmentUI("employment")),
      tabItem(tabName = "monetary", monetaryUI("monetary")),
      tabItem(tabName = "growth", growthUI("growth")),
      tabItem(tabName = "housing", housingUI("housing")),
      tabItem(tabName = "inflation", inflationUI("inflation"))
    )
  )
)
