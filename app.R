source("global.R")
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Macro dashboard"),
  dashboardSidebar(
    selectInput("country", "Select a country", c("US", "UK")),
    sidebarMenuOutput("menu")
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

server <- function(input, output) {
  observe({
    if (input$country == "US"){
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Fed Surveys", tabName = "fed_surveys"),
          menuItem("Employment", tabName = "employment"),
          menuItem("Monetary", tabName = "monetary"),
          menuItem("Growth", tabName = "growth"),
          menuItem("Housing", tabName = "housing")
        )
      })
    }
    else if(input$country == "UK"){
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("CBI Surveys", tabName = "employment")
        )
      })
    }
  })
  
  fedSurveyServer("fed")
  employmentServer("employment")
  monetaryServer("monetary")
  growthServer("growth")
  housingServer("housing")  
  inflationServer("inflation")
}


shinyApp(ui, server)

