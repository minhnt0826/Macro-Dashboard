server <- function(input, output) {
  fedSurveyServer("fed")
  employmentServer("employment")
  monetaryServer("monetary")
  growthServer("growth")
  housingServer("housing")  
  inflationServer("inflation")
}
