library(shiny)
library(shinydashboard)
library(plotly)



source("serverModules/fedSurveysServer.R")
source("UIModules/fedSurveysUI.R")

source("serverModules/employmentServer.R")
source("UIModules/employmentUI.R")

source("serverModules/monetaryServer.R")
source("UIModules/monetaryUI.R")

source("serverModules/growthServer.R")
source("UIModules/growthUI.R")

source("serverModules/housingServer.R")
source("UIModules/housingUI.R")

source("serverModules/inflationServer.R")
source("UIModules/inflationUI.R")

# UK ####
source("UK/dataRetrievers/boeData.R")
source("UK/dataRetrievers/onsData.R")

source("UK/serverModules/monetary/UKMonetaryServer.R")
source("UK/UIModules/monetary/UKMonetaryUI.R")



