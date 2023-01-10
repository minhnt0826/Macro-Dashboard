


# Shiny server ####
employmentServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$employment_plot1 <- renderPlotly({
        employment_plot
      })
      
    }
  )
}