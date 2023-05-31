library(shiny)
source('program/ui.R', local = TRUE)
source('program/server.R')

#thematic_shiny()

shinyApp(
  ui = ui,
  server = server
)