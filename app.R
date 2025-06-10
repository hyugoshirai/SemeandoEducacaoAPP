# Note: not necessary to load packages here: this all happens in global.R

# Increase the file upload size limit to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

library(shiny)
# Run the application 
# shinyApp(ui = ui.R, server = server.R)
runApp()