#-------------------------------------------------------------------------------
#
# This Shiny app takes an input of CGM data files and returns an ambulatory 
# glucose profile along with various CGM metrics. 
#
# v 1.0 
# Tim Vigers 
# 2/21/19
#
#-------------------------------------------------------------------------------

library(shiny)

# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.txt'
                  )
      )
      ),
      mainPanel(tableOutput('contents'))
      )
    )
server <- function(input, output) {
  inFile <- eventReactive({input$file1})
  cgmtable <- reactive({read.csv(inFile$datapath, header = T,sep = ",")})
  output$contents <- renderTable({
    if (is.null(inFile))
      return(NULL)
    cgmtable()
  })
}
# Run the application 
shinyApp(ui, server)
