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
ui <- fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1','Choose file to upload',
                accept = c('text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv')
                ),
      tags$hr(),
      heckboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='','Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      p('If you want a sample .csv or .tsv file to upload,',
          'you can first download the sample',
        a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
        a(href = 'pressure.tsv', 'pressure.tsv'),
        'files, and then try uploading them.')),
    mainPanel(
      tableOutput('contents')
      )
    )
  )

server <- function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
}
# Run the application 
shinyApp(ui, server)