#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  titlePanel("Uncovering Unknown Unknowns"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput('file1', 'Choose Input Data'),
      radioButtons('method', label = h4('Method'), choices = list('Random' = 1, 'Most Uncertain' = 2, 'Facility Location' = 3), selected = 1)
    ),
    
    mainPanel(
      imageOutput('query'), offset = 1)
    )
    
    

  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ##
  ## Load input data
  ##
  dat <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  
  output$query <- renderImage(src = dat()$FileName[1], deleteFile = FALSE)
  
  


}

# Run the application 
shinyApp(ui = ui, server = server)

