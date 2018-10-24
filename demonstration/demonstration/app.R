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
      
      fluidRow(column(12, imageOutput('query'))),
      fluidRow(
               column(2, actionButton('correct', "Correct")),
               column(2, actionButton('incorrect', "Incorrect")),
               column(2, textOutput('text'))
               )
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ##
  ## Load input data
  ##
  dat <- reactive({
    req(input$file1)
    return(read.csv(input$file1$datapath))
  })
  

  
  ##
  ## Select image
  ##
  X <- reactive(c(1:dim(dat())[1]))
  Q <- c(1, 2, 3)
  
  
  
  query <- reactive({
    toReturn <- 1
    if(input$method== 1){      ## Random
      toReturn <- sample(X()[! X() %in% Q], 1)
    }
    if(input$method == 2){      ## Most uncertain
      
    }
    if(input$method == 3){      ## Facility location
      
    }
    return(toReturn)
  })

  
  
  
  output$text <- renderText(paste(Q, collapse = ", "))
  
  ##
  ## Render selected image
  ##
  output$query <- renderImage({list(src = as.character(dat()$FileName[query()]), width = 350, height = 350)}, deleteFile = FALSE)


}

# Run the application 
shinyApp(ui = ui, server = server)

