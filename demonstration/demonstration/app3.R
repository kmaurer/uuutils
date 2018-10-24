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
      radioButtons('method', label = 'Method', choices = list('Random' = 1, 'Most Uncertain' = 2, 'Facility Location' = 3), selected = 1),
      uiOutput('cc'),
      

      
      actionButton('go', "Start")
    ),
    
    mainPanel(
      
      fluidRow(column(7, imageOutput('query')),
               column(5, h3(textOutput('text3')))),
      fluidRow(
               column(2, actionButton('correct', "Correct")),
               column(2, actionButton('incorrect', "Incorrect")),

               column(2, textOutput('text')),
               column(2, textOutput('text2'))
               ),
      fluidRow(
               column(2, actionButton('backCorrect', "Redo")),
               column(2, actionButton('backIncorrect', "Redo"))
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

  output$cc <- renderUI({
    selectInput('criticalClass', label = 'Critical Class', choices = unique(dat()$PredictionName))
  })
  
  ##
  ## Track queries
  ##
  # X <- reactive(c(1:dim(dat())[1]))
  # X <- c(1:12499)[which(dat()$PredictionName == input$criticalClass)]
  X <- reactiveValues()
  X$values <- c(1)
  
  
  Q <- reactiveValues()
  Q$values <- c()
  
  S <- reactiveValues()
  S$values <- c()
  
  ##
  ## Sample images
  ##
  query <- function(){
    toReturn <- 1
    if(input$method == 1){      ## Random
      toReturn <- sample(X$values[! X$values %in% Q$values], 1)
    }
    if(input$method == 2){      ## Most uncertain
      
    }
    if(input$method == 3){      ## Facility location
      
    }
    return(toReturn)
  }

  current <- reactiveValues()
  current$values <- isolate(query())
  
  
  ##
  ## Start button
  ##
  observeEvent(input$go, {
    Q$values <- c()
    S$values <- c()
    X$values <- isolate(c(1:nrow(dat()))[dat()$PredictionName == input$criticalClass])
    current$values <- isolate(query())
  })
  
  ##
  ## Add to lists
  ##
  observeEvent(input$correct, {
    Q$values <- c(isolate(Q$values), current$values)
    current$values <- isolate(query())
  })
  
  observeEvent(input$incorrect, {
    Q$values <- c(isolate(Q$values), current$values)
    S$values <- c(isolate(S$values), current$values)
    current$values <- isolate(query())
  })
  

  
  observeEvent(input$backCorrect, {
     current$values <- isolate(Q$values[length(Q$values)])
     Q$values <- isolate(Q$values[-length(Q$values)])
   })
  
  observeEvent(input$backIncorrect, {
    current$values <- isolate(Q$values[length(Q$values)])
    Q$values <- isolate(Q$values[-length(Q$values)])
    S$values <- isolate(S$values[-length(S$values)])
  })
  
  
  
  output$text <- renderText(paste("Queried: ", length(Q$values), sep = ""))
  output$text2 <- renderText(paste("Uncovered: ", length(S$values), sep = ""))
  output$text3 <- renderText(paste("Predicted: ", dat()$PredictionName[current$values], "\n", "Confidence: ", round(dat()$Confidence[current$values], 2), sep = ""))
  
  
  
  ##
  ## Render selected image
  ##
  output$query <- renderImage({list(src = as.character(dat()$FileName[current$values]), width = 350, height = 350)}, deleteFile = FALSE)


}

# Run the application 
shinyApp(ui = ui, server = server)

