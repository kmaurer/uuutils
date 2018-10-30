library(shiny)
library(ggplot2)
library(dplyr)
source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/bansal_weld_functions.R")
source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/phi_functions.R")
source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/facility_locations_utility.R")
source("/home/bennettew/Documents/Summer2018/unknownUnknowns/uuutils/bandits.R")

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
      ),
      
      hr(),
      
      fluidRow(
        column(12, plotOutput('plot'))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ##
  ## Load input data
  ##
  
  ##
  ## WE ASSUME THAT CONFIDENCE = 1 MEANS 100% SURE, 0 MEANS 0% SURE 
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
  X <- reactiveValues()       ## X$values hold the indicies of the training set
  X$values <- c(1)            ## Preload it with a dummy value to overwrite later
  X$dat <- c()
  
  
  Q <- reactiveValues()       ## Q$values hold the indicies of the queried points
  Q$values <- c()
  Q$confidence <- c()
  Q$wrong <- c()
  
  All <- reactiveValues()       ## Will hold information for facility locations search
  All$c_MX <- c()               ## Will hold confidence measures for facility locations
  All$true_misclass <- c()      ## Will hold misclassified for facility locations
  All$D_test <- c()             ## Will hold features for facility locations
  All$dist_mat <- c()           ## Will hold distances for facility locations
  
  S <- reactiveValues()       ## S$values hold the indicies of the unknown unknowns
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
      
      ## 
      ## Calculate Phi
      ## 
      print("start Phi")
      phi_D_test <- phi_all(All$D_test, All$c_MX, All$true_misclass, Q$values,
                            tau=.65, prior=rep(.5, length(All$D_test)), phi_mod_type="logistic",
                            clust_out = NULL, updateprior=FALSE, lambda=2)
      print("end Phi")
      
      print("start select")
      toReturn <- best_candidate(Q$values, All$true_misclass, phi_D_test, All$dist_mat, All$c_MX, NULL)
      print("end select")
    }
    return(toReturn)
  }

  current <- reactiveValues()
  current$values <- isolate(query())
  
  
  ##
  ## Start button
  ##
  observeEvent(input$go, {
    
    X$dat <- dat() %>% filter(PredictionName == input$criticalClass) %>%         ## Filter data
                       filter(Confidence > 0.65)
    X$values <- c(1:nrow(X$dat))                                                 ## Create indicies
    
    Q$values <- c()                                                              ## Clear out previous queries
    Q$confidence <- c()
    Q$wrong <- c()
    S$values <- c()
    
    if(input$method == 3){                                                       ## Collect information if for facility location
      All$c_MX <- X$dat$Confidence
      All$true_misclass <- rep(0, nrow(X$dat))
      All$D_test <- X$dat %>% select(-Prediction, -Confidence, -True.Label, -Misclassified, -FileName, -PredictionName)
      All$dist_mat <- make_dist_mat(All$D_test)
    }
    
    
    current$values <- isolate(query())
  })
  
  ##
  ## Add to lists
  ##
  observeEvent(input$correct, {
    Q$values <- c(isolate(Q$values), current$values)
    Q$confidence <- c(isolate(Q$confidence), X$dat$Confidence[current$values])
    Q$wrong <- c(isolate(Q$wrong), FALSE)
    current$values <- isolate(query())
  })
  
  observeEvent(input$incorrect, {
    Q$values <- c(isolate(Q$values), current$values)
    Q$confidence <- c(isolate(Q$confidence), X$dat$Confidence[current$values])
    Q$wrong <- c(isolate(Q$wrong), TRUE)
    S$values <- c(isolate(S$values), current$values)
    All$true_misclass[current$values] <- 1 
    current$values <- isolate(query())
  })
  

  
  observeEvent(input$backCorrect, {
     current$values <- isolate(Q$values[length(Q$values)])
     Q$values <- isolate(Q$values[-length(Q$values)])
     Q$confidence <- isolate(Q$confidence[-length(Q$confidence)])
     Q$wrong <- isolate(Q$wrong[-length(Q$wrong)])
   })
  
  observeEvent(input$backIncorrect, {
    current$values <- isolate(Q$values[length(Q$values)])
    Q$values <- isolate(Q$values[-length(Q$values)])
    Q$confidence <- isolate(Q$confidence[-length(Q$confidence)])
    Q$wrong <- isolate(Q$wrong[-length(Q$wrong)])
    S$values <- isolate(S$values[-length(S$values)])
    All$true_misclass[current$values] <- 0
  })
  
  
  
  output$text <- renderText(paste("Queried: ", length(Q$values), sep = ""))
  output$text2 <- renderText(paste("Uncovered: ", length(S$values), sep = ""))
  output$text3 <- renderText(paste("Predicted: ", X$dat$PredictionName[current$values], "\n", "Confidence: ", round(X$dat$Confidence[current$values], 3), sep = ""))
  
  
  
  ##
  ## Render selected image
  ##
  output$query <- renderImage({list(src = as.character(X$dat$FileName[current$values]), width = 350, height = 350)}, deleteFile = FALSE)

  
  ##
  ## Plot utility 
  ##
  output$plot <- renderPlot({
    
    SDR <- c()
    for(i in c(1:length(Q$values))){
      value <- sum(Q$wrong[1:i])/(length(Q$values[1:i]) - sum(Q$confidence[1:i]))
      SDR <- c(SDR, value)
    }
    
    dat <- data.frame(Budget = c(1:length(Q$values)), SDR = SDR)
    
    ggplot(dat, aes(x = Budget, y = SDR)) + 
      geom_line(color = 'red', size = 2) +
      scale_x_continuous(limits = c(1, 100)) +
      theme_bw() +
      ggtitle("Standardized Discovery Ratio") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

