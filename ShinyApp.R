#' PISA Shiny App
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
PISAShinyApp <- function(){
  require(shiny)
  require(tidyverse)
  # Define UI for application that draws a histogram
  ui <- fluidPage(    
    # Application title
    titlePanel("Student Shiny App"),
    sidebarPanel(
      # Radio Button to Choose Dataset
      radioButtons("radio", label = h6("Choose dataset:"),
                   choices = list("Student - QQQ" = 1, "Student - QQ2" = 2, "School" = 3, "Cognitive" = 4, "Timing" = 5, "Teacher" = 6, "Financial Literacy" = 7, "Collaborative Problem Solving" = 8), 
                   selected = NULL),
      selectInput("type","Type:",choices=c("Nominal","Ordinal","Number","Ratio"),selected="Nominal"),
      uiOutput("variable"),
      h6("Note: Students answered questions that begin with ST, IC or EC"),
      h6("Note: Parents answered questions that begin with PA")
    ),
    # Panel of Variable Description, Table of the Variable, Plot of the Variable
    mainPanel(
      tabsetPanel(
        tabPanel("Description", verbatimTextOutput("desc")),
        tabPanel("Table", verbatimTextOutput("tbl")),
        tabPanel("Plot",plotOutput("plot")),
        tabPanel("Plot",plotOutput("plot"))
      )
    )
  )
  
  # Define server logic required Description, Table and Plot
  server <- function(input, output) {
    
    #Choose the Selected Dataset
    datasetInput <- reactive({
      if (input$radio == 1){
        current_data <- stud2015
      } else if (input$radio == 2) {
        current_data <- stud2015_2
      } else if (input$radio == 3) {
        current_data <- sch2015
      } else if (input$radio == 4) {
        current_data <- cog2015
      } else if (input$radio == 5) {
        current_data <- tim2015
      } else if (input$radio == 6) {
        current_data <- tch2015
      } else if (input$radio == 7) {
        current_data <- lit2015
      } else {
        current_data <- cps2015
      }
      current_data
    })
    
    #Choose the Selected Dataset Codebook
    codebookInput <- reactive({
      if (input$radio == 1){
        current_book <- stud2015vars
      } else if (input$radio == 2) {
        current_book <- stud2015_2vars
      } else if (input$radio == 3) {
        current_book <- sch2015vars
      } else if (input$radio == 4) {
        current_book <- cog2015vars
      } else if (input$radio == 5) {
        current_book <- tim2015vars
      } else if (input$radio == 6) {
        current_book <- tch2015vars
      } else if (input$radio == 7) {
        current_book <- lit2015vars
      } else {
        current_book <- cps2015vars
      }
      current_book
    })
    
    #Choose the Selected Dataset Variable
    output$variable <- renderUI({
      data <- codebookInput()
      selectInput("variable",label="Variable:",choices=data$Names[data$Type==input$type],selected=data$Names[data$Type==input$type][1])
    })
    
    #Choose the Selected Variable Description
    output$desc <- renderText({
      codebk <- codebookInput()
      codebk$Description[codebk$Names==input$variable]
    })
    
    #Create the Selected Variable Table
    output$tbl <- renderPrint({
      data <- datasetInput()
      codebk <- codebookInput()
      if (codebk$Type[codebk$Names==input$variable] != "Number" & codebk$Type[codebk$Names==input$variable] != "Ratio") {
        # Create a Table of Values for the Character Variables
        curr_tbl <- table(data[,input$variable])
      } else {
        # Create a Numerical Summary for the Numerical Variables
        curr_tbl <- summary(as.numeric(data[,input$variable]))
      }
      curr_tbl
    })
    
    #Create the Selected Variable Plot
    output$plot <- renderPlot({
      data <- datasetInput()
      codebk <- codebookInput()
      if (codebk$Type[codebk$Names==input$variable] != "Number" & codebk$Type[codebk$Names==input$variable] != "Ratio") {
        # Obtain Variable Values from 'Table' for Chart
        tbl_names <- names(table(data[,input$variable])[order(-table(data[,input$variable]))])
        # Reduce the Varaible Values to the top 10 (to reduce size of plot)
        tbl_names_red <- tbl_names[1:min(length(tbl_names),10)]
        # Get the dataset of the Top 10 Values in the Variable
        dat <- data[data[,input$variable] %in% tbl_names_red,]
        # Create a Barplot of Values for the Character Variables
        curr_plot <- ggplot() + geom_bar(aes(x=dat[,input$variable])) + labs(x=input$variable)
      } else {
        # Create a Histogram of Values for the Numerical Variables
        curr_plot <- ggplot() + geom_histogram(aes(x=as.numeric(data[,input$variable]))) + labs(x=input$variable)
      }
      curr_plot
    })
  }
  
  # Bind ui and server together to Run the App
  shinyApp(ui, server)
}


