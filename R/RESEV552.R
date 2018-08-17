#' @name PISAShinyApp
#' @title PISA Shiny App
#' @description This is a Shiny App which describes all of the PISA 2015 datasets
#'
#' @return A Shiny App with choices of Dataset, Type of Variable and Specific Variable
#'
#' @format Three Shiny Outputs about the Variable Choice for a Specific Dataset
#' \describe{
#' \item{Description}{A Brief Description of the Chosen Variable}
#' \item{Table}{A Table of the Responses for the Chosen Variable (Summary of the Repsonses if the Variable is Numeric)}
#' \item{Plot}{A Bar Plot of the 10 Most Popular Repsonses for the Chosen Variable (Histogram if the Variable is Numeric)}
#' \item{Plot}{A Bar Plot of the 10 Most Popular Repsonses for the Chosen Variable (Histogram if the Variable is Numeric)}
#' }
#' @export
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


#' @name PISAData
#' @title PISA Data
#'
#' @description This is a function to download the PISA 2015 Data from the "mstuart2097/RESEV552Data" repository in Github
#' @return The eight datasets into your personal workspace
#'
#' @export
PISAData <- function(){
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_STU_QQQ.sav?raw=True"
  eval(parse(text="stud2015 <- read.spss(url)"))
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_STU_QQ2.sav?raw=True"
  eval(parse(text="stud2015_2 <- read.spss(url)"))
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_SCH_QQQ.sav?raw=True"
  eval(parse(text="sch2015 <- read.spss(url)"))
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_STU_COG.sav?raw=True"
  eval(parse(text="cog2015 <- read.spss(url)"))
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_STU_QTM.sav?raw=True"
  eval(parse(text="tim2015 <- read.spss(url)"))
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_TCH_QQQ.sav?raw=True"
  eval(parse(text="tch2015 <- read.spss(url)"))
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_STU_FLT.sav?raw=True"
  eval(parse(text="lit2015 <- read.spss(url)"))
  url <- "https://github.com/mstuart2097/RESEV552Data/blob/master/data/CY6_MS_CMB_STU_CPS.sav?raw=True"
  eval(parse(text="cps2015 <- read.spss(url)"))
}



