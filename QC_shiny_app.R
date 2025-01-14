rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(svDialogs)
library(gridExtra)
library(stringr)
library(plotly)
library(irr)
library(shiny)

source("DOchecks_functions.R")

ui <- fluidPage(
  # UI elements go here
  titlePanel("Quality Control Assessment"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("video1_info",
                "Select an annotated participant file.",
                accept = ".xlsx"
                ),
      selectInput("compare_files_yesno",
                  "Are you comparing two files?",
                  c("No, one video only" = "no", "Yes" = "yes")),
      conditionalPanel(
        condition = "input.compare_files_yesno == 'yes'",
        fileInput("video2_info",
                  "Select second annotated participant file.",
                  accept = ".xlsx"
        )
      )
    ),
    mainPanel(
      textOutput("data_info"),
      tableOutput("view")
    )
  )
)

server <- function(input, output) {
  # Server logic goes here
  
  output$data_info <- renderText({
    req(input$video1_info)
    file_name <- input$video1_info$name
    file_info <- get_video_info_shiny(file_name)
    paste(file_info$initials, file_info$vid_num, sep=" - ")
  })
  
  data <- reactive({
    req(input$video1_info)
    inFile <- input$video1_info
    if (is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath)
  })
  
  output$view <- renderTable({
    req(data())
    head(data())
  })

  
}

shinyApp(ui = ui, server = server)