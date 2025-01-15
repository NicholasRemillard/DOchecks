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
      tableOutput("view"),
      plotlyOutput("my_plot")
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
  
  data1 <- reactive({
    req(input$video1_info)
    inFile <- input$video1_info
    if (is.null(inFile))
      return(NULL)
    
    file_name <- input$video1_info$name
    file_info <- get_video_info_shiny(file_name)
    vid_name <- paste(file_info$initials, file_info$vid_num, sep=" - ")
    
    video1_raw_frame <- read_excel(inFile$datapath)
    video1_frames <- ready_to_plot_shiny(video1_raw_frame, vid_name)
    check_transitions(video1_frames$beh_frame)
  })
  
  # data2 <- reactive({
  #   req(input$video2_info)
  #   file_name2 <- input$video2_info$name
  #   file_info2 <- get_video_info_shiny(file_name2)
  #   vid_name2 <- paste(file_info2$initials, file_info2$vid_num, sep=" - ")
  #     
  #   inFile2 <- input$video2_info
  #   if (is.null(inFile2))
  #     return(NULL)
  #   video2_raw_frame <- read_excel(inFile2$datapath)
  #     
  #   video2_frames <- ready_to_plot_shiny(video2_raw_frame, vid_name2)
  # })
  # 
  output$view <- renderTable({
    req(data1())
  })
  
  output$my_plot <- renderPlotly({
    req(input$video1_info)
    # Get video1 info
    file_name <- input$video1_info$name
    file_info <- get_video_info_shiny(file_name)
    vid_name <- paste(file_info$initials, file_info$vid_num, sep=" - ")
    
    inFile <- input$video1_info
    if (is.null(inFile))
      return(NULL)
    video1_raw_frame <- read_excel(inFile$datapath)
    video1_frames <- ready_to_plot_shiny(video1_raw_frame, vid_name)
    
    # Get video2 info, if provided
    if (!is.null(input$video2_info)) {
      file_name2 <- input$video2_info$name
      file_info2 <- get_video_info_shiny(file_name2)
      vid_name2 <- paste(file_info2$initials, file_info2$vid_num, sep=" - ")
      
      inFile2 <- input$video2_info
      video2_raw_frame <- read_excel(inFile2$datapath)
      
      video2_frames <- ready_to_plot_shiny(video2_raw_frame, vid_name2)
      
      # Make plots for both videos
      plot_beh <- plot_annotation_shiny(vid1_name = vid_name, vid2_name = vid_name2,
                                        video1_data = video1_frames$beh_frame,
                                        video2_data = video2_frames$beh_frame)
      
      plot_mod <- plot_annotation_shiny(vid1_name = vid_name, vid2_name = vid_name2,
                                        video1_data = video1_frames$mod_frame,
                                        video2_data = video2_frames$mod_frame)
      
      plot_beh_int <- plot_annotation_shiny(vid1_name = vid_name, vid2_name = vid_name2,
                                            video1_data = video1_frames$beh_int_frame,
                                            video2_data = video2_frames$beh_int_frame)
      
      plot_mod_int <- plot_annotation_shiny(vid1_name = vid_name, vid2_name = vid_name2,
                                            video1_data = video1_frames$mod_int_frame,
                                            video2_data = video2_frames$mod_int_frame)
      
      # Create ggplotly objects
      make_interactive_plots(plot_beh, plot_beh_int, plot_mod, plot_mod_int) %>% layout(height = 1600)
      
    }else{
      # Make plot for video1 only
      plot_beh <- plot_annotation_shiny(vid1_name = vid_name,
                                        video1_data = video1_frames$beh_frame)
      
      plot_mod <- plot_annotation_shiny(vid1_name = vid_name,
                                        video1_data = video1_frames$mod_frame)
      
      plot_beh_int <- plot_annotation_shiny(vid1_name = vid_name,
                                            video1_data = video1_frames$beh_int_frame)
      
      plot_mod_int <- plot_annotation_shiny(vid1_name = vid_name,
                                            video1_data = video1_frames$mod_int_frame)
      
      # Create ggplotly objects
      make_interactive_plots(plot_beh, plot_beh_int, plot_mod, plot_mod_int) %>% layout(height = 1600)
    }
    
  }) # End of plot output code
  
}

shinyApp(ui = ui, server = server)