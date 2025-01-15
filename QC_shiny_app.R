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
      tabsetPanel(
        tabPanel("Info", uiOutput("name")),
        # tabPanel("Raw file", tableOutput("raw_table")),
        # tabPanel("All behavior table", tableOutput("beh_table")),
        # tabPanel("All modifier table", tableOutput("mod_table")),
        # tabPanel("Behavior of interest table", tableOutput("beh_int_table")),
        # tabPanel("Modifier of interest table", tableOutput("mod_int_table")),
        #tabPanel("Transition Table", tableOutput("view"))
        tabPanel("Plot", plotlyOutput("my_plot"))
      )
    )
  )
)

server <- function(input, output) {
  # Server logic goes here
  
  # Make re usable objects first
  vid_name <- reactive({
    req(input$video1_info)
    file_name <- input$video1_info$name
    file_info <- get_video_info_shiny(file_name)
    paste(file_info$initials, file_info$vid_num, sep=" - ")
  })
  
  vid2_name <- reactive({
    req(input$video2_info)
    file_name <- input$video2_info$name
    file_info <- get_video_info_shiny(file_name)
    paste(file_info$initials, file_info$vid_num, sep=" - ")
  })
  
  output$name <- renderUI({
    if(!is.null(input$video2_info)){
      HTML(paste("Video 1: ", vid_name(), "<br><br>", "Video 2: ", vid2_name()))
    }else{
      HTML(paste("Video 1: ", vid_name()))
    }
  })
  
  video1_raw_frame <- reactive({
    req(input$video1_info)
    inFile <- input$video1_info
    read_excel(inFile$datapath)
  })
  
  video2_raw_frame <- reactive({
    req(input$video2_info)
    inFile <- input$video2_info
    read_excel(inFile$datapath)
  })
  
  # output$raw_table <- renderTable({
  #   head(video1_raw_frame())
  # })
  
  video1_beh_frame <- reactive({
    req(video1_raw_frame(), vid_name())
    all_frames <- ready_to_plot_shiny(video1_raw_frame(), vid_name())
    all_frames$beh_frame
  })
  
  video2_beh_frame <- reactive({
    req(video2_raw_frame(), vid2_name())
    all_frames <- ready_to_plot_shiny(video2_raw_frame(), vid2_name())
    all_frames$beh_frame
  })
  
  # output$beh_table <- renderTable({
  #   req(video1_beh_frame())
  #   head(video1_beh_frame())
  # })
  
  video1_mod_frame <- reactive({
    req(video1_raw_frame(), vid_name())
    all_frames <- ready_to_plot_shiny(video1_raw_frame(), vid_name())
    all_frames$mod_frame
  })
  
  video2_mod_frame <- reactive({
    req(video2_raw_frame(), vid2_name())
    all_frames <- ready_to_plot_shiny(video2_raw_frame(), vid2_name())
    all_frames$mod_frame
  })
  
  # output$mod_table <- renderTable({
  #   req(video1_mod_frame())
  #   head(video1_mod_frame())
  # })
  
  video1_beh_int_frame <- reactive({
    req(video1_raw_frame(), vid_name())
    all_frames <- ready_to_plot_shiny(video1_raw_frame(), vid_name())
    all_frames$beh_int_frame
  })
  
  video2_beh_int_frame <- reactive({
    req(video2_raw_frame(), vid2_name())
    all_frames <- ready_to_plot_shiny(video2_raw_frame(), vid2_name())
    all_frames$beh_int_frame
  })
  
  # output$beh_int_table <- renderTable({
  #   req(video1_beh_int_frame())
  #   head(video1_beh_int_frame())
  # })
  
  video1_mod_int_frame <- reactive({
    req(video1_raw_frame(), vid_name())
    all_frames <- ready_to_plot_shiny(video1_raw_frame(), vid_name())
    all_frames$mod_int_frame
  })
  
  video2_mod_int_frame <- reactive({
    req(video2_raw_frame(), vid2_name())
    all_frames <- ready_to_plot_shiny(video2_raw_frame(), vid2_name())
    all_frames$mod_int_frame
  })
  
  # output$mod_int_table <- renderTable({
  #   req(video1_mod_int_frame())
  #   head(video1_mod_int_frame())
  # })
  
  output$my_plot <- renderPlotly({
    if(!is.null(input$video2_info)){
      req(video1_beh_frame(), video1_mod_frame(), video1_beh_int_frame(), video1_mod_int_frame(), vid_name(),
          video2_beh_frame(), video2_mod_frame(), video2_beh_int_frame(), video2_mod_int_frame(), vid2_name())
      
      plot_beh <- plot_annotation_shiny(vid1_name = vid_name(), vid2_name = vid2_name(),
                                        video1_data = video1_beh_frame(),
                                        video2_data = video2_beh_frame())
      
      plot_mod <- plot_annotation_shiny(vid1_name = vid_name(), vid2_name = vid2_name(),
                                        video1_data = video1_mod_frame(),
                                        video2_data = video2_mod_frame())
      
      plot_beh_int <- plot_annotation_shiny(vid1_name = vid_name(), vid2_name = vid2_name(),
                                            video1_data = video1_beh_int_frame(),
                                            video2_data = video2_beh_int_frame())
      
      plot_mod_int <- plot_annotation_shiny(vid1_name = vid_name(), vid2_name = vid2_name(),
                                            video1_data = video1_mod_int_frame(),
                                            video2_data = video2_mod_int_frame())
      
      # Create ggplotly objects
      make_interactive_plots(plot_beh, plot_beh_int, plot_mod, plot_mod_int) %>% layout(height = 1600)
    }else{
      req(video1_beh_frame(), video1_mod_frame(), video1_beh_int_frame(), video1_mod_int_frame(), vid_name())
      
      plot_beh <- plot_annotation_shiny(vid1_name = vid_name(),
                                        video1_data = video1_beh_frame())
      
      plot_mod <- plot_annotation_shiny(vid1_name = vid_name(),
                                        video1_data = video1_mod_frame())
      
      plot_beh_int <- plot_annotation_shiny(vid1_name = vid_name(),
                                            video1_data = video1_beh_int_frame())
      
      plot_mod_int <- plot_annotation_shiny(vid1_name = vid_name(),
                                            video1_data = video1_mod_int_frame())
      
      # Create ggplotly objects
      make_interactive_plots(plot_beh, plot_beh_int, plot_mod, plot_mod_int) %>% layout(height = 1600)
    }
  }) # End of plot code
}

shinyApp(ui = ui, server = server)