# Script for video annotation quality control
# Created by Nick Remillard 1/9/2024

# Script last updated: 1/13/2024

# SEC 1: Load necessary packages ----
rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(svDialogs)
library(gridExtra)
library(stringr)
library(plotly)
library(shiny)
library(htmlwidgets)

source("DOchecks_functions.R")

# Select parent folder
svDialogs::dlg_message("Select the folder containing annotated Excel files.")
folder_path <- svDialogs::dlg_dir()$res

# SEC 2: Setup ----

# Manually tell which activities have no modifiers
behaviors_no_mod <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                      "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                      "Treadmill Walking at slower pace than normal", "Walking Around Room", "Off Camera")

beh_of_interest <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                     "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                     "Treadmill Walking at slower pace than normal", "Off Camera", "Light Calisthenics", "Dusting", "Computer Work")


# SEC 3: Select video to assess ----
video1_info <- select_video(folder_path)

# If else loop ----
compare_files_yesno <- dlg_message("Are you comparing two files?", type = c("yesno"))$res

if(compare_files_yesno == "yes"){ # If comparing two videos and doing quality control
  
  video2_info <- select_video(folder_path)
  
  # Compare all behaviors and modifiers
    # Set up behavior data frames - v1
    video1_frames <- ready_to_plot(video1_info)
    video2_frames <- ready_to_plot(video2_info)
    
    # Calculate percent agreement for behavior (2 videos only)
    beh_agree <- calc_perc_agreement(video1_frames$beh_frame, video2_frames$beh_frame)
    
    # Calculate percent agreement for modifier (2 videos only)
    mod_agree <- calc_perc_agreement(video1_frames$mod_frame, video2_frames$mod_frame)
  
  
  # Compare only periods with behaviors of interest
    # Calculate percent agreement for behavior (2 videos only)
    beh_int_agree <- calc_perc_agreement(video1_frames$beh_int_frame, video2_frames$beh_int_frame)
    
    # Calculate percent agreement for modifier (2 videos only)
    mod_int_agree <- calc_perc_agreement(video1_frames$mod_int_frame, video2_frames$mod_int_frame)
    
  # Total plot comparison (2 videos only)
  plot_beh <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                              tall_data1 = video1_frames$beh_frame, tall_data2 = video2_frames$beh_frame,
                              agree_list = beh_agree)
    
  plot_mod <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                              tall_data1 = video1_frames$mod_frame, tall_data2 = video2_frames$mod_frame,
                              agree_list = beh_agree)
    
  plot_beh_int <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                  tall_data1 = video1_frames$beh_int_frame, tall_data2 = video2_frames$beh_int_frame,
                  agree_list = beh_int_agree)
  
  plot_mod_int <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                                  tall_data1 = video1_frames$mod_int_frame, tall_data2 = video2_frames$mod_int_frame,
                                  agree_list = mod_int_agree)
  
  # Create ggplotly objects
  plot_beh_interactive <- ggplotly(plot_beh)
  plot_beh_int_interactive <- ggplotly(plot_beh_int)
  
  plot_mod_interactive <- ggplotly(plot_mod)
  plot_mod_int_interactive <- ggplotly(plot_mod_int)
  
  combined_plot <- subplot(plot_beh_int_interactive, plot_beh_interactive,
                           plot_mod_int_interactive, plot_mod_interactive,
                           nrows = 4, shareX = TRUE) %>%
    layout(
      yaxis = list(fixedrange = TRUE),
      yaxis2 = list(fixedrange = TRUE),
      yaxis3 = list(fixedrange = TRUE),
      yaxis4 = list(fixedrange = TRUE)
    )
  
}else{ # If only doing quality control on one video
  
  video1_frames <- ready_to_plot(video1_info)
  
  # Total plot comparison (2 videos only)
  plot_beh <- plot_annotation(video1_info = video1_info,
                              tall_data1 = video1_frames$beh_frame)
  
  plot_beh_int <- plot_annotation(video1_info = video1_info,
                                  tall_data1 = video1_frames$beh_int_frame)
  
  ggplotly(plot_beh_int)
  ggplotly(plot_beh)
  
} # End if else statement here

# Shiny app ----

# User interface
ui <- fluidPage(
  titlePanel("Interactive ggplotly Graph"),
  mainPanel(
    plotlyOutput("interactive_plot")
  )
)

# Server
server <- function(input, output) {
  output$interactive_plot <- renderPlotly({
    combined_plot
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)