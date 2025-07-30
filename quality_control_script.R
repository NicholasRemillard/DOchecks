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
library(irr)

source("DOchecks_functions.R")

# Select parent folder
svDialogs::dlg_message("Select the folder containing annotated Excel files.")
folder_path <- svDialogs::dlg_dir()$res

# SEC 2: Setup ----

# Manually tell which activities have no modifiers
behaviors_no_mod <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                      "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                      "Treadmill Walking at slower pace than normal", "Walking Around Room", "Off Camera")

beh_of_interest <- c("Off Camera", "Unknown",
                     "Reading", "Writing", "Watching Television", "Computer Work",
                     "Sweeping the Floor", "Dusting", "Vacuuming", "Folding Clothes/putting clothes away", "Cleaning Window/mirror",
                     "Moving Boxes",
                     "Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                     "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                     "Treadmill Walking at slower pace than normal", "Walking around room",
                     "Light Calisthenics", "Resistance Exercise with weights", "Yoga")


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
    
    # Calculate percent agreement for behavior and modifier(2 videos only)
    beh_agree <- calc_perc_agreement(video1_frames$beh_frame, video2_frames$beh_frame)
    mod_agree <- calc_perc_agreement(video1_frames$mod_frame, video2_frames$mod_frame)
  
  # Compare only periods with behaviors of interest
    # Calculate percent agreement for behavior and modifier (2 videos only)
    beh_int_agree <- calc_perc_agreement(video1_frames$beh_int_frame, video2_frames$beh_int_frame)
    mod_int_agree <- calc_perc_agreement(video1_frames$mod_int_frame, video2_frames$mod_int_frame)
    
  # Make plots
  plot_beh <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                              video1_data = video1_frames$beh_frame, video2_data = video2_frames$beh_frame,
                              agree_list = beh_agree)
    
  plot_mod <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                              video1_data = video1_frames$mod_frame, video2_data = video2_frames$mod_frame,
                              agree_list = mod_agree)
    
  plot_beh_int <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                  video1_data = video1_frames$beh_int_frame, video2_data = video2_frames$beh_int_frame,
                  agree_list = beh_int_agree)
  
  plot_mod_int <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                                  video1_data = video1_frames$mod_int_frame, video2_data = video2_frames$mod_int_frame,
                                  agree_list = mod_int_agree)
  
  
  # Calculate ICC
  calculate_ICC(beh_agree$agreement_frame)
  calculate_ICC(mod_agree$agreement_frame)
  
  # Create ggplotly objects
  combined_plot <- make_interactive_plots(plot_beh, plot_beh_int, plot_mod, plot_mod_int)
  
  # Check transitions
  check_transitions(video1_frames$beh_frame)
  check_transitions(video2_frames$beh_frame)
  
  # Check periods where activities quickly alternate
  check_alternating(video_data = video1_frames$beh_frame, number_seqs = 5, seq_duration = 10)
  check_alternating(video_data = video2_frames$beh_frame, number_seqs = 5, seq_duration = 10)
  
  # Check for long periods of activity
  check_long_activities(video_data = video1_frames$beh_frame, duration = 900)
  check_long_activities(video_data = video2_frames$beh_frame, duration = 900)
  
  # Check for comments
  check_comments(video1_info$raw_file)
  check_comments(video2_info$raw_file)
  
}else{ # If only doing quality control on one video
  
  video1_frames <- ready_to_plot(video1_info)
  
  # Make plots
  plot_beh <- plot_annotation(video1_info = video1_info,
                              video1_data = video1_frames$beh_frame)
  
  plot_mod <- plot_annotation(video1_info = video1_info,
                              video1_data = video1_frames$mod_frame)
  
  plot_beh_int <- plot_annotation(video1_info = video1_info,
                                  video1_data = video1_frames$beh_int_frame,)
  
  plot_mod_int <- plot_annotation(video1_info = video1_info,
                                  video1_data = video1_frames$mod_int_frame)
  
  # Create ggplotly objects
  combined_plot <- make_interactive_plots(plot_beh, plot_beh_int, plot_mod, plot_mod_int)
  
  source_file <- paste(video1_info$initials,video1_info$vid_num, sep="_")
  
  # Check transitions
  trans_frame <- check_transitions(video1_frames$beh_frame)
  
  # Check periods where activities quickly alternate
  alt_frame <- check_alternating(video_data = video1_frames$beh_frame, number_seqs = 5, seq_duration = 10)
  
  # Check for long periods of activity
  longact_frame <- check_long_activities(video_data = video1_frames$beh_frame, duration = 900)
  
  # Check for comments
  comment_frame <- check_comments(video1_info$raw_file)$comment_frame
  
  start_time <- check_comments(video1_info$raw_file)$start_time
  
  qc_frame <- dplyr::bind_rows(trans_frame, alt_frame, longact_frame, comment_frame)
  qc_frame$source <- source_file
  
} # End if else statement here

# # Shiny app ----
# 
# # User interface
# ui <- fluidPage(
#   titlePanel("Interactive ggplotly Graph"),
#   sidebarLayout(
#     sidebarPanel(
#       h4("Data Summary"),
#       htmlOutput("agreement_text")
#     ),
#     mainPanel(
#       plotlyOutput("interactive_plot")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output) {
#   output$interactive_plot <- renderPlotly({
#     combined_plot %>% layout(height = 1600)
#   })
#   
#   output$agreement_text <- renderText({
#     paste(
#       "Behavior of interest only percent agreement: ", beh_int_agree$percent_agreement, "%<br><br>",
#       "Overall behavior percent agreement: ", beh_agree$percent_agreement, "%<br><br>",
#       "Modifier percent agreement corresponding with behaviors of interest: ", mod_int_agree$percent_agreement, "%<br><br>",
#       "Overall modifier percent agreement: ", mod_agree$percent_agreement, "%", sep=""
#     )
#   })
#   
# }
# 
# # Run the shiny app
# shinyApp(ui = ui, server = server)