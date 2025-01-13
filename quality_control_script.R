# Script for video annotation quality control
# Created by Nick Remillard 1/9/2024

# Script last updated: 1/9/2024

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
#raw_file_v1 <- video1_info$raw_file

# If else loop ----
compare_files_yesno <- dlg_message("Are you comparing two files?", type = c("yesno"))$res

if(compare_files_yesno == "yes"){ # If comparing two videos and doing quality control
  
  video2_info <- select_video(folder_path)
  #raw_file_v2 <- video2_info$raw_file
  
  # Compare all behaviors and modifiers
    # Set up behavior data frames - v1
    video1_frames <- ready_to_plot(video1_info)
    video2_frames <- ready_to_plot(video2_info)
    
    # Calculate percent agreement for behavior (2 videos only)
    beh_agree <- calc_perc_agreement(video1_frames$beh_frame, video2_frames$beh_frame)
    
    # Calculate percent agreement for modifier (2 videos only)
    mod_agree <- calc_perc_agreement(video1_frames$mod_frame, video2_frames$mod_frame)
  
    # Total plot comparison (2 videos only)
    plot_beh <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                    tall_data1 = tall_beh_v1, tall_data2 = tall_beh_v2,
                    agree_list = beh_agree)
    # 
    # plot_mod <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
    #                             tall_data1 = tall_mod_v1, tall_data2 = tall_mod_v2,
    #                             agree_list = beh_agree)
  
  # Compare only periods with behaviors of interest
    # Filter agreement data frames for only periods containing behaviors of interest
    # beh_int_frame1 <- tall_beh_v1
    # beh_int_frame1$behavior <- ifelse(beh_int_frame1$behavior %in% beh_of_interest, beh_int_frame1$behavior, "Other")
    # 
    # beh_int_frame2 <- tall_beh_v2
    # beh_int_frame2$behavior <- ifelse(beh_int_frame2$behavior %in% beh_of_interest, beh_int_frame2$behavior, "Other")
    # 
    # Calculate percent agreement for behavior (2 videos only)
    beh_int_agree <- calc_perc_agreement(video1_frames$beh_int_frame, video2_frames$beh_int_frame)
    
    # # Make mod_int_frames - need to put "Other" for same rows as beh_int_frames
    # mod_int_frame1 <- tall_mod_v1
    # mod_int_frame2 <- tall_mod_v2
    # 
    # other_rows1 <- which(beh_int_frame1$behavior %in% "Other")
    # other_rows2 <- which(beh_int_frame1$behavior %in% "Other")
    # 
    # for(i in other_rows1){mod_int_frame1$modifier[i] <- "Other"}
    # for(i in other_rows2){mod_int_frame2$modifier[i] <- "Other"}
    
    # Calculate percent agreement for modifier (2 videos only)
    mod_int_agree <- calc_perc_agreement(video1_frames$mod_int_frame, video2_frames$mod_int_frame)
    
    
    plot_beh_int <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                    tall_data1 = beh_int_frame1, tall_data2 = beh_int_frame2)
  
  
  # Create your ggplotly objects
  plot_beh_int_interactive <- ggplotly(plot_beh_int)
  plot_beh_interactive <- ggplotly(plot_beh)
  
  combined_plot <- subplot(plot_beh_int_interactive, plot_beh_interactive, nrows = 2)
  
  # Save the combined plot as an HTML file
  saveWidget(combined_plot, "combined_plots.html", selfcontained = TRUE)
  
  # Open the combined plot in the default browser
  browseURL("combined_plots.html")
  
}else{ # If only doing quality control on one video
  
  # Set up behavior data frames - v1
  tall_beh_v1 <- clean_raw_file(video1_info)$tall_data_beh
  tall_mod_v1 <- clean_raw_file(video1_info)$tall_data_mod
  
  # Total plot comparison (2 videos only)
  plot_beh <- plot_annotation(video1_info = video1_info,
                              tall_data1 = tall_beh_v1)
  
  # Compare only periods with behaviors of interest
  # Filter agreement data frames for only periods containing behaviors of interest
  beh_int_frame1 <- tall_beh_v1 %>% filter(behavior %in% beh_of_interest)
  
  beh_int_frame1 <- add_other_rows(beh_int_frame1)
  
  plot_beh_int <- plot_annotation(video1_info = video1_info,
                                  tall_data1 = beh_int_frame1)
  
  ggplotly(plot_beh_int)
  ggplotly(plot_beh)
  
} # End if else statement here
