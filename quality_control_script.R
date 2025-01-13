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

# Select parent folder
svDialogs::dlg_message("Select the folder containing annotated Excel files.")
folder_path <- svDialogs::dlg_dir()$res

# SEC 2: Setup and Functions ----

source("DOchecks_functions.R")

# Manually tell which activities have no modifiers
behaviors_no_mod <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                      "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                      "Treadmill Walking at slower pace than normal", "Walking Around Room", "Off Camera")

beh_of_interest <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                     "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                     "Treadmill Walking at slower pace than normal", "Off Camera", "Light Calisthenics", "Dusting", "Computer Work")

# # Function to select videos
# select_video <- function(folder_path) {
#   # Select file
#   svDialogs::dlg_message("Select the desired video (must be an excel file with one sheet) to undergo quality control.")
#   comp_name <- file.choose()
#   raw_file <- readxl::read_excel(comp_name)
#   
#   # Process file name
#   comp_name <- gsub("\\\\", "/", comp_name)
#   vid_name <- gsub(paste(folder_path, sep=""), "", comp_name)
#   
#   # Extract pattern, initials, and video number
#   pattern <- stringr::str_extract(vid_name, "[A-Z]{2,3} - CUAMC_\\d+")
#   initials <- stringr::str_extract(pattern, "[A-Z]{2,3}")
#   vid_num <- stringr::str_extract(pattern, "CUAMC_\\d+")
#   
#   # Manual input if extraction fails
#   if(is.na(initials)){
#     initials <- svDialogs::dlg_input(message = "Enter initials of coder. (ex: NR)")$res
#   }
#   if(is.na(vid_num)){
#     vid_num <- svDialogs::dlg_input(message = "Enter which participant video was selected. (ex: CUAMC_01)")$res
#   }
#   
#   # Return results as a list
#   return(list(
#     raw_file = raw_file,
#     comp_name = comp_name,
#     vid_name = vid_name,
#     initials = initials,
#     vid_num = vid_num
#   ))
# }
# 
# # Function to fill modifier columns with values for behaviors that have no modifier
# fill_blank_mod <- function(behaviors_no_mod, values_to_check, replace_values) {
#   # Find the indices where values_to_check match behaviors_no_mod
#   indices_to_replace <- which(values_to_check %in% behaviors_no_mod)
#   
#   # Replace corresponding elements with "no modifier"
#   replace_values[indices_to_replace] <- "no modifier"
#   
#   return(replace_values)
# }
# 
# # Function to clean raw files to ready for percent agreement and graphing
# clean_raw_file <- function(video_info){
#   
#   raw_file <- video_info$raw_file
#   initials <- video_info$initials
#   vid_num <- video_info$vid_num
#   
#   beh <- raw_file %>% select(Time_Relative_sf, Behavior)
#   
#   # Set up modifier data frames
#   mod <- raw_file %>% select(Time_Relative_sf, Modifier_1)
#   
#   # Fill in blank modifiers to separate from true NAs
#   mod$Modifier_1 <- fill_blank_mod(behaviors_no_mod, beh$Behavior, mod$Modifier_1)
#   
#   # Remove rows with NA
#   beh <- na.omit(beh)
#   
#   # Saving order of variables to correct order later
#   beh_order <- beh$Behavior
#   
#   mod_order <- mod$Modifier_1
#   
#   paste_beh <- paste("Behavior", initials, vid_num, sep="_")
#   paste_mod <- paste("Modifier_1", initials, vid_num, sep="_")
#   
#   tall_data_beh <- data.frame(Time_Relative_sf = beh$Time_Relative_sf,
#                               source = rep(paste_beh, nrow(beh)),
#                               behavior = beh$Behavior)
#   
#   tall_data_mod <- data.frame(Time_Relative_sf = mod$Time_Relative_sf,
#                               source = rep(paste_mod, nrow(mod)),
#                               modifier = mod$Modifier_1)
#   return(list(
#     tall_data_beh = tall_data_beh,
#     tall_data_mod = tall_data_mod
#   ))
# }
# 
# # Calculating percent agreement function
# calc_perc_agreement <- function(frame1, frame2){
#   
#   # Find the maximum time in each dataset
#   max_time <- max(max(frame1$Time_Relative_sf, na.rm = TRUE), max(frame2$Time_Relative_sf, na.rm = TRUE))
#   
#   # Create a sequence of time from 0 to the maximum time in milliseconds
#   time_sequence <- seq(0, max_time, by = 0.1)
#   
#   # Create a dataframe with the time column
#   new_df <- data.frame(Time_Relative_sf = time_sequence)
#   
#   # Add columns for behavior_1 and behavior_2
#   new_df$behavior_1 <- NA
#   new_df$behavior_2 <- NA
#   
#   # Add start_stop column to frame1 and frame2
#   frame1$start_stop <- rep(c("start", "stop"), length.out = nrow(frame1))
#   frame2$start_stop <- rep(c("start", "stop"), length.out = nrow(frame2))
#   
#   # Fill in new_df$behavior_1
#   frame1$Time_Relative_sf <- as.numeric(format(frame1$Time_Relative_sf, digits = 3, nsmall = 1))
#   new_df$Time_Relative_sf <- as.numeric(format(new_df$Time_Relative_sf, nsmall = 1))
#   
#   rows_to_snag <- seq(from = 1, to = nrow(frame1), by = 2)
#   for(i in rows_to_snag){
#     start_row <- which(new_df$Time_Relative_sf %in% frame1$Time_Relative_sf[i])
#     end_row <- which(new_df$Time_Relative_sf %in% frame1$Time_Relative_sf[i+1])-1
#     behavior <- frame1[i,3] # Grabbing either behavior or modifier column
#     new_df$behavior_1[start_row:end_row] <- behavior
#   }
#   
#   # Fill in new_df$behavior_2
#   frame2$Time_Relative_sf <- as.numeric(format(frame2$Time_Relative_sf, digits = 3, nsmall = 1))
#   
#   rows_to_snag <- seq(from = 1, to = nrow(frame2), by = 2)
#   for(i in rows_to_snag){
#     start_row <- which(new_df$Time_Relative_sf %in% frame2$Time_Relative_sf[i])
#     end_row <- which(new_df$Time_Relative_sf %in% frame2$Time_Relative_sf[i+1])-1
#     behavior <- frame2[i,3] # Grabbing either behavior or modifier column
#     new_df$behavior_2[start_row:end_row] <- behavior
#   }
#   
#   percent_agreement <- format(mean(new_df$behavior_1 == new_df$behavior_2, na.rm = TRUE) * 100, digits = 3, nsmall = 1)
#   
#   return(list(
#     percent_agreement = percent_agreement,
#     agreement_frame = new_df,
#     max_time = max_time))
# }
# 
# # Making Other last in factor function
# make_last_factor <- function(vec, last_value) {
#   # Convert to factor
#   vec_factor <- as.factor(vec)
#   
#   # Get all levels
#   all_levels <- levels(vec_factor)
#   
#   # Remove the last_value from levels and add it to the end
#   new_levels <- c(setdiff(all_levels, last_value), last_value)
#   
#   # Reorder the factor
#   vec_factor <- factor(vec_factor, levels = new_levels)
#   
#   return(vec_factor)
# }
# 
# # Adding Other rows for creating behavior of interest only dataframes
# add_other_rows <- function(df) {
#   # Ensure the dataframe is ordered by Time_Relative_sf
#   df <- df[order(df$Time_Relative_sf),]
#   
#   # Create a new dataframe to store the results
#   new_df <- data.frame()
#   
#   # Loop through the dataframe, adding new rows after every 2 original rows
#   for (i in seq(1, nrow(df), by = 2)) {
#     # Add the original two rows
#     new_df <- rbind(new_df, df[i,])
#     if (i+1 <= nrow(df)) {
#       new_df <- rbind(new_df, df[i+1,])
#       
#       # Add the first 'Other' row
#       other_row1 <- df[i+1,]
#       other_row1$behavior <- 'Other'
#       new_df <- rbind(new_df, other_row1)
#       
#       # Add the second 'Other' row
#       if (i+2 <= nrow(df)) {
#         other_row2 <- df[i+2,]
#         other_row2$behavior <- 'Other'
#         new_df <- rbind(new_df, other_row2)
#       }
#     }
#   }
#   
#   # Reset row names
#   rownames(new_df) <- NULL
#   
#   return(new_df)
# }
# 
# # Plotting function
# plot_annotation <- function(video1_info, tall_data1, video2_info = NULL, tall_data2 = NULL, agree_list = NULL) {
#   y_column <- if("behavior" %in% names(tall_data1)) "behavior" else if("modifier" %in% names(tall_data1)) "modifier"
#   
#   tall_data1[[y_column]] <- make_last_factor(tall_data1[[y_column]], "Other")
#   
#   plot <- ggplot() +
#     geom_path(data = tall_data1, aes(x = Time_Relative_sf, y = .data[[y_column]], group = 1, color = paste(video1_info$initials))) +
#     theme_minimal() +
#     labs(x = "Relative Time (s)", y = stringr::str_to_title(y_column)) +
#     theme(legend.position = "bottom") +
#     scale_x_continuous(breaks = seq(0, ceiling(max(tall_data1$Time_Relative_sf)), by = 600)) +
#     scale_y_discrete(limits = rev(levels(tall_data1[[y_column]])))
#   
#   if (!is.null(video2_info) && !is.null(tall_data2)) {
#     tall_data2[[y_column]] <- make_last_factor(tall_data2[[y_column]], "Other")
#     
#     plot <- plot +
#       geom_path(data = tall_data2, aes(x = Time_Relative_sf, y = .data[[y_column]], group = 1, color = paste(video2_info$initials)))
#   }
#   
#   if (!is.null(agree_list)) {
#     plot <- plot +
#       labs(title = paste(video1_info$vid_num, " Percent agreement: ",
#                          agree_list$percent_agreement, "%", sep=""))
#     
#     # Update x-axis scale if agree_list is provided
#     plot <- plot +
#       scale_x_continuous(breaks = seq(0, ceiling(agree_list$max_time), by = 600))
#   } else {
#     plot <- plot +
#       labs(title = paste(video1_info$vid_num, " Data"))
#   }
#   
#   return(plot)
# }

# SEC 3: Select video to assess ----
video1_info <- select_video(folder_path)
raw_file_v1 <- video1_info$raw_file

# If else loop ----
compare_files_yesno <- dlg_message("Are you comparing two files?", type = c("yesno"))$res

if(compare_files_yesno == "yes"){ # If comparing two videos and doing quality control
  
  video2_info <- select_video(folder_path)
  raw_file_v2 <- video2_info$raw_file
  
  # Set up behavior data frames - v1
  tall_beh_v1 <- clean_raw_file(video1_info)$tall_data_beh
  tall_mod_v1 <- clean_raw_file(video1_info)$tall_data_mod
  
  # Set up behavior data frames - v2
  tall_beh_v2 <- clean_raw_file(video2_info)$tall_data_beh
  tall_mod_v2 <- clean_raw_file(video2_info)$tall_data_mod
  
  # Calculate percent agreement for behavior (2 videos only)
  beh_agree <- calc_perc_agreement(tall_beh_v1, tall_beh_v2)
  beh_perc_agree <- beh_agree$percent_agreement
  beh_agree_frame <- beh_agree$agreement_frame
  
  # Calculate percent agreement for modifier (2 videos only)
  mod_agree <- calc_perc_agreement(tall_mod_v1, tall_mod_v2)
  mod_perc_agree <- mod_agree$percent_agreement
  mod_agree_frame <- mod_agree$agreement_frame
  
  # Total plot comparison (2 videos only)
  plot_beh <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                  tall_data1 = tall_beh_v1, tall_data2 = tall_beh_v2,
                  agree_list = beh_agree)
  
  plot_mod <- plot_annotation(video1_info = video1_info, video2_info = video2_info,
                              tall_data1 = tall_mod_v1, tall_data2 = tall_mod_v2,
                              agree_list = beh_agree)
  
  # Compare only periods with behaviors of interest
  # Filter agreement data frames for only periods containing behaviors of interest
  beh_int_frame1 <- tall_beh_v1 %>% filter(behavior %in% beh_of_interest)
  beh_int_frame2 <- tall_beh_v2 %>% filter(behavior %in% beh_of_interest)
  
  beh_int_frame1 <- add_other_rows(beh_int_frame1)
  beh_int_frame2 <- add_other_rows(beh_int_frame2)
  
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
