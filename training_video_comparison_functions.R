# Functions for use in training_video_comparison_script.R

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(svDialogs)
library(gridExtra)
library(stringr)
library(writexl)
library(lubridate)
library(purrr)

get_obs_file <- function(file_path = NULL, cur_folder_path = NULL){
  
  if(is.null(file_path)){
    dlg_message("Select the desired training video (must be an excel file with one sheet) to compare to criterion.")
    file_path <- file.choose()
    
  }
  compare_frame <- read_excel(file_path)
  
  if(is.null(cur_folder_path)){
    cur_folder_path <- paste(getwd(), "/Input Observer Files/", sep="")
  }
  
  comp_name <- gsub("\\\\", "/", file_path)
  comp_name <- gsub(cur_folder_path, "", comp_name)
  study_name <- gsub(" -.*", "", comp_name)
  pattern <- str_extract(comp_name, "[a-zA-Z]{2,3}_[tT]rainingvid\\d+_\\d+")
  initials <- str_extract(pattern, "[a-zA-Z]{2,3}")
  vid_num <- str_extract(pattern, "[tT]rainingvid\\d+")
  attempt_num <- gsub("_", "", str_extract(pattern, "_\\d+"))
  
  retest_yn <- dlg_input(message = "Yes this a retest in a new semester? yes/no", default = "no")$res
  
  creation_date <- file.info(comp_name)$mtime
  creation_date <- force_tz(creation_date, tzone = "UTC")
  
  # Handle missing data, if any
  if(initials %in% NA){
    initials <- dlg_input(message = "Enter initials of coder. (ex: NR)")$res
  }
  if(vid_num %in% NA){
    vid_num <- dlg_input(message = "Enter which training video was selected. (ex: training_2)")$res
  }
  if(attempt_num %in% NA){
    attempt_num <- as.numeric(dlg_input(message = "Which attempt is this? Enter an integer. (Ex: 1)")$res)
  }
  if(study_name %in% NA){
    study_name <- dlg_input(message = "Enter study name. (ex: GyroS2_Free-living)")$res
  }
  
  # Return values
  return(list(
    compare_frame = compare_frame,
    initials = initials,
    vid_num = vid_num,
    attempt_num = attempt_num,
    study_name = study_name,
    retest_yn = retest_yn,
    creation_date = creation_date
  ))
  
}

get_crit_file <- function(file_path = NULL, vid_num){
  
  if(is.null(file_path)){
    file_path <- choose.dir()
  }
  
  crit_files <- list.files(file_path)
  crit_name <- crit_files[grep(vid_num, crit_files)]
  criterion_frame <- read_excel(paste(file_path, crit_name, sep = "/"))
  
  return(criterion_frame)
}

plot_comparison <- function(criterion_frame, comparison_list, input_column) {
  
  criterion <- criterion_frame %>% select(Time_Relative_sf, !!sym(input_column))
  compare <- comparison_list$compare_frame %>% select(Time_Relative_sf, !!sym(input_column))
  
  criterion[[input_column]][is.na(criterion[[input_column]])] <- "ZZ_No value"
  compare[[input_column]][is.na(compare[[input_column]])] <- "ZZ_No value"
  
  criterion_tall <- data.frame(Time_Relative_sf = criterion$Time_Relative_sf,
                                 source = rep("Criterion", nrow(criterion)),
                                 behavior = criterion[[input_column]])
  
  compare_tall <- data.frame(Time_Relative_sf = compare$Time_Relative_sf,
                                 source = rep("Comparison", nrow(compare)),
                                 behavior = compare[[input_column]])
  
  # Find the maximum time in each dataset
  max_time <- max(max(criterion_tall$Time_Relative_sf, na.rm = TRUE), max(compare_tall$Time_Relative_sf, na.rm = TRUE))
  
  # Create a sequence of time from 0 to the maximum time in milliseconds
  time_sequence <- seq(0, max_time, by = 0.001)
  
  # Create a dataframe with the time column
  new_df <- data.frame(Time_Relative_sf = time_sequence)
  
  # Add columns for behavior_1 and behavior_2
  new_df$criterion <- NA
  new_df$comparison <- NA
  
  # Add start_stop column to tall_data_b_crit and tall_data_b_comp
  criterion_tall$start_stop <- rep(c("start", "stop"), length.out = nrow(criterion_tall))
  compare_tall$start_stop <- rep(c("start", "stop"), length.out = nrow(compare_tall))
  
  # Fill in new_df$criterion
  criterion_tall$Time_Relative_sf <- as.numeric(format(criterion_tall$Time_Relative_sf, digits = 3, nsmall = 3))
  new_df$Time_Relative_sf <- as.numeric(format(new_df$Time_Relative_sf, nsmall = 3))
  
  rows_to_snag <- seq(from = 1, to = nrow(criterion_tall), by = 2)
  for(i in rows_to_snag){
    start_row <- which(new_df$Time_Relative_sf %in% criterion_tall$Time_Relative_sf[i])
    end_row <- which(new_df$Time_Relative_sf %in% criterion_tall$Time_Relative_sf[i+1])-1
    behavior <- criterion_tall$behavior[i]
    new_df$criterion[start_row:end_row] <- behavior
  }
  
  # Fill in new_df$comparison
  compare_tall$Time_Relative_sf <- as.numeric(format(compare_tall$Time_Relative_sf, digits = 3, nsmall = 3))
  
  rows_to_snag <- seq(from = 1, to = nrow(compare_tall), by = 2)
  for(i in rows_to_snag){
    start_row <- which(new_df$Time_Relative_sf %in% compare_tall$Time_Relative_sf[i])
    end_row <- which(new_df$Time_Relative_sf %in% compare_tall$Time_Relative_sf[i+1])-1
    behavior <- compare_tall$behavior[i]
    new_df$comparison[start_row:end_row] <- behavior
  }
  
  percent_agreement <- format(mean(new_df$criterion == new_df$comparison, na.rm = TRUE) * 100, digits = 3, nsmall = 1)
  
  #print(percent_agreement)
  
  my_plot <- ggplot() +
    geom_path(data = criterion_tall, aes(x = Time_Relative_sf, y = behavior, group = 1, color = "Criterion")) +
    geom_path(data = compare_tall, aes(x = Time_Relative_sf, y = behavior, group = 1, color = "Your coding")) +
    theme_minimal() +
    labs(x = "Relative Time (s)", y = input_column, title = paste("Percent agreement: ", percent_agreement, "%",sep="")) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(0, ceiling(max_time), by = 60))
  
  percent_agreement <- as.numeric(percent_agreement)
  
  if(percent_agreement < 90.0){
    did_pass <- c("Agreement value is below 90%, please recode")
  }else{
    did_pass <- c("Agreement at or above 90%, passed")
  }
  
  # Putting plots together
  combined_plot <- grid.arrange(my_plot, top = paste(comparison_list$vid_num, ": ", did_pass, sep=""))
  
  return(combined_plot)
  
}
