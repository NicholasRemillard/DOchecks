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
    file_path <- gsub("\\\\", "/", file_path)
    
  }
  compare_frame <- read_excel(file_path)
  
  if(is.null(cur_folder_path)){
    cur_folder_path <- paste(getwd(), "/Input Observer Files/", sep="")
  }
  
  comp_name <- gsub(cur_folder_path, "", file_path)
  study_name <- gsub(" -.*", "", comp_name)
  #pattern <- str_extract(comp_name, "[a-zA-Z]{2,3}_[tT]rainingvid\\d+_\\d+")
  pattern <- str_extract(comp_name, "(?<=- ).*?(?= -)")
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
    vid_num <- dlg_input(message = "Enter which training video was selected. (ex: trainingvid2)")$res
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
    creation_date = creation_date,
    file_path = file_path
  ))
  
}

get_crit_file <- function(file_path = NULL, vid_num){
  
  if(is.null(file_path)){
    dlg_message("Select the folder containing the criterion videos.")
    file_path <- choose.dir()
  }
  
  crit_files <- list.files(file_path)
  crit_name <- crit_files[grep(vid_num, crit_files)]
  criterion_frame <- read_excel(paste(file_path, crit_name, sep = "/"))
  
  return(criterion_frame)
}

plot_comparison <- function(criterion_frame, comparison_list, input_column) {
  
  criterion <- criterion_frame %>% select(Time_Relative_sf, !!sym(input_column))
  criterion[[input_column]][is.na(criterion[[input_column]])] <- "ZZ_No value"
  criterion_tall <- data.frame(Time_Relative_sf = criterion$Time_Relative_sf,
                               source = rep("Criterion", nrow(criterion)),
                               behavior = criterion[[input_column]])
  max_time_crit <- max(criterion_tall$Time_Relative_sf, na.rm = TRUE)
  
  # Checking if there is an empty behavior row
  if (input_column == "Behavior") {
    if (any(is.na(comparison_list$compare_frame[[input_column]]))) {
      stop("There seems to be an empty behavior row, please check the original file and resubmit.")
    }
  }
  
  # Handling cases where a modifier may not pop up in the compare_frame
  if(input_column %in% colnames(comparison_list$compare_frame)){
    compare <- comparison_list$compare_frame %>% select(Time_Relative_sf, !!sym(input_column))
    compare[[input_column]][is.na(compare[[input_column]])] <- "ZZ_No value"
    compare_tall <- data.frame(Time_Relative_sf = compare$Time_Relative_sf,
                               source = rep("Comparison", nrow(compare)),
                               behavior = compare[[input_column]])
    max_time_comp <- max(compare_tall$Time_Relative_sf, na.rm = TRUE) 
  }
  
  # Find the maximum time in each dataset
  if(!exists("max_time_comp")){
    max_time <- max_time_crit
  }else{
    max_time <- max(max_time_crit, max_time_comp)
  }
  
  # Create a sequence of time from 0 to the maximum time in milliseconds
  time_sequence <- seq(0, max_time, by = 0.001)
  
  # Create a dataframe with the time column
  new_df <- data.frame(Time_Relative_sf = time_sequence)
  
  # Add columns for behavior_1 and behavior_2
  new_df$criterion <- NA
  new_df$comparison <- NA
  
  # Add start_stop column to tall_data_b_crit and tall_data_b_comp
  criterion_tall$start_stop <- rep(c("start", "stop"), length.out = nrow(criterion_tall))
  
  if(exists("compare_tall")){
    compare_tall$start_stop <- rep(c("start", "stop"), length.out = nrow(compare_tall)) 
  }
  
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
  if(exists("compare_tall")){
   
    compare_tall$Time_Relative_sf <- as.numeric(format(compare_tall$Time_Relative_sf, digits = 3, nsmall = 3))
    
    rows_to_snag <- seq(from = 1, to = nrow(compare_tall), by = 2)
    for(i in rows_to_snag){
      start_row <- which(new_df$Time_Relative_sf %in% compare_tall$Time_Relative_sf[i])
      end_row <- which(new_df$Time_Relative_sf %in% compare_tall$Time_Relative_sf[i+1])-1
      behavior <- compare_tall$behavior[i]
      new_df$comparison[start_row:end_row] <- behavior
    }
  }
  
  percent_agreement <- format(mean(new_df$criterion == new_df$comparison, na.rm = TRUE) * 100, digits = 3, nsmall = 1)
  
  #print(percent_agreement)
  
  if(!exists("compare_tall")){
    
    my_plot <- ggplot() +
      geom_path(data = criterion_tall, aes(x = Time_Relative_sf, y = behavior, group = 1, color = "Criterion")) +
      theme_minimal() +
      labs(x = "Relative Time (s)", y = input_column, title = paste("Percent agreement: ", percent_agreement, "%",sep="")) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(0, ceiling(max_time), by = 60))
    
  }else{
    
    my_plot <- ggplot() +
      geom_path(data = criterion_tall, aes(x = Time_Relative_sf, y = behavior, group = 1, color = "Criterion")) +
      geom_path(data = compare_tall, aes(x = Time_Relative_sf, y = behavior, group = 1, color = "Your coding")) +
      theme_minimal() +
      labs(x = "Relative Time (s)", y = input_column, title = paste("Percent agreement: ", percent_agreement, "%",sep="")) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(0, ceiling(max_time), by = 60))
    
  }
  
  
  
  percent_agreement <- as.numeric(percent_agreement)
  
  if(is.na(percent_agreement)){
    did_pass <- c("Modifier did not appear in comparison")
  }else if(percent_agreement < 90.0){
    did_pass <- c("Agreement value is below 90%, please reannotate")
  }else{
    did_pass <- c("Agreement at or above 90%, passed")
  }
  
  # Putting plots together
  combined_plot <- grid.arrange(my_plot, top = paste(comparison_list$vid_num, ": ", did_pass, sep=""))
  
  return(list(
    combined_plot = combined_plot,
    percent_agreement = percent_agreement))
  
}

move_file <- function(file_path, to_path = NULL){
  if (is.null(to_path)) {
    dir_path <- dirname(file_path)
    file_name <- basename(file_path)
    checked_dir <- file.path(dir_path, "Checked")
    
    # Create the "Checked" folder if it doesn't exist
    if (!dir.exists(checked_dir)) {
      dir.create(checked_dir)
    }
    
    to_path <- file.path(checked_dir, file_name)
  }
  
  success <- file.rename(file_path, to_path)
  
  if (success) {
    message("File moved to: ", to_path)
  } else {
    warning("File could not be moved. Check if the file exists and you have permissions.")
  }
  
  return(success)
  
}
