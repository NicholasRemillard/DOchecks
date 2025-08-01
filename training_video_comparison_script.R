# Script to compare training videos for accuracy and improvement
# Created by Nick Remillard 2/8/2024
# For DO Coding Training Purposes Only

rm(list = ls())

# ------------------------------------------------------------------------------
# Step 1: Setup

  source("training_video_comparison_functions.R")
  
  # Check if master file exists
  training_file_name <- "Output Annotation Stats/annotation_training_stats.xlsx"
  
  # Check if the file exists in the current working directory
  if (file.exists(training_file_name)) {
    training_excel <- read_excel(training_file_name)
  } else {
    training_excel <- data.frame(Initials = NA,
                                 Date_analyzed = NA,
                                 Dataset = NA, 
                                 Video_num = NA,
                                 Retest = NA,
                                 Attempt_num = NA,
                                 Passed = NA)
  }


# ------------------------------------------------------------------------------
# Step 2: Choose files

  obs_list <- get_obs_file()
  criterion_frame <- get_crit_file(file_path = "Input Criterion Files/",
                                   vid_num = obs_list$vid_num)

  
# ------------------------------------------------------------------------------
# Step 3: Make plots

  # Get modifiers
  modifier_cols <- colnames(criterion_frame)[grepl("^Modifier_", 
                                                   colnames(criterion_frame))]
  
  # Add behavior to list of modifier column names for plotting
  plot_cols <- append(modifier_cols, "Behavior", after = 0)
  rm(modifier_cols)
  
  # Plotting over list of column names
  my_plots <- purrr::map(plot_cols,
              ~ plot_comparison(criterion_frame = criterion_frame,
                                             comparison_list = obs_list,
                                             input_column = .x)$combined_plot
  ) # End map()
  
  # Put all plots together
  all_plots <- grid.arrange(grobs = my_plots, ncol = 2)
    

# ------------------------------------------------------------------------------  
# Step 4: Save plot
  num_rows <- ceiling(length(plot_cols)/2) # For flexible plot height
  
  plot_path <- "Output Graphs"
  ggsave(paste(plot_path, "/",
               obs_list$initials, "_",
               obs_list$vid_num, "_",
               obs_list$attempt_num, "_plot.png",
               sep=""),
         plot = all_plots, width = 24, height = 5*num_rows, dpi = 300)

  
# ------------------------------------------------------------------------------
# Step 6: Move file to 'Checked' folder
  move_file(obs_list$file_path)
  
  
# ------------------------------------------------------------------------------
# Step 5: Save data
  
  yes_save <-dlg_input(message = "Do you want to save this data? 
                       Please enter 'yes' or 'no'.")$res
  
  did_pass <-dlg_input(message = "Check the plot output. Did the annotator pass
                       this video? Type 'yes' or 'no'.")$res
  
  if (file.exists(training_file_name) & yes_save == "yes") {
    # Add a row and fill with data
    row_num <- nrow(training_excel)+1
    training_excel[row_num,] <- NA
    
    training_excel$Initials[row_num] <- obs_list$initials
    training_excel$Date_analyzed[row_num] <- as.character(today())
    training_excel$Dataset[row_num] <- obs_list$study_name
    training_excel$Video_num[row_num] <- obs_list$vid_num
    training_excel$Retest[row_num] <- obs_list$retest_yn
    training_excel$Attempt_num[row_num] <- as.numeric(obs_list$attempt_num)
    training_excel$Passed[row_num] <- did_pass
    
    write_xlsx(training_excel, "Output Annotation Stats/annotation_training_stats.xlsx")
    
  } else if(!file.exists(training_file_name) & yes_save == "yes") {
    
    training_excel$Initials[1] <- obs_list$initials
    training_excel$Date_analyzed[1] <- as.character(today())
    training_excel$Dataset[1] <- obs_list$study_name
    training_excel$Video_num[1] <- obs_list$vid_num
    training_excel$Retest[1] <- obs_list$retest_yn
    training_excel$Attempt_num[1] <- as.numeric(obs_list$attempt_num)
    training_excel$Passed[1] <- did_pass
    
    write_xlsx(training_excel, "Output Annotation Stats/annotation_training_stats.xlsx")
    
  }
