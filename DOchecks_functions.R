# Functions for use in DOchecks

# SEC 2: Setup and Functions

behaviors_no_mod <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                      "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                      "Treadmill Walking at slower pace than normal", "Walking Around Room", "Off Camera")

beh_of_interest <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                     "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                     "Treadmill Walking at slower pace than normal", "Off Camera", "Light Calisthenics", "Dusting", "Computer Work")

# Function to select videos
select_video <- function(folder_path) {
  # Select file
  svDialogs::dlg_message("Select the desired video (must be an excel file with one sheet) to undergo quality control.")
  comp_name <- file.choose()
  raw_file <- readxl::read_excel(comp_name)
  
  # Process file name
  comp_name <- gsub("\\\\", "/", comp_name)
  vid_name <- gsub(paste(folder_path, sep=""), "", comp_name)
  
  # Extract pattern, initials, and video number
  pattern <- stringr::str_extract(vid_name, "[A-Z]{2,3} - CUAMC_\\d+")
  initials <- stringr::str_extract(pattern, "[A-Z]{2,3}")
  vid_num <- stringr::str_extract(pattern, "CUAMC_\\d+")
  
  # Manual input if extraction fails
  if(is.na(initials)){
    initials <- svDialogs::dlg_input(message = "Enter initials of coder. (ex: NR)")$res
  }
  if(is.na(vid_num)){
    vid_num <- svDialogs::dlg_input(message = "Enter which participant video was selected. (ex: CUAMC_01)")$res
  }
  
  # Return results as a list
  return(list(
    raw_file = raw_file,
    comp_name = comp_name,
    vid_name = vid_name,
    initials = initials,
    vid_num = vid_num
  ))
}

# Function to get video info - alternative to select_video for use in shiny
get_video_info_shiny <- function(file_name) {
  # Select file
  comp_name <- c(file_name)
  
  # Extract pattern, initials, and video number
  pattern <- stringr::str_extract(comp_name, "[A-Z]{2,3} - CUAMC_\\d+")
  initials <- stringr::str_extract(pattern, "[A-Z]{2,3}")
  vid_num <- stringr::str_extract(pattern, "CUAMC_\\d+")
  
  # Return results as a list
  return(list(
    comp_name = comp_name,
    initials = initials,
    vid_num = vid_num
  ))
}

# Function to fill modifier columns with values for behaviors that have no modifier
fill_blank_mod <- function(behaviors_no_mod, values_to_check, replace_values) {
  # Find the indices where values_to_check match behaviors_no_mod
  indices_to_replace <- which(values_to_check %in% behaviors_no_mod)
  
  # Replace corresponding elements with "no modifier"
  replace_values[indices_to_replace] <- "no modifier"
  
  return(replace_values)
}

# Function to clean raw files to ready for percent agreement and graphing
clean_raw_file <- function(video_info){
  
  raw_file <- video_info$raw_file
  initials <- video_info$initials
  vid_num <- video_info$vid_num
  
  beh <- raw_file %>% select(Time_Relative_sf, Behavior)
  
  # Set up modifier data frames
  mod <- raw_file %>% select(Time_Relative_sf, Modifier_1)
  
  # Fill in blank modifiers to separate from true NAs
  mod$Modifier_1 <- fill_blank_mod(behaviors_no_mod, beh$Behavior, mod$Modifier_1)
  
  # Remove rows with NA
  beh <- na.omit(beh)
  
  # Saving order of variables to correct order later
  beh_order <- beh$Behavior
  
  mod_order <- mod$Modifier_1
  
  paste_beh <- paste("Behavior", initials, vid_num, sep="_")
  paste_mod <- paste("Modifier_1", initials, vid_num, sep="_")
  
  tall_data_beh <- data.frame(Time_Relative_sf = beh$Time_Relative_sf,
                              source = rep(paste_beh, nrow(beh)),
                              behavior = beh$Behavior)
  
  tall_data_mod <- data.frame(Time_Relative_sf = mod$Time_Relative_sf,
                              source = rep(paste_mod, nrow(mod)),
                              modifier = mod$Modifier_1)
  return(list(
    tall_data_beh = tall_data_beh,
    tall_data_mod = tall_data_mod
  ))
}

# Function to clean raw files to ready for percent agreement and graphing
clean_raw_file_shiny <- function(raw_file, vid_name){
  
  raw_file <- raw_file
  vid_name <- vid_name
  
  beh <- raw_file %>% select(Time_Relative_sf, Behavior)
  
  # Set up modifier data frames
  mod <- raw_file %>% select(Time_Relative_sf, Modifier_1)
  
  # Fill in blank modifiers to separate from true NAs
  mod$Modifier_1 <- fill_blank_mod(behaviors_no_mod, beh$Behavior, mod$Modifier_1)
  
  # Remove rows with NA
  beh <- na.omit(beh)
  
  # Saving order of variables to correct order later
  beh_order <- beh$Behavior
  
  mod_order <- mod$Modifier_1
  
  paste_beh <- paste("Behavior", vid_name, sep="_")
  paste_mod <- paste("Modifier_1", vid_name, sep="_")
  
  tall_data_beh <- data.frame(Time_Relative_sf = beh$Time_Relative_sf,
                              source = rep(paste_beh, nrow(beh)),
                              behavior = beh$Behavior)
  
  tall_data_mod <- data.frame(Time_Relative_sf = mod$Time_Relative_sf,
                              source = rep(paste_mod, nrow(mod)),
                              modifier = mod$Modifier_1)
  return(list(
    tall_data_beh = tall_data_beh,
    tall_data_mod = tall_data_mod
  ))
}

# Calculating percent agreement function
calc_perc_agreement <- function(frame1, frame2){
  
  # Find the maximum time in each dataset
  max_time <- max(max(frame1$Time_Relative_sf, na.rm = TRUE), max(frame2$Time_Relative_sf, na.rm = TRUE))
  
  # Create a sequence of time from 0 to the maximum time in milliseconds
  time_sequence <- seq(0, max_time, by = 0.1)
  
  # Create a dataframe with the time column
  new_df <- data.frame(Time_Relative_sf = time_sequence)
  
  # Add columns for behavior_1 and behavior_2
  new_df$behavior_1 <- NA
  new_df$behavior_2 <- NA
  
  # Add start_stop column to frame1 and frame2
  frame1$start_stop <- rep(c("start", "stop"), length.out = nrow(frame1))
  frame2$start_stop <- rep(c("start", "stop"), length.out = nrow(frame2))
  
  # Fill in new_df$behavior_1
  frame1$Time_Relative_sf <- as.numeric(format(frame1$Time_Relative_sf, digits = 3, nsmall = 1))
  new_df$Time_Relative_sf <- as.numeric(format(new_df$Time_Relative_sf, nsmall = 1))
  
  rows_to_snag <- seq(from = 1, to = nrow(frame1), by = 2)
  for(i in rows_to_snag){
    start_row <- which(new_df$Time_Relative_sf %in% frame1$Time_Relative_sf[i])
    
    if(i < nrow(frame1)){
      end_row <- which(new_df$Time_Relative_sf %in% frame1$Time_Relative_sf[i+1])-1
    }else{
      end_row <- nrow(new_df)
    }
    
    behavior <- frame1[i,3] # Grabbing either behavior or modifier column
    new_df$behavior_1[start_row:end_row] <- behavior
  }
  
  # Fill in new_df$behavior_2
  frame2$Time_Relative_sf <- as.numeric(format(frame2$Time_Relative_sf, digits = 3, nsmall = 1))
  
  rows_to_snag <- seq(from = 1, to = nrow(frame2), by = 2)
  for(i in rows_to_snag){
    start_row <- which(new_df$Time_Relative_sf %in% frame2$Time_Relative_sf[i])
    
    if(i < nrow(frame1)){
      end_row <- which(new_df$Time_Relative_sf %in% frame2$Time_Relative_sf[i+1])-1
    }else{
      end_row <- nrow(new_df)
    }
    
    behavior <- frame2[i,3] # Grabbing either behavior or modifier column
    new_df$behavior_2[start_row:end_row] <- behavior
  }
  
  percent_agreement <- format(mean(new_df$behavior_1 == new_df$behavior_2, na.rm = TRUE) * 100, digits = 3, nsmall = 1)
  
  return(list(
    percent_agreement = percent_agreement,
    agreement_frame = new_df,
    max_time = max_time))
}

# Making Other last in factor function
make_last_factor <- function(vec, last_value) {
  # Convert to factor
  vec_factor <- as.factor(vec)
  
  # Get all levels
  all_levels <- levels(vec_factor)
  
  # Remove the last_value from levels and add it to the end
  new_levels <- c(setdiff(all_levels, last_value), last_value)
  
  # Reorder the factor
  vec_factor <- factor(vec_factor, levels = new_levels)
  
  return(vec_factor)
}

# Adding Other rows for creating behavior of interest only dataframes
add_other_rows <- function(df) {
  # Ensure the dataframe is ordered by Time_Relative_sf
  df <- df[order(df$Time_Relative_sf),]
  
  # Create a new dataframe to store the results
  new_df <- data.frame()
  
  # Loop through the dataframe, adding new rows after every 2 original rows
  for (i in seq(1, nrow(df), by = 2)) {
    # Add the original two rows
    new_df <- rbind(new_df, df[i,])
    if (i+1 <= nrow(df)) {
      new_df <- rbind(new_df, df[i+1,])
      
      # Add the first 'Other' row
      other_row1 <- df[i+1,]
      other_row1$behavior <- 'Other'
      new_df <- rbind(new_df, other_row1)
      
      # Add the second 'Other' row
      if (i+2 <= nrow(df)) {
        other_row2 <- df[i+2,]
        other_row2$behavior <- 'Other'
        new_df <- rbind(new_df, other_row2)
      }
    }
  }
  
  # Reset row names
  rownames(new_df) <- NULL
  
  return(new_df)
}

# Plotting function
plot_annotation <- function(video1_info, video1_data, video2_info = NULL, video2_data = NULL, agree_list = NULL) {
  y_column <- if("behavior" %in% names(video1_data)) "behavior" else if("modifier" %in% names(video1_data)) "modifier"
  
  video1_data[[y_column]] <- make_last_factor(video1_data[[y_column]], "Other")
  
  plot <- ggplot() +
    geom_path(data = video1_data, aes(x = Time_Relative_sf, y = .data[[y_column]], group = 1, color = paste(video1_info$initials))) +
    theme_minimal() +
    labs(x = "Relative Time (s)", y = stringr::str_to_title(y_column), color = "Annotator") +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = seq(0, ceiling(max(video1_data$Time_Relative_sf)), by = 600)) +
    scale_y_discrete(limits = rev(levels(video1_data[[y_column]])))
  
  if (!is.null(video2_info) && !is.null(video2_data)) {
    video2_data[[y_column]] <- make_last_factor(video2_data[[y_column]], "Other")
    
    plot <- plot +
      geom_path(data = video2_data, aes(x = Time_Relative_sf, y = .data[[y_column]], group = 1, color = paste(video2_info$initials)))
  }
  
  if (!is.null(agree_list)) {
    # plot <- plot +
    #   labs(title = paste(video1_info$vid_num, " Percent agreement: ",
    #                      agree_list$percent_agreement, "%", sep=""))
    
    # Update x-axis scale if agree_list is provided
    plot <- plot +
      scale_x_continuous(breaks = seq(0, ceiling(agree_list$max_time), by = 600))
  } else {
    plot <- plot +
      labs(title = paste(video1_info$vid_num, " Data"))
  }
  
  return(plot)
}

# Plotting function - shiny
plot_annotation_shiny <- function(vid1_name, video1_data, vid2_name = NULL, video2_data = NULL, agree_list = NULL) {
  y_column <- if("behavior" %in% names(video1_data)) "behavior" else if("modifier" %in% names(video1_data)) "modifier"
  
  video1_data[[y_column]] <- make_last_factor(video1_data[[y_column]], "Other")
  
  video1_initials <- stringr::str_extract(vid1_name, "[A-Z]{2,3}")
  video2_initials <- stringr::str_extract(vid2_name, "[A-Z]{2,3}")
  
  plot <- ggplot() +
    geom_path(data = video1_data, aes(x = Time_Relative_sf, y = .data[[y_column]], group = 1, color = paste(video1_initials))) +
    theme_minimal() +
    labs(x = "Relative Time (s)", y = stringr::str_to_title(y_column), color = "Annotator") +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = seq(0, ceiling(max(video1_data$Time_Relative_sf)), by = 600)) +
    scale_y_discrete(limits = rev(levels(video1_data[[y_column]])))
  
  if (!is.null(vid2_name) && !is.null(video2_data)) {
    video2_data[[y_column]] <- make_last_factor(video2_data[[y_column]], "Other")
    
    plot <- plot +
      geom_path(data = video2_data, aes(x = Time_Relative_sf, y = .data[[y_column]], group = 1, color = paste(video2_initials)))
  }
  
  if (!is.null(agree_list)) {
    # plot <- plot +
    #   labs(title = paste(video1_info$vid_num, " Percent agreement: ",
    #                      agree_list$percent_agreement, "%", sep=""))
    
    # Update x-axis scale if agree_list is provided
    plot <- plot +
      scale_x_continuous(breaks = seq(0, ceiling(agree_list$max_time), by = 600))
  } else {
    plot <- plot +
      labs(title = paste(vid1_name, " Data"))
  }
  
  return(plot)
}

# Make plottable data function - relies on clean_raw_file function
ready_to_plot <- function(list){
  
  beh_frame <- clean_raw_file(list)$tall_data_beh
  mod_frame <- clean_raw_file(list)$tall_data_mod
  
  beh_int_frame <- beh_frame
  beh_int_frame$behavior <- ifelse(beh_int_frame$behavior %in% beh_of_interest, beh_int_frame$behavior, "Other")
  
  mod_int_frame <- mod_frame
  other_rows <- which(beh_int_frame$behavior %in% "Other")
  for(i in other_rows){mod_int_frame$modifier[i] <- "Other"}
  
  return(list(
    beh_frame = beh_frame,
    mod_frame = mod_frame,
    beh_int_frame = beh_int_frame,
    mod_int_frame = mod_int_frame
  ))
  
}

# Make plottable data function - relies on clean_raw_file function
ready_to_plot_shiny <- function(raw_file, vid_name){
  
  beh_frame <- clean_raw_file_shiny(raw_file, vid_name)$tall_data_beh
  mod_frame <- clean_raw_file_shiny(raw_file, vid_name)$tall_data_mod
  
  beh_int_frame <- beh_frame
  beh_int_frame$behavior <- ifelse(beh_int_frame$behavior %in% beh_of_interest, beh_int_frame$behavior, "Other")
  
  mod_int_frame <- mod_frame
  other_rows <- which(beh_int_frame$behavior %in% "Other")
  for(i in other_rows){mod_int_frame$modifier[i] <- "Other"}
  
  return(list(
    beh_frame = beh_frame,
    mod_frame = mod_frame,
    beh_int_frame = beh_int_frame,
    mod_int_frame = mod_int_frame
  ))
  
}

# Make 4 panel interactive plot
make_interactive_plots <- function(plot1, plot2, plot3, plot4){

  # Create ggplotly objects
  plot_beh_interactive <- ggplotly(plot1)
  plot_beh_int_interactive <- ggplotly(plot2)
  
  plot_mod_interactive <- ggplotly(plot3)
  plot_mod_int_interactive <- ggplotly(plot4)
  
  combined_plot <- subplot(plot_beh_int_interactive, plot_beh_interactive,
                           plot_mod_int_interactive, plot_mod_interactive,
                           nrows = 4, shareX = TRUE) %>%
    layout(
      yaxis = list(fixedrange = TRUE),
      yaxis2 = list(fixedrange = TRUE),
      yaxis3 = list(fixedrange = TRUE),
      yaxis4 = list(fixedrange = TRUE)
    )
  
  return(combined_plot)
  
}

# Check transitions
check_transitions <- function(video_data){
  t_rows <- which(video_data$behavior %in% "Transition")
  t_rows <- t_rows[seq(1, length(t_rows), by=2)]
  
  rows_over_15 <- c()
  for(i in t_rows){
    if((video_data$Time_Relative_sf[i+1] - video_data$Time_Relative_sf[i]) >=15){
      rows_over_15 <- c(rows_over_15, i)
    }
  }
  if(length(rows_over_15) == 0){
    print("No transitions are over 15 seconds.")
  }else{
    t_frame <- video_data[rows_over_15,] # Initialize empty frame
    for(i in 1:nrow(t_frame)){
      cur_t_row <- rows_over_15[i]
      t_frame$duration[i] <- video_data$Time_Relative_sf[cur_t_row+1] - video_data$Time_Relative_sf[cur_t_row]
    }
  }
  return(t_frame)
}

# Check quick alternating activities
check_alternating <- function(video_data, number_seqs = 5, seq_duration = 10){
  video_data$duration <- NA
  rows_to_fill <- seq(1, nrow(video_data), by=2)
  
  for(i in rows_to_fill){
    video_data$duration[i] <- video_data$Time_Relative_sf[i+1] - video_data$Time_Relative_sf[i]
  }
  video_data2 <- video_data %>% na.omit()
  
  # Find sequential durations below threshold of seq_duration
  below_duration <- video_data2$duration < seq_duration
  runs <- rle(below_duration)
  
  long_runs <- which(runs$values == TRUE & runs$lengths >= number_seqs)
  
  ends <- cumsum(runs$lengths)
  end_indices <- ends[long_runs]
  start_indices <- (end_indices - runs$lengths[long_runs]) + 1
  
  for(i in 1:length(start_indices)){
    if(!exists("new_vector")){
      new_vector <- c(start_indices[i]:end_indices[i])
    }else{
      new_vector <- c(new_vector, start_indices[i]:end_indices[i])
    }
  }
  
  video_data2 <- video_data2[new_vector,]
  
  return(video_data2)

}

# Check for long periods of activity
check_long_activities <- function(video_data, duration = 900){
  video_data$duration <- NA
  rows_to_fill <- seq(1, nrow(video_data), by=2)
  
  for(i in rows_to_fill){
    video_data$duration[i] <- video_data$Time_Relative_sf[i+1] - video_data$Time_Relative_sf[i]
  }
  video_data2 <- video_data %>% na.omit()
  
  long_indices <- which(video_data2$duration >= duration)
  
  video_data2 <- video_data2[long_indices,]
  
  return(video_data2)
}

# Check for comments
check_comments <- function(raw_file){
  start_time <- raw_file$Comment[1]
  
  comment_rows <- which(complete.cases(raw_file$Comment))
  
  comment_frame <- raw_file[comment_rows,] %>% select(Time_Relative_sf, Behavior, Modifier_1, Event_Type, Comment)
  
  return(list(
    start_time = start_time,
    comment_frame = comment_frame
  ))
}

# Calculate ICC function
calculate_ICC <- function(agreement_frame){
  list_all <- factor(unique(c(agreement_frame$behavior_1, agreement_frame$behavior_2)))
  
  agreement_frame$behavior_1 <- factor(agreement_frame$behavior_1, levels = list_all)
  agreement_frame$behavior_2 <- factor(agreement_frame$behavior_2, levels = list_all)
  
  agreement_frame$behavior_1_numeric <- as.numeric(factor(agreement_frame$behavior_1))
  agreement_frame$behavior_2_numeric <- as.numeric(factor(agreement_frame$behavior_2))
  
  ratings <- cbind(agreement_frame$behavior_1_numeric, agreement_frame$behavior_2_numeric)
  
  icc_result <- icc(ratings, model = "twoway", type = "agreement", unit = "single")
  
  return(icc_result)
  
}
