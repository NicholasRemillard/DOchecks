# Functions for use in training_video_comparison_script.R

plot_comparison <- function(criterion_data, comparison_data, input_column) {
  
  criterion <- criterion_data %>% select(Time_Relative_sf, !!sym(input_column))
  compare <- comparison_data %>% select(Time_Relative_sf, !!sym(input_column))
  
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
  combined_plot <- grid.arrange(my_plot, top = paste(vid_num, ": ", did_pass, sep=""))
  
  return(combined_plot)
  
}
