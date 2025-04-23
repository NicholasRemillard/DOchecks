# Script to compare training videos for accuracy and improvement
# Created by Nick Remillard 2/8/2024
# For DO Coding Training Purposes Only

# Script last updated: 2/19/2025

rm(list = ls())

# SEC 1: Load necessary packages and master file----

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(svDialogs)
library(gridExtra)
library(stringr)
library(writexl)
library(lubridate)

source("training_video_comparison_functions.R")

# Onedrive
parent_folder <- c("C:/Users/nickr/OneDrive - University of Tennessee/PAAL Undergrad Docs/02 DO Coding/02 DO Training Files/")

ready_check_child <- c("Ready-to-check Observer Excel Files")
criterion_child <- c("Criterion Video Files")

setwd(parent_folder)

# Check if master file exists
training_file_name <- "annotation_training_stats.xlsx"

# Check if the file exists in the current working directory
if (file.exists(training_file_name)) {
  training_excel <- read_excel(training_file_name)
} else {
  training_excel <- data.frame(Initials = NA, Date = NA, Trained_on_video = NA,
                               Training_num = NA, Attempt_num = NA, Retest = NA,
                               Beh_agreement = NA, Mod_agreement = NA)
}


# SEC 2: Choose files ----
setwd(paste(parent_folder, ready_check_child, sep=""))
dlg_message("Select the desired training video (must be an excel file with one sheet) to compare to criterion.")
comp_name <- file.choose()
compare_frame <- read_excel(comp_name)

pattern <- str_extract(comp_name, "[A-Z]{2,3} - [tT]raining_\\d+")
initials <- str_extract(pattern, "[A-Z]{2,3}")
vid_num <- str_extract(pattern, "[tT]raining_\\d+")

attempt_num <- as.numeric(dlg_input(message = "Which attempt is this? Enter an integer. (Ex: 1)")$res)

retest_yn <- dlg_input(message = "Yes this a retest in a new semester? yes/no", default = "no")$res

trained_on_vid_yn <- dlg_input(message = "Was this person trained on the new online training presentation (Started Spring 2025)? yes/no",
                               default = "yes")$res


creation_date <- file.info(comp_name)$mtime
creation_date <- force_tz(creation_date, tzone = "UTC")

# If above didn't work, choose manually
if(initials %in% NA){
  initials <- dlg_input(message = "Enter initials of coder. (ex: NR)")$res
}
if(vid_num %in% NA){
  vid_num <- dlg_input(message = "Enter which training video was selected. (ex: training_2)")$res
}

# Select criterion file
crit_path <- paste(parent_folder, criterion_child, sep="")
crit_files <- list.files(crit_path)
crit_name <- crit_files[grep(vid_num, crit_files)]
criterion_frame <- read_excel(paste(crit_path,crit_name, sep = "/"))

beh_plot <- plot_comparison(criterion_data = criterion_frame, comparison_data = compare_frame, input_column = "Behavior")
mod1_plot <- plot_comparison(criterion_data = criterion_frame, comparison_data = compare_frame, input_column = "Modifier_1")








# SEC 3: For modifiers, manually tell which behaviors do not have modifiers ----
behaviors_no_mod <- c("Treadmill Running at slower pace than normal", "Treadmill Running at faster pace than normal",
                      "Treadmill Walking at faster pace than normal", "Treadmill Walking at normal walking pace",
                      "Treadmill Walking at slower pace than normal", "Walking Around Room", "Off Camera")

fill_blank_mod <- function(behaviors_no_mod, values_to_check, replace_values) {
  # Find the indices where values_to_check match behaviors_no_mod
  indices_to_replace <- which(values_to_check %in% behaviors_no_mod)
  
  # Replace corresponding elements with "no modifier"
  replace_values[indices_to_replace] <- "no modifier"
  
  return(replace_values)
}



# SEC 4: Set up data frames ----

# Set up behavior data frames
criterion_b <- criterion %>% select(Time_Relative_sf, Behavior)
compare_b <- compare %>% select(Time_Relative_sf, Behavior)

# Set up modifier data frames
criterion_m <- criterion %>% select(Time_Relative_sf, Modifier_1)
compare_m <- compare %>% select(Time_Relative_sf, Modifier_1)

# Fill in blank modifiers to separate from true NAs
compare_m$Modifier_1 <- fill_blank_mod(behaviors_no_mod, compare_b$Behavior, compare_m$Modifier_1)
criterion_m$Modifier_1 <- fill_blank_mod(behaviors_no_mod, criterion_b$Behavior, criterion_m$Modifier_1)

# Remove rows with NA
compare_b <- na.omit(compare_b)
compare_m <- na.omit(compare_m)

# Saving order of variables to correct order later
criterion_b_order <- criterion_b$Behavior
compare_b_order <- compare_b$Behavior

criterion_m_order <- criterion_m$Modifier_1
compare_m_order <- compare_m$Modifier_1



# SEC 5: Make behavior and modifier frames tall ----

# Error occurred when running YP_training_5_2, run code below INSTEAD of SEC 5-7
# Merging data frames redundant/unnecessary for this code- consider removing

tall_data_b_crit <- data.frame(Time_Relative_sf = criterion_b$Time_Relative_sf,
                               source = rep("Behavior_criterion", nrow(criterion_b)),
                               behavior = criterion_b$Behavior)

tall_data_m_crit <- data.frame(Time_Relative_sf = criterion_m$Time_Relative_sf,
                               source = rep("Modifier_1_criterion", nrow(criterion_m)),
                               modifier = criterion_m$Modifier_1)

tall_data_b_comp <- data.frame(Time_Relative_sf = compare_b$Time_Relative_sf,
                               source = rep("Behavior_compare", nrow(compare_b)),
                               behavior = compare_b$Behavior)

tall_data_m_comp <- data.frame(Time_Relative_sf = compare_m$Time_Relative_sf,
                               source = rep("Modifier_1_compare", nrow(compare_m)),
                               modifier = compare_m$Modifier_1)


# Percent Agreement behavior ----

# Behaviors
# Find the maximum time in each dataset
max_time <- max(max(tall_data_b_crit$Time_Relative_sf, na.rm = TRUE), max(tall_data_b_comp$Time_Relative_sf, na.rm = TRUE))

# Create a sequence of time from 0 to the maximum time in milliseconds
time_sequence <- seq(0, max_time, by = 0.001)

# Create a dataframe with the time column
new_df <- data.frame(Time_Relative_sf = time_sequence)

# Add columns for behavior_1 and behavior_2
new_df$behavior_1 <- NA
new_df$behavior_2 <- NA

# Add start_stop column to tall_data_b_crit and tall_data_b_comp
tall_data_b_crit$start_stop <- rep(c("start", "stop"), length.out = nrow(tall_data_b_crit))
tall_data_b_comp$start_stop <- rep(c("start", "stop"), length.out = nrow(tall_data_b_comp))

# Fill in new_df$behavior_1
tall_data_b_crit$Time_Relative_sf <- as.numeric(format(tall_data_b_crit$Time_Relative_sf, digits = 3, nsmall = 3))
new_df$Time_Relative_sf <- as.numeric(format(new_df$Time_Relative_sf, nsmall = 3))

rows_to_snag <- seq(from = 1, to = nrow(tall_data_b_crit), by = 2)
for(i in rows_to_snag){
  start_row <- which(new_df$Time_Relative_sf %in% tall_data_b_crit$Time_Relative_sf[i])
  end_row <- which(new_df$Time_Relative_sf %in% tall_data_b_crit$Time_Relative_sf[i+1])-1
  behavior <- tall_data_b_crit$behavior[i]
  new_df$behavior_1[start_row:end_row] <- behavior
}


# Fill in new_df$behavior_2
tall_data_b_comp$Time_Relative_sf <- as.numeric(format(tall_data_b_comp$Time_Relative_sf, digits = 3, nsmall = 3))

rows_to_snag <- seq(from = 1, to = nrow(tall_data_b_comp), by = 2)
for(i in rows_to_snag){
  start_row <- which(new_df$Time_Relative_sf %in% tall_data_b_comp$Time_Relative_sf[i])
  end_row <- which(new_df$Time_Relative_sf %in% tall_data_b_comp$Time_Relative_sf[i+1])-1
  behavior <- tall_data_b_comp$behavior[i]
  new_df$behavior_2[start_row:end_row] <- behavior
}


percent_agreement_b <- format(mean(new_df$behavior_1 == new_df$behavior_2, na.rm = TRUE) * 100, digits = 3, nsmall = 1)
print(percent_agreement_b)

# Percent Agreement Modifier ----

# Modifier
# Find the maximum time in each dataset
max_time <- max(max(tall_data_m_crit$Time_Relative_sf, na.rm = TRUE), max(tall_data_m_comp$Time_Relative_sf, na.rm = TRUE))

# Create a sequence of time from 0 to the maximum time in milliseconds
time_sequence <- seq(0, max_time, by = 0.001)

# Create a dataframe with the time column
new_df_m <- data.frame(Time_Relative_sf = time_sequence)

# Add columns for behavior_1 and behavior_2
new_df_m$mod_1 <- NA
new_df_m$mod_2 <- NA

# Add start_stop column to tall_data_b_crit and tall_data_b_comp
tall_data_m_crit$start_stop <- rep(c("start", "stop"), length.out = nrow(tall_data_m_crit))
tall_data_m_comp$start_stop <- rep(c("start", "stop"), length.out = nrow(tall_data_m_comp))

# Fill in new_df_m$mod_1
tall_data_m_crit$Time_Relative_sf <- as.numeric(format(tall_data_m_crit$Time_Relative_sf, digits = 3, nsmall = 3))
new_df_m$Time_Relative_sf <- as.numeric(format(new_df_m$Time_Relative_sf, nsmall = 3))

rows_to_snag <- seq(from = 1, to = nrow(tall_data_m_crit), by = 2)
for(i in rows_to_snag){
  start_row <- which(new_df_m$Time_Relative_sf %in% tall_data_m_crit$Time_Relative_sf[i])
  end_row <- which(new_df_m$Time_Relative_sf %in% tall_data_m_crit$Time_Relative_sf[i+1])-1
  mod <- tall_data_m_crit$modifier[i]
  new_df_m$mod_1[start_row:end_row] <- mod
}


# Fill in new_df_m$mod_2
tall_data_m_comp$Time_Relative_sf <- as.numeric(format(tall_data_m_comp$Time_Relative_sf, digits = 3, nsmall = 3))

rows_to_snag <- seq(from = 1, to = nrow(tall_data_m_comp), by = 2)
for(i in rows_to_snag){
  start_row <- which(new_df_m$Time_Relative_sf %in% tall_data_m_comp$Time_Relative_sf[i])
  end_row <- which(new_df_m$Time_Relative_sf %in% tall_data_m_comp$Time_Relative_sf[i+1])-1
  mod <- tall_data_m_comp$modifier[i]
  new_df_m$mod_2[start_row:end_row] <- mod
}


percent_agreement_m <- format(mean(new_df_m$mod_1 == new_df_m$mod_2, na.rm = TRUE) * 100, digits = 3, nsmall = 1)
print(percent_agreement_m)

#calculate_training_ICC(new_df$behavior_1, new_df$behavior_2)

# Plotting ----

# Behavior plot
b_plot <- ggplot() +
  geom_path(data = tall_data_b_crit, aes(x = Time_Relative_sf, y = behavior, group = 1, color = "Criterion")) +
  geom_path(data = tall_data_b_comp, aes(x = Time_Relative_sf, y = behavior, group = 1, color = "Your coding")) +
  theme_minimal() +
  labs(x = "Relative Time (s)", y = "Behavior", title = paste("Behavior Comparison % agreement: ", percent_agreement_b, "%",sep="")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, ceiling(max_time), by = 60))

# Modifier plot
m_plot <- ggplot() +
  geom_path(data = tall_data_m_crit, aes(x = Time_Relative_sf, y = modifier, group = 1, color = "Criterion")) +
  geom_path(data = tall_data_m_comp, aes(x = Time_Relative_sf, y = modifier, group = 1, color = "Your coding")) +
  theme_minimal() +
  labs(x = "Relative Time (s)", y = "Modifier", title = paste("Modifier Comparison % agreement: ", percent_agreement_m, "%",sep="")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, ceiling(max_time), by = 60))

percent_agreement_b <- as.numeric(percent_agreement_b)
percent_agreement_m <- as.numeric(percent_agreement_m)

if(percent_agreement_b < 90.0 | percent_agreement_m < 90.0){
  did_pass <- c("At least one of the agreement values is below 90%, please recode")
}else{
  did_pass <- c("Both agreements at or above 90%, passed")
}

# Putting plots together
combined_plot <- grid.arrange(b_plot, m_plot, top = paste(vid_num, ": ", did_pass, sep=""))










# Saving plot
plot_path <- paste(parent_folder, "Training Graphs", sep="")
ggsave(paste(plot_path, "/", initials, "_", vid_num, "_", attempt_num, "_plot.png", sep=""), plot = combined_plot, width = 20, height = 10, dpi = 300)

# Save data
setwd(parent_folder)
if (file.exists(training_file_name)) {
  # Add a row and fill with data
  row_num <- nrow(training_excel)+1
  training_excel[row_num,] <- NA
  
  training_excel$Initials[row_num] <- initials
  training_excel$Date[row_num] <- creation_date
  training_excel$Trained_on_video[row_num] <- trained_on_vid_yn
  training_excel$Training_num[row_num] <- vid_num
  training_excel$Attempt_num[row_num] <- attempt_num
  training_excel$Retest[row_num] <- retest_yn
  training_excel$Beh_agreement[row_num] <- percent_agreement_b
  training_excel$Mod_agreement[row_num] <- percent_agreement_m
  
  write_xlsx(training_excel, "annotation_training_stats.xlsx")
  
} else {
  training_excel$Initials <- initials
  training_excel$Date <- creation_date
  training_excel$Trained_on_video <- trained_on_vid_yn
  training_excel$Training_num <- vid_num
  training_excel$Attempt_num <- attempt_num
  training_excel$Retest <- retest_yn
  training_excel$Beh_agreement <- percent_agreement_b
  training_excel$Mod_agreement <- percent_agreement_m
  
  #write_xlsx(training_excel, "annotation_training_stats.xlsx")
}
