# Clean & merge Game Scores

# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Specify the folder path
folder_path <- "./Web-Scraping-Stats/raw_stats_data/GameScores"

# List all files
csv_files <- list.files(path = folder_path, full.names = TRUE)

# Function to process a CSV file
process_csv <- function(file_path) {
  # Extract year from the file name
  year <- as.integer(str_extract(basename(file_path), "\\d{4}"))
  
  # Read CSV file, skip first row
  data <- read.csv(file_path, skip = 1, stringsAsFactors = FALSE)
  
  # Add a new column "Year"
  data$Year <- year
  
  # Add column "OT" and fill with 0 if not present in the data
  data <- data %>%
    mutate(OT = ifelse("OT" %in% colnames(data), OT, 0))
  
  # Rename columns
  colnames(data)[1:5] <- c("Team", "Q1", "Q2", "Q3", "Q4")
  
  # Create the processed file path with "_processed.csv" suffix
  processed_file_path <- gsub(".csv$", "_processed.csv", basename(file_path))
  
  # Write the processed data to the file
  write.csv(data, file = file.path(folder_path, processed_file_path), row.names = FALSE)
  
  return(data)
}

# Process each file and save data
processed_data <- lapply(csv_files, process_csv)

# Bind all processed data frames into one
merged_data <- bind_rows(processed_data)

# Reorder columns to match the desired order
merged_data <- merged_data[, c("Team", "Q1", "Q2", "Q3", "Q4", "OT", "T", "Year")]

# Print the processed data
print(merged_data)

# Save merged data to new file
write.csv(merged_data, "./Web-Scraping-Stats/cleaned_stats_data/game_scores.csv", 
          row.names=FALSE)