## Data Cleaning of Webscraped Files -- NBA Game Players Stats

```{r}
##CHECK DATA TYPES, MISSING VALUES, DUPLICATE VALUES
# Set folder path
folder_path <- "../Web-Scraping-Stats/raw_stats_data/PlayerStats"

# List all files in the folder
file_paths <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Loop over files
for (file_path in file_paths) {
  # Read the CSV file
  data <- read.csv(file_path, header = TRUE)
  
  # need to reformat due to style of table
  data <- data[-1, ]
  colnames(data) <- data[1, ]
  data <- data[-1, ]
  data <- data[-(6:7), ]
  
  # Print file name for reference
  print(paste("Analyzing file:", file_path))
  
  # Get data types of columns as a comma-separated list
  data_types <- paste(sapply(data, class), collapse = ",")
  print(paste("Data Types:", data_types))
  
  # Count total missing values in the file
  total_missing <- sum(is.na(data))
  print(paste("Total Missing Values:", total_missing))
  
  # Count duplicate rows
  duplicate_count <- sum(duplicated(data))
  print(paste("Duplicate Rows Count:", duplicate_count))
}
```

```{r}
##CHECK FOR NORMALITY
# Load the necessary library for KS test
library(MASS)

# Set folder path
folder_path <- "../Web-Scraping-Stats/raw_stats_data/PlayerStats"

# List all files in the folder
file_paths <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store normality test results
normality_test_results <- list()

# Loop over files
for (file_path in file_paths) {
  # Read the CSV file
  data <- read.csv(file_path, header = TRUE)
  
  # need to reformat due to style of table
    data <- data[-1, ]
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data <- data[-(6:7), ]
  
  # Perform KS test for normality on numeric columns
  numeric_columns <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_columns]
  
  # Initialize a logical variable to store normality result for this file
  is_normal <- TRUE
  
  for (col in colnames(numeric_data)) {
    ks_result <- ks.test(numeric_data[[col]], "pnorm", mean = mean(numeric_data[[col]]), sd = sd(numeric_data[[col]]))
    
    # If p-value is less than or equal to 0.05, set is_normal to FALSE
    if (ks_result$p.value <= 0.05) {
      is_normal <- FALSE
      break  # No need to check other columns once non-normality is detected
    }
  }
  
  # Store the normality result for this file
  normality_test_results[basename(file_path)] <- is_normal
}

# Print or analyze the normality test results as needed
print(normality_test_results)
```


```{r}
## CLEAN DATA & REFORMAT
library(chron) #for changing Minutes Played to time column

cleanRawData <- function(input_folder, output_folder) {
  # List over all files
  file_paths <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop over files
  for (file_path in file_paths) {
    # Extract year from the file name using regular expression
    year <- as.numeric(gsub(".*(\\d{4})_Table_[23].csv", "\\1", basename(file_path)))
    
    # Read CSV file
    data <- read.csv(file_path, header = FALSE)
    
    # Remove cols and make header
    data <- data[-1, ]
    colnames(data) <- data[1, ]
    data <- data[-1, ]
    data <- data[-(6:7), ]
    
    # Rename first col
    colnames(data)[1] <- "Players"
    
    # Create col for starters v reserves
    data$Player_Type <- ifelse(1:nrow(data) <= 5, "Starter", "Reserve")
    
    # Create col for east or west
    data$Region <- ifelse(any(data$Tm %in% c("LAL", "UTA", "PHO")), "West", "East")
    
    # Remove totals (we can calculate later when doing team analysis)
    data <- data[-nrow(data), ]
    
    # Check if the first row in Region column is East or West
    region <- data$Region[1]
    
    # Add a new column for the year
    data$Year <- year
    
    # Convert the range of columns to integer values
    data[, 4:18] <- lapply(data[, 4:18], as.integer)
    
    # Convert the range of columns to float values
    data[, 19:21] <- lapply(data[, 19:21], as.numeric)
    
    # Convert Minutes Played to a times column
    # ???
    
    # Get data types of columns as a comma-separated list
    data_types <- paste(sapply(data, class), collapse = ",")
    print(paste("Data Types:", data_types))
    
    # Create file name based on year and region
    file_name <- paste0(year, "_Stats_", region, ".csv")
    
    # Construct the file path
    new_file_path <- file.path(output_folder, file_name)
    
    # Create a new empty file (or overwrite if it already exists)
    file.create(new_file_path)
    
    # Write data to the file
    write.csv(data, new_file_path, row.names = FALSE)
    
    # Print for debugging
    print(paste("File created at:", new_file_path))
  }
}
```

```{r}
# Usage
input_folder <- "../Web-Scraping-Stats/raw_stats_data/PlayerStats"
output_folder <- "../Web-Scraping-Stats/cleaned_stats_data"

cleanRawData(input_folder, output_folder)
```

```{r}
library(dplyr)

# Pull data from folder
folder_path <- "../Web-Scraping-Stats/cleaned_stats_data"

# List files
csv_files <- list.files(path=folder_path, full.names=TRUE)

# Read and merge files
merged_data <- lapply(csv_files, read.csv) %>%
  bind_rows()

# Save merged data to new file
write.csv(merged_data, file="../Web-Scraping-Stats/cleaned_stats_data/player_stats_combined.csv", row.names=FALSE)
```

