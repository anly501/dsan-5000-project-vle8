## WEBSCRAPING FOR NBA ALL STAR GAME STATS 1980-2010 (NO 1999)

library(rvest)
library(dplyr)

# define base URL
base_url <- "https://www.basketball-reference.com/allstar/NBA_"

# define years (excluding 1999 b/c no NBA All-Star Game)
years <- setdiff(1980:2010, 1999)

# loop through the years
for (year in years) {
  # construct the full URL for the current year
  url <- paste0(base_url, year, ".html")
  
  # Read HTML content from the URL
  webpage <- read_html(url)
  
  # Extract all the table elements from the webpage
  tables <- webpage %>%
    html_nodes("table")
  
  # Loop through the tables
  for (i in 1:length(tables)) {
    # Define the file name
    if (i == 1) {
      file_name <- paste0(year, "_Score.csv")
    } else if (i == 2) {
      file_name <- paste0(year, "_Table_2.csv")
    } else if (i == 3) {
      file_name <- paste0(year, "_Table_3.csv")
    } else {
      # Default naming for tables beyond the third table
      file_name <- paste0(year, "_Table_", i, ".csv")
    }
    
    cat("Saving table", i, "for", year, "to", file_name, "...\n")
    
    # Extract the table data
    table_data <- tables[[i]] %>%
      html_table()
    
    # Save the table as a CSV file
    write.csv(table_data, file_name, row.names = FALSE)
    
    cat("Table", i, "saved as", file_name, "\n\n")
  }
}
