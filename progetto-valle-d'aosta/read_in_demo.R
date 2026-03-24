# Read-in Demographic-Municipality data

# folder = "Bilanci_demografici"

library(tidyverse)

# 1. Define the parameters
folder_path <- "~/Desktop/Research/UNVDA/data/Bilanci_demografici"
years <- 2019:2024

# 2. Read, merge, and add the 'year' column
data_merged <- map_dfr(years, function(y) {
  # Construct the file path
  file_name <- paste0("demo_VDA_", y, ".csv")
  file_path <- file.path(folder_path, file_name)
  
  # Check if file exists to avoid errors
  if (file.exists(file_path)) {
    # Read the data. (Note: if your CSV uses semicolons instead of commas, 
    # replace read.csv with read.csv2 or read_csv2)
    df <- read.csv(file_path)
    
    # Add the year column
    df$year <- y
    
    return(df)
  } else {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
})

# 3. Reshape the dataset so every row corresponds to 1 Municipality per Year
data_wide <- data_merged %>%
  pivot_wider(
    # The columns that uniquely identify a single row
    id_cols = c(Codice.comune, Comune, year), 
    
    # The column containing the labels we want to turn into new columns
    names_from = Sesso,                       
    
    # The variables we want to split by gender and total
    values_from = -c(Codice.comune, Comune, Sesso, year), 
    
    # Format the new column names (e.g., "Popolazione.censita.al.1..gennaio_Maschi")
    names_glue = "{.value}_{Sesso}"           
  )

# View the result
head(data_wide)

data_totale <- data_merged %>%
  # Keep only the rows representing the total population
  filter(Sesso == "Totale") %>%
  # Remove the 'Sesso' column since it's now redundant
  select(-Sesso)

# View the result
head(data_totale)

setwd("~/Desktop/Research/UNVDA/data")
fwrite(data_totale, file = "demo_data.csv")

