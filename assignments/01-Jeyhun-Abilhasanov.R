# Load required libraries
library(data.table)
library(httr)

# Set the URL of the zip file and the output file name
file_url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
zip_file <- "basin_timeseries_v1p2_modelOutput_daymet.zip"

# Save current working directory and define where to extract files
work_dir <- getwd()
extract_to <- file.path(work_dir, "model_output_daymet")

# Try downloading the file with timeout
tryCatch({
  GET(file_url, write_disk(zip_file, overwrite = TRUE), timeout(1000))
  message("✅ File downloaded successfully.")
}, error = function(e) {
  message("❌ Failed to download the file: ", e$message)
})

# Unzip the file if download succeeded
if (file.exists(zip_file)) {
  unzip(zip_file, exdir = extract_to)
  message("✅ File extracted successfully.")
} else {
  stop("❌ Download failed. Zip file not found.")
}

# Get all directories including subdirectories
directories <- list.dirs(extract_to, recursive = TRUE, full.names = TRUE)

# Prepare a list to collect data
all_data <- list()

# Loop through folders and search for files containing "model_output"
for (path in directories) {
  file_list <- list.files(path, full.names = TRUE)
  output_files <- grep("model_output", file_list, value = TRUE)
  
  for (file in output_files) {
    dt <- fread(file)
    all_data[[length(all_data) + 1]] <- dt
  }
}

# Merge all data tables into one
combined_data <- rbindlist(all_data, use.names = TRUE, fill = TRUE)

# Remove rows with missing values
cleaned_data <- na.omit(combined_data)

# Save cleaned data to a CSV file
write.csv(cleaned_data, "cleaned_model_output.csv", row.names = FALSE)

# Notes:
# - Uses httr::GET to download with timeout
# - Uses data.table::fread for fast data reading
# - Searches recursively for files with "model_output" in their names
# - Combines all data and removes rows with NA values
