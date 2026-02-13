# Load necessary libraries
library(data.table)
library(httr)

# Define the file URL and the directory where the file will be saved
url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
output_zip <- "basin_timeseries_v1p2_modelOutput_daymet.zip"

# Save the current working directory and define a new one for extraction
current_dir <- getwd()
extracted_dir <- paste0(current_dir, "/model_output_daymet/")

# Download the file with a timeout option
tryCatch({
  GET(url, write_disk(output_zip, overwrite = TRUE), timeout(1000))
  message("File successfully downloaded!")
}, error = function(e) {
  message("Error in downloading file: ", e$message)
})

# Unzip the file if downloaded
if (file.exists(output_zip)) {
  unzip(output_zip, exdir = extracted_dir)
  message("File unzipped successfully!")
} else {
  stop("File did not download correctly!")
}

# Get list of folders in the extracted directory
folders <- list.dirs(extracted_dir, full.names = TRUE, recursive = TRUE)

# Initialize an empty list to store data from matching files
matching_data <- list()

# Search for files with "model_output" in the name within each folder
for (folder in folders) {
  files <- list.files(folder, full.names = TRUE)
  model_files <- grep("model_output", files, value = TRUE)
  
  # Read matching files and store them in the list
  for (file in model_files) {
    data <- fread(file)  # Read the file using fread for efficiency
    matching_data <- append(matching_data, list(data))
  }
}

# Combine all the data into a single data table
final_data <- rbindlist(matching_data, use.names = TRUE, fill = TRUE)

# Remove any rows with NA values
final_data_clean <- na.omit(final_data)

# Save the final cleaned data as a CSV file
write.csv(final_data_clean, "cleaned_model_output.csv", row.names = FALSE)

# Summary:
# This method uses HTTR for file downloading and fread for reading large files efficiently.
# The approach processes all subfolders and finds files with "model_output" in their name.
# We ensure no NA values are in the final dataset and save it in CSV format.
