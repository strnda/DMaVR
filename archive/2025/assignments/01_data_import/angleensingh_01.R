# Set the working directory to the desired folder
setwd("C:/Users/Admin/Desktop/rprog")

# Set a longer timeout to prevent download interruptions
options(timeout = 1800)

# URL of the CAMELS dataset
dataset_url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"

# File name for the downloaded ZIP file
dataset_zip_path <- "camels_basin_timeseries.zip"

# Folder where the data will be extracted
extracted_data_dir <- "camels_data"

# Remove any existing ZIP file to ensure a fresh download
file.remove(dataset_zip_path)

# Download the ZIP file from the specified URL
download.file(dataset_url, dataset_zip_path, mode = "wb")

# Check if the ZIP file was downloaded successfully
if (!file.exists(dataset_zip_path)) {
  download.file(dataset_url, dataset_zip_path, mode = "wb")
  message("Download completed.")
} else {
  message("ZIP file already exists.")
}

# Unzip the dataset if the folder doesn't already exist
if (!dir.exists(extracted_data_dir)) {
  unzip(dataset_zip_path, exdir = extracted_data_dir)
  message("Data unzipped.")
} else {
  message("Extracted data folder already exists.")
}

# List all files (recursively) in the extracted folder
all_dataset_files <- list.files(extracted_data_dir, recursive = TRUE, full.names = TRUE)

# Filter for files that contain 'model_output' in their names (case-insensitive)
model_output_files <- grep("model_output", all_dataset_files, value = TRUE, ignore.case = TRUE)

# Check that the data folder exists
dir.exists(extracted_data_dir)

# Check the size of the ZIP file (in bytes)
file.info(dataset_zip_path)$size

# Stop the script if no model output files are found
if (length(model_output_files) == 0) {
  stop("No model output files found in the dataset.")
}

# Initialize a list to store the imported data from model output files
imported_model_data <- list()

# Read each model output file and store the data in the list
for (model_file in model_output_files) {
  # Read the CSV-like file into a data frame
  data <- read.table(model_file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # Store the data frame using the file's base name as the list key
  imported_model_data[[basename(model_file)]] <- data
  
  # Print message for each imported file
  message("Imported: ", basename(model_file))
}

# Print total number of model output files successfully imported
message("Total number of model output files imported: ", length(imported_model_data))