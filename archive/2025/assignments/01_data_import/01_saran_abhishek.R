#set working dirctory to project folder
setwd("/Users/saran/Documents/R/Project_dmvr")

#increase timeout to allow or larger downloads
options(timeout = 1800)

# URL of the dataset to be downloaded
dataset_url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"

# Name of the zip file to be saved locally
zip_file <- "camels_basin_timeseries.zip"

# Folder where data will be extracted
data_folder <- "camels_data"

# Remove existing zip file if present
file.remove(zip_file)

# Download the dataset
download.file(dataset_url1, zip_file, mode = "wb")

# Check if the zip file was downloaded successfully
if (!file.exists(zip_file)) {
  download.file(dataset_url, zip_file, mode = "wb")
  cat("Download done")
} else {
  cat("ZIP file already exists")
}

# Unzip the file if the data folder doesn't already exist
if (!dir.exists(data_folder)) {
  unzip(zip_file, exdir = data_folder)
  cat("Unzipped")
} else {
  cat("Unzipped data folder already exists")
}

# List all files in the data folder recursively
list.files(data_folder, recursive = TRUE)

# Store full paths of all files
all_files <- list.files(data_folder, recursive = TRUE, full.names = TRUE)

# Filter only model output files (case-insensitive)
model_files <- grep("model_output", all_files, value = TRUE, ignore.case = TRUE)

# Check if the folder exists and print file size
dir.exists(data_folder)
file.info(zip_file)$size

# Stop if no model output files are found
if (length(model_files) == 0) {
  stop("No model output files found in the dataset.")
}

# Initialize list to hold imported data
model_data <- list()

# Read each model output file and store it in the list
for (file in model_files) {
  
  data <- read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  model_data[[basename(file)]] <- data
  
  cat("Imported:", basename(file))
}

# Print the total number of imported model files
cat("Total length of model output files imported:", length(model_data))
