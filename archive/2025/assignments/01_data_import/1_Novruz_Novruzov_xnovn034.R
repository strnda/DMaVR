setwd("C:/Users/Admin/Desktop/CZU University First Year Stuff/Second Semester/R")

options(timeout = 1800)

dataset_url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
zip_file <- "camels_basin_timeseries.zip"
data_folder <- "camels_data"

file.remove(zip_file)
download.file(dataset_url, zip_file, mode = "wb")

if (!file.exists(zip_file)) {
  download.file(dataset_url, zip_file, mode = "wb")
  cat("Download done")
} else {
  cat("ZIP file already exists")
}

if (!dir.exists(data_folder)) {
  unzip(zip_file, exdir = data_folder)
  cat("Unzipped")
} else {
  cat("Unzipped data folder already exists")
}

list.files(data_folder, recursive = TRUE)

all_files <- list.files(data_folder, recursive = TRUE, full.names = TRUE)

model_files <- grep("model_output", all_files, value = TRUE, ignore.case = TRUE)

dir.exists(data_folder)
file.info(zip_file)$size

if (length(model_files) == 0) {
  stop("No model output files found in the dataset.")
}

model_data <- list()

for (file in model_files) {
  
  data <- read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  model_data[[basename(file)]] <- data
  
  cat("Imported:", basename(file))
}

cat("Total length of model output files imported:", length(model_data))
