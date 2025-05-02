# Increase timeout in case of slow download
options(timeout = 1800)

# Define variables
url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
local_zip <- "timeseries_data.zip"
extract_path <- "timeseries_unzipped"


file.remove(local_zip)


download.file(url, destfile = local_zip, mode = "wb")

# Confirm the file exists
if (!file.exists(local_zip)) {
  download.file(url, destfile = local_zip, mode = "wb")
  cat("Download completed.\n")
} else {
  cat("ZIP file already downloaded.\n")
}

# Unzip the file if not already done
if (!dir.exists(extract_path)) {
  unzip(local_zip, exdir = extract_path)
  cat("Data unzipped.\n")
} else {
  cat("Data already unzipped.\n")
}

contents <- list.files(extract_path, recursive = TRUE)

all_paths <- list.files(extract_path, recursive = TRUE, full.names = TRUE)

# Filter model output files
model_outputs <- grep("model_output", all_paths, value = TRUE, ignore.case = TRUE)

dir.exists(extract_path)
file.info(local_zip)$size

if (length(model_outputs) == 0) {
  stop("No model output files found.")
}

# Read each model file into a list
output_list <- list()

for (path in model_outputs) {
  df <- read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  output_list[[basename(path)]] <- df
  cat("Loaded:", basename(path), "\n")
}

cat("Total number of model files loaded:", length(output_list), "\n")
