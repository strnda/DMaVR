# -------------------------------
# Assignment: Importing Model Output Data with Base R
# Author: Hesam Owlanj (xowlh001)
# Date: April 2025
# -------------------------------

# --- Step 1: Setup ---
# Define the URL and local file path
url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
local_zip <- "model_output_daymet.zip"
extract_dir <- "model_output_daymet"

# --- Step 2: Download the dataset ---
# Download only if the file doesn't exist
if (!file.exists(local_zip)) {
  download.file(url, destfile = local_zip, mode = "wb")
  message("Download complete.")
} else {
  message("Zip file already downloaded.")
}

# --- Step 3: Unzip the file ---
# Create directory if it doesn't exist
if (!dir.exists(extract_dir)) {
  dir.create(extract_dir)
  unzip(local_zip, exdir = extract_dir)
  message("Unzipping complete.")
} else {
  message("Files already unzipped.")
}

# --- Step 4: List & filter model output files ---
# List all files in the directory
all_files <- list.files(extract_dir, recursive = TRUE, full.names = TRUE)

# Model output files typically contain "model_output" or end in .txt format
model_files <- all_files[grepl("model_output", all_files) | grepl("\\.txt$", all_files)]

# For large directories, it's good to confirm what we've selected
cat("Number of model output files found:", length(model_files), "\n")

# --- Step 5: Import model output data ---
# Create an empty list to store the imported data
model_data_list <- list()

# Read in each file using base R's read.table (memory-efficient)
for (i in seq_along(model_files)) {
  file <- model_files[i]
  
  # Use tryCatch to handle potential read errors
  model_data_list[[basename(file)]] <- tryCatch({
    read.table(file, header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    message("Error reading file: ", file)
    NULL
  })
  
  # Optional: print progress
  if (i %% 50 == 0) cat("Imported", i, "files\n")
}

# --- Step 6: Summary ---
# Remove NULLs (in case of errors)
model_data_list <- Filter(Negate(is.null), model_data_list)

cat("Successfully imported", length(model_data_list), "model output files.\n")


