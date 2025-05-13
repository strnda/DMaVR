setwd("C:/Users/azato/PED")

# Step 1: Setup
url <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"
zip_file <- "camels_model_output_daymet.zip"
extract_dir <- "camels_extracted"
output_file <- "camels_model_output_cleaned.rds"
expected_size <- 4207763546  # Approximate size in bytes

# Step 2: Download ZIP file with validation and retry logic
download_success <- FALSE
max_tries <- 5

for (i in 1:max_tries) {
  cat(sprintf("📥 Attempt %d to download the file...\n", i))
  
  try({
    download.file(url, destfile = zip_file, mode = "wb", quiet = TRUE)
    size <- file.info(zip_file)$size
    if (!is.na(size) && size >= expected_size) {
      cat("✅ Download complete and file size validated.\n")
      download_success <- TRUE
      break
    } else {
      cat("⚠️ File appears incomplete (partial download). Retrying...\n")
    }
  }, silent = TRUE)
}

if (!download_success) {
  stop("❌ Failed to download complete ZIP file after several attempts.")
}

# Step 3: Unzip the archive
if (!dir.exists(extract_dir)) dir.create(extract_dir)
unzip(zip_file, exdir = extract_dir)
cat("📂 Files successfully extracted.\n")

# Step 4: List and filter only model output files
all_files <- list.files(
  path = extract_dir,
  pattern = "model_output.*\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(all_files) == 0) {
  stop("❌ No model_output files found after extraction.")
}
cat(sprintf("🔍 %d model_output files detected.\n", length(all_files)))

# Step 5: Read and combine files efficiently (base R only)
data_list <- vector("list", length(all_files))

for (i in seq_along(all_files)) {
  file_path <- all_files[i]
  cat(sprintf("📄 Reading %d of %d: %s\n", i, length(all_files), basename(file_path)))
  
  df <- read.table(
    file_path,
    header = TRUE,
    na.strings = c("NA", "-999"),
    stringsAsFactors = FALSE
  )
  
  df$id <- tools::file_path_sans_ext(basename(file_path))  # Assign ID from filename
  data_list[[i]] <- df
}

# Step 6: Combine all data and remove NAs
combined_data <- do.call(rbind, data_list)
combined_data <- na.omit(combined_data)
combined_data$id <- as.factor(combined_data$id)

# Step 7: Save cleaned dataset to RDS for fast future use
saveRDS(combined_data, file = output_file)
cat("💾 Cleaned data saved as:", output_file, "\n")

# Step 8: Preview first few rows
cat("🧾 First rows of the dataset:\n")
print(head(combined_data))





