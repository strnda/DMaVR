# Load necessary library
library(data.table)  

# Set working directory to the folder containing model output files
setwd("/Users/wangshuhuai/model_output_daymet")  

# Step 1: Find all "_model_output.txt" files in subdirectories
files <- list.files(pattern = "_model_output\\.txt$", full.names = TRUE, recursive = TRUE)
cat("Total files found:", length(files), "\n")

# Step 2: Initialize an empty data table to store merged data
final_data <- data.table()

# Step 3: Define batch size to process files in smaller groups
batch_size <- 500
num_batches <- ceiling(length(files) / batch_size)  # Total number of batches

# Step 4: Process files in batches to save memory
for (i in seq_len(num_batches)) {
  cat("Processing batch", i, "of", num_batches, "...\n")
  
  # Select files for the current batch
  batch_files <- files[((i - 1) * batch_size + 1):min(i * batch_size, length(files))]
  
  # Read files in the batch, skipping empty ones
  batch_data <- rbindlist(lapply(batch_files, function(f) {
    if (file.size(f) > 0) return(fread(f)) else return(NULL)
  }), fill = TRUE)
  
  # Append batch data to the final dataset
  final_data <- rbind(final_data, batch_data, fill = TRUE)
}

# Step 5: Check final dataset size and preview the first few rows
cat("Final dataset size:", dim(final_data), "\n")
head(final_data)

# Step 6: Save the dataset as CSV
fwrite(final_data, "filtered_model_output.csv")

# Step 7 (Optional): Compress the CSV file
fwrite(final_data, "filtered_model_output.csv.gz", compress = "gzip")
