library(data.table)

# Setting paths
zip_path <- "C:\\Users\\maroi\\Desktop\\CZU\\Code VS\\basin_timeseries_v1p2_modelOutput_daymet.zip"
extraction_dir <- "C:\\Users\\maroi\\Desktop\\CZU\\Code VS\\model_output"
output_file <- "complete_model_output.csv"

# creating extraction directory if needed
dir.create(extraction_dir, showWarnings = FALSE, recursive = TRUE)

#expected pattern
expected_file_pattern <- "model_output"

# unzip only if files do not already exist
if (length(list.files(extraction_dir, pattern = expected_file_pattern, recursive = TRUE, ignore.case = TRUE)) == 0) {
  unzip(zip_path, exdir = extraction_dir)
}

# Get all files in exdir that match pattern
model_files <- list.files(
  extraction_dir,
  pattern = expected_file_pattern,
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(model_files) == 0) {
  stop("No files with 'model_output' found")
}

# Process files in batches
batch_size <- 100
header <- TRUE

for (i in seq(1, length(model_files), batch_size)) {
  batch_files <- model_files[i:min(i + batch_size - 1, length(model_files))]

  # Read and process batch to get  ID from filename and remove NA
  batch_data <- lapply(batch_files, function(file_path) {
    dt <- fread(file_path)
    id_value <- strsplit(basename(file_path), "_")[[1]][1]
    dt[, ID := id_value]
    if (exists("na.omit")) dt <- na.omit(dt)
    dt
  })

  # Combine batch
  combined_batch <- rbindlist(batch_data, use.names = TRUE, fill = TRUE)

  # Write CSV
  fwrite(
    combined_batch,
    output_file,
    append = !header,
    col.names = header
  )

  # Update header and clear memory
  if (header) header <- FALSE
  rm(batch_data, combined_batch)
  gc()
}

message("Processing complete. Output saved to: ", output_file)
