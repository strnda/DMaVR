library(data.table)

#seet file paths
zip_file <- "C:/Users/student_project/Desktop/basin_timeseries.zip"
output_dir <- "C:/Users/student_project/Desktop/model_output"
output_csv <- "combined_model_output.csv"

#create folder if not exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#unzip if needed
unzipped_files <- list.files(output_dir, pattern = "model_output", recursive = TRUE)
if (length(unzipped_files) == 0) {
  unzip(zip_file, exdir = output_dir)
}

#get all model_output files
model_files <- list.files(output_dir, pattern = "model_output", full.names = TRUE, recursive = TRUE)

if (length(model_files) == 0) {
  stop("No model_output files found.")
}

#loop through files in chunks
batch <- 100
first_time <- TRUE

for (start in seq(1, length(model_files), by = batch)) {
  end <- min(start + batch - 1, length(model_files))
  current_files <- model_files[start:end]

  data_list <- lapply(current_files, function(f) {
	dt <- fread(f)
	dt[, ID := strsplit(basename(f), "_")[[1]][1]]
	dt <- na.omit(dt)
	return(dt)
  })

  combined <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

  # Write to csv
  fwrite(combined, output_csv, append = !first_time, col.names = first_time)

  first_time <- FALSE
  rm(data_list, combined)
  gc()
}

cat("Done! File saved as:", output_csv)
