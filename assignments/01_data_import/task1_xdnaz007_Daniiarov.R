#1. Download a zip (from website)
url <- "https://www.hydroshare.org/resource/2cc1b6b57b1d4c9f8d67b6c3f790a6c5/data/contents/camels_us.zip"
zip_file <- "camels_us.zip"
download.file(url, destfile = zip_file, mode = "wb")

#2. Unzip files
unzip(zip_file, exdir = "camels_data")

#3. Find and filter only model output files
model_files <- list.files(
  path = "camels_data", 
  pattern = "model_output.*\\.txt$", 
  recursive = TRUE, 
  full.names = TRUE
)

#4. Download data from file (ex 1 or 2)
loaded_data <- lapply(model_files, function(file) {
  read.table(file, header = TRUE)
})

#5. Get info (as a file)
cat("Imported", length(model_files), "model output files .\n")
cat("Example of a first file :\n")
print(str(loaded_data[[1]]))
