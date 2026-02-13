#setwd("C:/Users/simca/Documents/czu/R/assignments/1")

#save users current directory
current_dir <- getwd()

download the file
download.file("https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip",
             destfile = paste0(current_dir, "/basin_timeseries_v1p2_modelOutput_daymet.zip"))

unzip the downloaded folder and check if the file really downloaded
if(file.exists('basin_timeseries_v1p2_modelOutput_daymet.zip')){
  unzip('basin_timeseries_v1p2_modelOutput_daymet.zip')
} else{
  print("File didn't download!")
}

#set working directory to that path
setwd(paste0(current_dir, "/model_output_daymet/model_output/flow_timeseries/daymet/"))
new_dir <- getwd()

#save folders that contains the model output files
folders <- list.files()

all_matching_files <- list()
matching_files <- list()

#loop that goes through the folders
for(i in folders){
  #set working directory in active folder
  setwd((paste0(new_dir, "/", i)))
  
  #save the all the files in active folder
  files <- list.files()
  
  #check for files containing "model_output" in their name
  contains_word <- grepl("model_output", files)
  
  #get matching files
  matching_files <- files[contains_word]
  
  #show results
  #print(matching_files)
  rm(files)
}
#make sure there are no duplicates in the output
match_no_duplicates <- unique(matching_files)

#convert selected files to data frames
all_matching_files <- lapply(match_no_duplicates, read.table)

#combine all files into one data frame
combined_df <- do.call("rbind", all_matching_files)

#remove NA's
combined_df <- na.omit(combined_df)

write.csv(combined_df, "data.csv", row.names = FALSE)

#Summary:
#My approach was to loop through all the folders and search for files that contains "model_output" in their name
#I didn't face much challenges. Only during the downloading of the file a timeout of 60sec was reached.
#So I used options(timeout = 1000) in the console and everything worked great