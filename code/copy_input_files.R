# use this to update the date from the fileshare to the run location
# library(tidyverse)
# library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
# library(mapview)

copy_the_files <- function(infolder, dest, list_only = TRUE, file_type = "*.xlsx" ) {
  
  dataFiles <- dir(infolder, file_type, ignore.case = TRUE, all.files = TRUE)  

  
  print(paste(cat("\n\n\nCopying files from\n", infolder, '\n\nto\n', dest,'\n', sep = '  ')))
  print(paste(cat("The files to be copied are:\n", dataFiles, sep = '\n')))
  
  if (list_only){
    print(writeLines('\n\n=============\nnot transferring files\n=============\n\n'))
  }
  else {
  # To run without error:
    print(writeLines('\n\n---------------\nTransferring files\n-------------\n\n'))
  file.copy(file.path(infolder, dataFiles), dest, overwrite = TRUE)
  }
  }


list_only <-FALSE

# copy modified files
data_path <- Sys.getenv("DATA_PATH")
infolder <-"C:/Users/arich/West Yost Associates/SRP GSA Administrative Services - GUIDE & Fee Updates/sonoma_co_gsas_rate_fee/general/modified_values"
dest <- path(data_path,'general','modified_values')
copy_the_files(infolder, dest, list_only  = list_only)


# copy Public_Water_Connection_Modified file
data_path <- Sys.getenv("DATA_PATH")
infolder <-"C:/Users/arich/West Yost Associates/SRP GSA Administrative Services - GUIDE & Fee Updates/sonoma_co_gsas_rate_fee/general"
dest <- path(data_path,'general')
copy_the_files(infolder, dest, list_only  = list_only)

# copy recycled water -SRP- file
data_path <- Sys.getenv("DATA_PATH")
infolder <-"C:/Users/arich/West Yost Associates/SRP GSA Administrative Services - GUIDE & Fee Updates/sonoma_co_gsas_rate_fee/srp/recycled_water"
dest <- path(data_path,'srp', 'recycled_water')
copy_the_files(infolder, dest, list_only  = list_only, file_type = "*.csv")

# copy recycled water -SRP- file
data_path <- Sys.getenv("DATA_PATH")
infolder <-"C:/Users/arich/West Yost Associates/SRP GSA Administrative Services - GUIDE & Fee Updates/sonoma_co_gsas_rate_fee/son/recycled_water"
dest <- path(data_path,'son', 'recycled_water')
copy_the_files(infolder, dest, list_only  = list_only, file_type = "*.csv")