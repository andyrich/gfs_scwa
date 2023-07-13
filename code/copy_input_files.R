
library(tidyverse)
# library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
# library(mapview)

copy_the_files <- function(infolder, dest, list_only = TRUE ) {
  
  dataFiles <- dir(infolder, "*.xlsx", ignore.case = TRUE, all.files = TRUE)  

  
  print(paste(cat("\n\n\nCopying files from\n", infolder, '\n\nto\n', dest,'\n', sep = '  ')))
  print(paste(cat("The files to be copied are:\n", dataFiles, sep = '\n')))
  
  if (list_only){
    print(writeLines('\n\n=============\nnot transferring files\n=============\n\n'))
  }
  else {
  # To run without error:
  # file.copy(file.path(infolder, dataFiles), dest, overwrite = TRUE)
  }
  }


# copy modified files
data_path <- Sys.getenv("DATA_PATH")
infolder <-"C:/Users/arich/West Yost Associates/SRP GSA Administrative Services - GUIDE & Fee Updates/sonoma_co_gsas_rate_fee/general/modified_values"
dest <- path(data_path,'general','modified_values')
copy_the_files(infolder, dest, list_only  = TRUE)


# copy Public_Water_Connection_Modified file
data_path <- Sys.getenv("DATA_PATH")
infolder <-"C:/Users/arich/West Yost Associates/SRP GSA Administrative Services - GUIDE & Fee Updates/sonoma_co_gsas_rate_fee/general"
dest <- path(data_path,'general')
copy_the_files(infolder, dest, list_only  = TRUE)
