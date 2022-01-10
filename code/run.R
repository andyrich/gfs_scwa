f <- fs::dir_ls(here::here("code"), regexp = "03|04")
keep <- c("f", "i", "st", "data_path", "epsg",
          "f_load_b118_basin", "f_load_dot_env", 
          "f_progress", "f_verify_non_duplicates")

st <- Sys.time()
for(i in seq_along(f)){
  
  cat("RUNNING:", basename(f[i]), "\n\n")
  
  source(f[i])
  
  cat("\n\nCOMPLETED:", basename(f[i]), "\n\n")
  
  # clear all objects and packages, but keep the files to run, counter, and time
  obj = ls(all = TRUE)
  obj = obj[-which(obj %in% keep)]
  pkg = paste('package:', names(sessionInfo()$otherPkgs), sep = "")
  
  rm(list = obj)
  lapply(pkg, detach, character.only = TRUE, unload = TRUE)
  
}
Sys.time() - st
