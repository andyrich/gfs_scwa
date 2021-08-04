# load all .Renviron vars and store as clean objects in global env
library(magrittr)

f_load_dot_env <- function(){
  # read contents of .Renviron
  env <- readLines(here::here(".Renviron")) %>% 
    stringr::str_extract_all("[^=]*", simplify = TRUE) %>% 
    .[, 1] %>% 
    stringr::str_trim()
  
  cat("Detected", length(env), 
      "vars (", paste(env, collapse = ", "), ") in", 
      here::here(".Renviron"), "\n")
  
  # load env variables to .GlobalEnv
  genv <- Sys.getenv(env)
  
  if(length(genv) > 1){
    names(genv) <- janitor::make_clean_names(names(genv))
  }
  
  if(length(genv) == 1){
    names(genv) <- janitor::make_clean_names(env)
  }
  
  genv <- lapply(split(genv, names(genv)), unname)
  
  cat("Renamed vars as (", paste(names(genv), collapse = ", "), ").\n")
  
  # if an environmental var is all numeric, make it numeric
  suppressWarnings(
    is_numeric <- purrr::map(genv, ~!is.na(as.numeric(.x)))
  )
  genv <- purrr::map2(genv, is_numeric, ~ifelse(isTRUE(.y), as.numeric(.x), .x))
  
  if(sum(unlist(is_numeric)) >= 1){
    cat("Converted", names(genv[unlist(is_numeric)]), "to numeric.\n")
  }
  
  # load to .GlobalEnv
  suppressMessages(
    list2env(genv, envir = .GlobalEnv)
  )
  
  cat("Loaded", paste(names(genv), collapse = ", "), "to .GlobalEnv\n")
}
