# function to track progress towards complete database
f_progress <- function(){
  aoi_obj = get(aoi)
  percent = round(sum(colnames(aoi_obj) %in% add)/length(add)*100)
  cat(percent, "% complete.\n")
}

# function to ensure no duplicates are returned during spatial joins and joins
f_verify_non_duplicates <- function(){
  aoi_obj = get(aoi)
  print(nrow(aoi_obj))
  print(nrow(distinct(aoi_obj)))
  print(length(unique(aoi_obj$APN)))
}
