# function to track progress towards complete database
f_progress <- function(){
  aoi_obj = get(aoi)
  percent = round(sum(colnames(aoi_obj) %in% add)/length(add)*100)
  cat(percent, "% complete.\n")
  missing_fields<- fields[!(fields  %in% colnames(aoi_obj))]
  
  if (percent<100 & length(missing_fields)<10) {
    print(cat('These are the missing columnames:\n',
              missing_fields, '\n'))
  
  }
  else {
    print(cat('These are ', length(missing_fields), ' missing columnames (not listing all)\n'))
  }

}

# function to ensure no duplicates are returned during spatial joins and joins
f_verify_non_duplicates <- function(){
  aoi_obj = get(aoi)
  print(nrow(aoi_obj))
  print(nrow(distinct(aoi_obj)))
  print(length(unique(aoi_obj$APN)))
}

get_schema_fields <- function(data_path){
  fields <- get_schema_path(data_path) %>%
  readxl::read_xlsx(sheet = 1, range = cellranger::cell_cols("B")) %>%
  set_names("name") %>%
  filter(!is.na(name)) %>%
  pull(name)

  return(fields)
}

get_schema <- function(data_path){
s_ <-get_schema_path(data_path)
print("loading schema")
print(s_)
schema <- s_ %>%
  readxl::read_xlsx(sheet = 'Sheet1')

return(schema)
}

get_schema_path <-function(data_path){
  s_ <- path(data_path, "schema",
             '2023_05_31 GSA Schema from RP.xlsx')
  return(s_)
}


check_use_codes <-function(parcel, check_missing){
  
  
  if (missing(check_missing)) {
    print(paste('The number of nulls in parcel UseCode:', 
                sum(is.na(parcel$UseCode)) , sep = " "))
    return(sum(is.na(parcel$UseCode)))
    
  }
  else {
  print(paste('The number of nulls in parcel UseCode:',
  sum(is.na(parcel$UseCode)), 'it was previously', 
  check_missing, sep = " "))
  if (sum(is.na(parcel$UseCode))==check_missing){
  return(check_missing)
  }
  else {
  stop(paste("the number of missing UseCodes has changed. It was ",
             check_missing,
             'and is now',
             sum(is.na(parcel$UseCode)), sep = ' ' ))
  }

  }
  
}