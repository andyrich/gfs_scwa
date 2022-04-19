# load GSA boundaries
load_urban_wells <- function(data_path,parc){
  print('loading surface water data')

  urban_wells_list <- path(data_path, "general/urban_wells", 
                   'urban_wells_ALL_2022_04_18.xlsx')
  print('Loading pre-processed list of urban wells')
  urb <- urban_wells_list %>%
    readxl::read_xlsx(sheet = 'Urban_Wells_ALL_2022_04_18') %>%
    select(APN)
  
  urb$Urban_list <- 'Yes'
  
  print('Done loading urban wells')
  
  parc<- left_join(parc, urb) %>%
    mutate(Urban_Well = ifelse(is.na(Urban_list), 'No', 'Yes')) %>%
    select(-Urban_list)
  
  #count number of occurrences of each value (including NA values) in column
  print('these are the total value counts in urban_wells for the basin')
  print(as.data.frame(table(parc$Urban_Well, useNA = 'always')))
  
  return(parc)
}



