# load GSA boundaries
f_load_surface_water <- function(data_path){
  print('loading surface water data')

  jsonpath <- path( data_path,"general/ewrims/water_rights_v3/ewrims_totals.csv")
  print('Loading pre-processed Water Right usages')
  ewrims <- jsonpath %>%
    read_csv() %>%
    rename(Surface_Water_Use_Ac_Ft = GSA_DIVERSION_FINAL)

    
  print('Done loading surface water data')
  
  return(ewrims)
}



