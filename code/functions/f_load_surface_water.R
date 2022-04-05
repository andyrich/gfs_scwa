# load GSA boundaries
f_load_surface_water <- function(data_path){
  print('loading surface water data')
  ## surface water connection -----------------------------------------------
  # read ewrims data, filter to SON, transform, select relevant cols
  ewrims <- path(data_path, "general/ewrims", 'water_rights_list_2021-09-23.csv') %>%
    read_csv(col_select = c("longitude", "latitude",
                            "face_value_amount", "county"),
             col_types = list(
               longitude         = "d",
               latitude          = "d",
               face_value_amount = "d",
               county            = "c")) %>%
    rename(Surface_Water_Use_Ac_Ft = face_value_amount) %>%
    filter(county == "Sonoma" | is.na(county)) %>%
    # remove a few rows without location data
    filter(!is.na(latitude), !is.na(longitude),
           !is.nan(latitude), !is.nan(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>%
    st_transform(epsg) %>%
    select(-county)
  print('Done loading surface water data')
  
  return(ewrims)
}



