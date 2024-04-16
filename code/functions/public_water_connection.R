load_public_water_connections <- function(){
  # add public water connections for modified APNs:
  apn_add_pwc <- path(data_path, "general/Public_Water_Connection_Modified.xlsx") %>% 
    readxl::read_xlsx(sheet = 1) 
  
  apn_yes <- apn_add_pwc[ tolower(apn_add_pwc$Value)
                          == 'yes',] %>% pull(APN)
  
  apn_no <- apn_add_pwc[ tolower(apn_add_pwc$Value)
                         == 'no',] %>% pull(APN)
  
  water_comment <- path(data_path, "general/Public_Water_Connection_Modified.xlsx") %>%
    readxl::read_xlsx(sheet = 1) %>%
    select(APN, 
           Water_Source_Comment=Comment)
  

  
  return(list('yes' = apn_yes, 'no' = apn_no, 'water_comments' = water_comment))
}

add_public_water_connection <-function(parcel){
  
  apn <-load_public_water_connections()

  
  parcel <- mutate(parcel,
           Public_Water_Connection = ifelse(Jurisdiction == "Unincorporated Sonoma County" ,
                                             "No", "Yes"),
           pwc = Public_Water_Connection,
           Public_Water_Connection = ifelse(Public_Water_Connection == "Yes" & #for incorporated areas, check if has no connection
                                               APN %in% apn$no, "No", Public_Water_Connection),
           Public_Water_Connection = ifelse(pwc=='No' &
                                               APN %in% apn$yes, "Yes", Public_Water_Connection), #for unincorporated areas, check if has yes connection
           Public_Water_Connection_Modified = ifelse(APN %in% apn$yes | APN %in% apn$no,
                                                     "Yes", "No")) %>%
             select(-pwc)
  
  water_comment<-apn$water_comments
  
  parcel <-left_join(parcel,water_comment) %>%
    mutate(Water_Source_Comment = replace_na(Water_Source_Comment, ''))
  
  return(parcel)
  
}


pwc_use_code_fix <-function(parcel){

pwc_accessor_key <- readxl::read_xlsx(accessor_key_path, 
                                      sheet = 3, range = "B2:C28") %>% 
  janitor::clean_names() %>% 
  mutate(use_code = str_pad(use_code, 4, "left", "0"))

# if the parcel is within a water service area and the use code is listed, 
# mark a public water service connection even if not explicitly listed
parcel <- parcel %>% 
  mutate(Public_Water_Connection = ifelse(
    Public_Water_Connection == "Yes" & 
      UseCode %in% pwc_accessor_key$use_code,
    "Yes", Public_Water_Connection
  )
  )

return(parcel)
}

get_wsa_key <- function(parcel, area){
  ## water service areas ----------------------------------------------------
  
  # water service areas in SON
  wsa <- path(data_path, "general", "water_system_boundaries",
              "SABL_Public_083121/SABL_Public_083121.shp") %>% 
    st_read() %>% 
    st_transform(epsg)  %>%
    st_intersection(area) %>% 
    select(CA_DrinkingWater_SvcArea_Name = WATER_SY_1,
           pwsid = SABL_PWSID) 
  
  # sanity check
  # mapview(pet) + mapview(wsa)
  
  # add water service areas to parcel data, first need to summarize data
  # to avoid duplicates where a parcel falls within more than one water system!
  wsa_key <- st_join(parcel, wsa) %>% 
    select(APN, CA_DrinkingWater_SvcArea_Name) %>% 
    group_by(APN) %>% 
    # for parcels with > 1 water system, combine water system names
    mutate(CA_DrinkingWater_SvcArea_Name = paste(
      CA_DrinkingWater_SvcArea_Name, collapse = "; ")) %>%
    ungroup() %>% 
    distinct() %>% 
    st_drop_geometry() %>% 
    select(APN, CA_DrinkingWater_SvcArea_Name) %>% 
    # coerce character "NA" to NA
    mutate(CA_DrinkingWater_SvcArea_Name = ifelse(
      CA_DrinkingWater_SvcArea_Name == "NA", 
      NA, CA_DrinkingWater_SvcArea_Name)) 
  
  return(wsa_key)
}

calc_urban_irrigation <-function(parcel){

# if there’s an urban well & public water connection, assume 0.1 AF/yr, else 0
parcel <- parcel %>% 
  mutate(
    Urban_Irrigation_GW_Use_Prelim_Ac_Ft = ifelse(
      Urban_Well == "Yes" & Public_Water_Connection == "Yes", 0.1, 0))


return(parcel)
}