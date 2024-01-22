fill_parcel_info <- function(parcel, basin_name) {
  parcel <- parcel%>% 
  mutate(
    # Add parcel size
    LandSizeParcelAcres = LandSizeAcres,
    # Is the parcel a boundary parcel
    Basin_Boundary_Parcel = edge,
    # Basin_Boundary_Parcel = ifelse(APN %in% boundary_parcels, "Yes", "No"),
    # area of the total APN across both GSAs is the recorded APN area
    Intersect_GSA_Bndry_Sum_Acres = ifelse(edge =='Yes',
                                           LandSizeAcres, NA),
    # adjust area of the bisected parcels in the GSA. remember, we clipped 
    # to the B118 basin polygon, so the area is just the calculated area!
    LandSizeAcres = ifelse(
      edge =='Yes',
      as.numeric(units::set_units(st_area(geometry), acres)), 
      LandSizeAcres
    ),
    # # proportion of the APN in this GSA, used to assign a GSA
    # area_prop_apn = LandSizeAcres / Intersect_GSA_Bndry_Sum_Acres,
    # GSA_Jurisdiction_Prelim = !!parse_quosure(basin_name),
    # intentionally left blank for clients to evaluate and populate
    # GSA_Jurisdiction_Modified = NA,
    # GSA_Jurisdiction_Mod_Value = NA,
    # GSA_Jurisdiction = NA
  ) 
  
  parcel$GSA_Jurisdiction_Prelim = basin_name
  
  return(parcel)}


parcel_contact <-function(parcel){
  
  parcel <- parcel %>% 
    mutate(
      # parcel and contact info
      LandSizeAcres       = LndSzAcre, # this value gets changed below
      LandSizeParcelAcres = LandSizeAcres,
      UseCode_Description = UseCDesc,
      UseCode_Category    = UseCType,
      CurrentOwnerName    = NA,
      Situs_Address       = SitusFormatted1)
  
  parcel <- parcel %>%
    mutate(Jurisdiction = ifelse(CityType == 'Incorporated',
                                 POCity, 'Unincorporated Sonoma County'))
  
  
  return(parcel)
}

load_land_use <- function(parcel){
# crop data - intersect to son. For reference, crop classes:
### C = Citrus and subtropical
### D = Deciduous Fruits and Nuts
### G = Grain
### P = Pasture
### T = Truck Nursery and Berry Crops
### V = Vineyard
### X = other
print('adding crop data to crop table')
crop <- path(data_path, 
"general/crops/i15_crop_mapping_2020/i15_crop_mapping_2020_edit/i15_Crop_Mapping_2020_SON_Edit.shp" 
             ) %>% 
  st_read() %>% 
  filter(COUNTY == "Sonoma") %>% 
  st_transform(epsg) %>% 
  st_make_valid() %>% 
  st_intersection(parcel) %>% 
  select(crop_class = CLASS2) %>% 
  mutate(crop_class = case_when(
    crop_class == "C" ~ "Citrus_and_Subtropical",
    crop_class == "D" ~ "Deciduous_Fruit_and_Nuts",
    crop_class == "T" ~ "Truck_and_Berry_Crops",
    crop_class == "V" ~ "Vine",
    crop_class == "G" ~ "Grain",
    crop_class == "P" ~ "Pasture",
    TRUE ~ "X"
  ))

return(crop)
}


calc_crop_water_use <-function (parcel, crop){
  print('calculating crop water usage')
# get crops per APN and recalculate area, and as before, because there many
# APN with > 1 crop, we need to summarize the data before joining!
crops_per_apn <- st_intersection(select(parcel, APN), crop) %>% 
  mutate(crop_acres = 
           as.numeric(units::set_units(st_area(geometry), "acres"))) %>% 
  # very important! Duplicates are present, and de-duplication is needed
  distinct() %>% 
  st_drop_geometry() %>% 
  # also incredibly important: there are multiple crop polygons per APN 
  # that need to be summed!
  group_by(APN, crop_class) %>% 
  summarise(crop_acres = sum(crop_acres, na.rm = TRUE)) %>% 
  ungroup()

# applied water (in acre feet per acre) per crop, for references, see Rob
# Pennington's Sheet path(data_path, "general/crop_water_use/Crop Type Vs Water Use v3.xlsx")
# and email 2022-01-10 Re: Ongoing technical question log
crop_applied_water <- tibble(
  crop_class = c("Citrus_and_Subtropical", "Deciduous_Fruit_and_Nuts", 
                 "Truck_and_Berry_Crops", "Vine", "Grain", "Pasture", "X"),
  applied_af_acre = c(1.85, 1.83, 1.78, 0.6, 0, 0.04, 0))

# add applied water and calculate acre feet used per parcel, but because there
# are multiple crops per field, we need to make sure the output dataframe has
# only one row per APN, and is in wide format ready for the join to `pson`
crops_per_apn_key <- left_join(crops_per_apn, crop_applied_water) %>% 
  mutate(applied_af = crop_acres * applied_af_acre) %>% 
  select(-applied_af_acre) %>% 
  pivot_wider(names_from = crop_class, values_from = c(crop_acres, applied_af))

# fix names
cns <- names(crops_per_apn_key)
cns[str_which(cns, "crop_acres")] <- paste0(
  str_replace_all(
    cns[str_which(cns, "crop_acres")], "crop_acres_", ""), 
  "_Area_Ac")
cns[str_which(cns, "applied_af")] <- paste0(
  str_replace_all(
    cns[str_which(cns, "applied_af")], "applied_af_", ""), 
  "_Rate")
names(crops_per_apn_key) <- cns

# NA values for crop water use go to zero
crops_per_apn_key[is.na(crops_per_apn_key)] <- 0

# join crops to parcels in the specified format and calculate consumptive rate
parcel <- parcel %>% 
  left_join(crops_per_apn_key) %>% 
  mutate(
    # crop area per parcel
    Cannabis_Outdoor_Area_Ac         = 0, # no cannabis data
    Cannabis_Indoor_Area_Ac          = 0, # no cannabis data
    
    # crop consumptive use (AF/year)
    Cannabis_Outdoor_Rate            = 0, 
    Cannabis_Indoor_Rate             = 0, 
    
    # summation columns
    Total_Crop_Area_Prelim_Ac = rowSums(across(ends_with("Area_Ac")), 
                                        na.rm = TRUE),
    Total_Crop_Area_Ac        = Total_Crop_Area_Prelim_Ac, 
    Water_Use_Ag_Rate_Ac_Ft   = rowSums(across(ends_with("_Rate")), 
                                        na.rm = TRUE)
  ) %>% 
  # replace NA areas and rates with 0
  mutate(across(ends_with("_Area_Ac"), ~ifelse(is.na(.x), 0, .x)),
         across(ends_with("_Rate"), ~ifelse(is.na(.x), 0, .x)))

return(parcel)
}

calculate_lawn <- function(parcel, basin){# schools and golf courses ------------------------------------------------
  
  # Reference ET0 in feet, via CIMIS ET0 zone 8: 
  # https://cimis.water.ca.gov/App_Themes/images/etozonemap.jpg
  # et <- 4.1 
  
  # Calculate applied water at schools from ET assuming 
  # application efficiency = 0.65 (65%) from Sandoval, 2010. 
  # http://watermanagement.ucdavis.edu/research/application-efficiency/
  # aw <- et / (1 - 0.65) # feet
  aw <- 3.5 # feet/yr from Andy Rich
  
  # school locations
  # filter(psrp, str_detect(UseCode_Description, "School|Golf")) %>% mapview()
  
  lawn <- path(data_path, "general/crops/i15_LandUse_Sonoma2012_SHP/i15_LandUse_Sonoma2012.shp") %>% 
    st_read() %>% 
    filter(CLASS1 == "UL") %>% #filter to only Urban Landscape
    filter(SUBCLASS1 != "5") %>%  # urban landscape except class 5, which is non-irrigated
    filter(WATERSOURC != "1") %>%  # watersource 1 is surface water
    filter(WATERSOURC != "5") %>%  # watersource 5 is reclaimed
    filter(WATERSOURC != "6") %>%  # watersource 6 is recycled
    st_transform(epsg) %>% 
    st_make_valid() %>% 
    st_intersection(basin) %>% 
    select(crop_class = CLASS1)
  
  # get lawn area per APN and recalculate area, and as before, because there many
  # APN with > 1 crop, we need to summarize the data before joining!
  lawn_per_apn <- st_intersection(select(parcel, APN), lawn) %>% 
    mutate(crop_acres = 
             as.numeric(units::set_units(st_area(geometry), "acres"))) %>% 
    # very important! Duplicates are present, and de-duplication is needed
    distinct() %>% 
    st_drop_geometry() %>% 
    # also incredibly important: there are multiple crop polygons per APN 
    # that need to be summed!
    group_by(APN, crop_class) %>% 
    summarise(lawn_acres = sum(crop_acres, na.rm = TRUE)) %>% 
    ungroup()  %>%
    select(-crop_class)
  
  parcel<- left_join(parcel, lawn_per_apn) %>%
    mutate(lawn_acres = ifelse(is.na(lawn_acres), 0, lawn_acres))
  
  #if no public connection, remove lawns <0.2 acres
  parcel<- parcel %>%
    mutate(lawn_acres = ifelse(Public_Water_Connection == 'No' & lawn_acres<0.2,
                               0, lawn_acres))
  
  #if there is public connection, remove lawns <0.5 acres
  parcel<- parcel %>%
    mutate(lawn_acres = ifelse(Public_Water_Connection == 'Yes' & lawn_acres<0.5,
                               0, lawn_acres))
  
  # School_Golf_GW_Use_prelim_Ac_Ft
  parcel <- parcel %>%
    mutate(School_Golf_GW_Use_Prelim_Ac_Ft =
      aw*lawn_acres)   %>%
      select(-lawn_acres)

  
  return(parcel)
  
}

add_wells <- function(parcel){
  ## wells ------------------------------------------------------------------
  # Sonoma county wells - deduplicate
  scwells <- path(data_path, "general",
                  "soco_wells/all_soco_wells_spatial.shp") %>%
    st_read() %>%
    st_transform(epsg) %>%
    st_intersection(select(parcel, APN)) %>%
    add_count(APN, name = "Well_Count") %>%
    group_by(APN) %>%
    mutate(Well_Log_Nos = paste(Log_No, collapse = "; ")) %>%
    slice(1) %>%
    ungroup() %>%
    select(APN, Well_Count, Well_Log_Nos) %>%
    st_drop_geometry()
 return(scwells) 
}
  
calc_wells <-function(parcel, wells){
  # populate database columns
  parcel <- parcel %>%
    left_join(wells) %>%
    mutate(
      # no well count means 0 onsite wells
      Well_Count = ifelse(is.na(Well_Count), 0, Well_Count),
      # Well_Count = 0, # wellcount=0 for all SRP wells as of 6/6/2023, so leaving here as default
      Active_Well = ifelse(Well_Count > 0, "Yes", "No"),
      # Shared_Well = NA, # placeholder for future review
      # Shared_Well_APN = NA, # placeholder for future review
      Well_Records_Available = ifelse(Well_Count > 0, "Yes", "No"),
      Onsite_Well =
        ifelse(Active_Well == "Yes" | Well_Records_Available == "Yes",
               "Yes", "No")
    )
  
  return(parcel)
}

school_golf_calc <-function(parcel){
  ####
  # Code to use to incorporate surface water/recycled water uses.
  # calculate groundwater School_Golf_GW_Use_Prelim_Ac_Ft water use
  parcel <- parcel %>%
    mutate(
      # first ensure that NA values in water budget components go to 0
      School_Golf_GW_Use_Prelim_Ac_Ft  = ifelse(
        is.na(School_Golf_GW_Use_Prelim_Ac_Ft), 0, School_Golf_GW_Use_Prelim_Ac_Ft),
      Surface_Water_Use_Ac_Ft  = ifelse(
        is.na(Surface_Water_Use_Ac_Ft), 0, Surface_Water_Use_Ac_Ft),
      Recycled_Water_Use_Ac_Ft = ifelse(
        is.na(Recycled_Water_Use_Ac_Ft), 0, Recycled_Water_Use_Ac_Ft),
      #calculate School_Golf_Surface_Recycled_Use_Ac_Ft
      School_Golf_Surface_Recycled_Use_Ac_Ft = Surface_Water_Use_Ac_Ft + Recycled_Water_Use_Ac_Ft,
      #calculate school_golf_gw_demand in order to limit surface/recycled water actual use
      school_golf_gw_demand = School_Golf_GW_Use_Prelim_Ac_Ft,
      # Ag School_Golf_GW_Use_Prelim_Ac_Ft use is the following mass balance:
      School_Golf_GW_Use_Prelim_Ac_Ft =
        School_Golf_GW_Use_Prelim_Ac_Ft -
        (Surface_Water_Use_Ac_Ft + Recycled_Water_Use_Ac_Ft),
    )
  
  
  # if a parcel receives more water from surface and recycled sources
  # than estimated demand, the calculated groundwater use is negative, so
  # we coerce this to zero
  parcel <- parcel %>%
    mutate(School_Golf_GW_Use_Prelim_Ac_Ft = ifelse(
      School_Golf_GW_Use_Prelim_Ac_Ft < 0, 0, School_Golf_GW_Use_Prelim_Ac_Ft))
  print('modifying School_Golf_Surface_Recycled_Use_Ac_Ft')
  # if a parcel receives more water from surface and recycled sources
  # than estimated demand, then set the
  # total School_Golf_Surface_Recycled_Use_Ac_Ft = School_Golf_GW_Use_Prelim_Ac_Ft
  parcel <- parcel %>%
    mutate(School_Golf_Surface_Recycled_Use_Ac_Ft = ifelse(
      School_Golf_Surface_Recycled_Use_Ac_Ft > school_golf_gw_demand,
      school_golf_gw_demand, School_Golf_Surface_Recycled_Use_Ac_Ft)) %>%
    select(-school_golf_gw_demand)
  
  
  # blank fields to permit revision of the data
  parcel <- parcel %>% 
    mutate(
      School_Golf_GW_Use_Ac_Ft = ifelse(School_Golf_Modified == "Yes", 
                                        School_Golf_GW_Use_Modified_Ac_Ft, 
                                        School_Golf_GW_Use_Prelim_Ac_Ft)) 
  
  return(parcel)
  
}

calc_ag_use <-function(parcel){
  parcel <- parcel %>% 
  mutate(
    # first ensure that NA values in water budget components go to 0
    Water_Use_Ag_Rate_Ac_Ft  = ifelse(
      is.na(Water_Use_Ag_Rate_Ac_Ft), 0, Water_Use_Ag_Rate_Ac_Ft),
    Surface_Water_Use_Ac_Ft  = ifelse(
      is.na(Surface_Water_Use_Ac_Ft), 0, Surface_Water_Use_Ac_Ft),
    Recycled_Water_Use_Ac_Ft = ifelse(
      is.na(Recycled_Water_Use_Ac_Ft), 0, Recycled_Water_Use_Ac_Ft),
    #calculate Ag_Surface_Recycled_Actual_Use_Ac_Ft
    Ag_Surface_Recycled_Actual_Use_Ac_Ft = Surface_Water_Use_Ac_Ft + Recycled_Water_Use_Ac_Ft,
    # Ag GW use is the following mass balance:
    Ag_GW_Use_GIS_Ac_Ft = 
      Water_Use_Ag_Rate_Ac_Ft - 
      (Surface_Water_Use_Ac_Ft + Recycled_Water_Use_Ac_Ft))

# if a parcel receives more water from surface and recycled sources 
# than estimated demand, the calculated groundwater use is negative, so
# we coerce this to zero
  parcel <- parcel %>% 
  mutate(Ag_GW_Use_GIS_Ac_Ft = ifelse(
    Ag_GW_Use_GIS_Ac_Ft < 0, 0, Ag_GW_Use_GIS_Ac_Ft))

# if a parcel receives more water from surface and recycled sources
# than estimated demand, then set the
# total Ag_Surface_Recycled_Actual_Use_Ac_Ft = Ag_GW_Use_GIS_Ac_Ft
  parcel <- parcel %>%
  mutate(Ag_Surface_Recycled_Actual_Use_Ac_Ft = ifelse(
    Ag_Surface_Recycled_Actual_Use_Ac_Ft > Water_Use_Ag_Rate_Ac_Ft,
    Water_Use_Ag_Rate_Ac_Ft, Ag_Surface_Recycled_Actual_Use_Ac_Ft))

# No Idle Acres to start - this is reported
  parcel <- parcel %>% mutate(Idle_Ac = 0)



# blank fields to permit revision of the data
  parcel <- parcel %>% 
  mutate(
    Ag_GW_Use_Ac_Ft = ifelse(Ag_GW_Use_Modified == "Yes", 
                             Ag_GW_Use_Modified_Ac_Ft, 
                             Ag_GW_Use_GIS_Ac_Ft)) 
  return(parcel)
}

gis_survey_determination <- function(parcel){
  
  parcel <- parcel %>% 
    mutate(
      Residential_Water_Use_Determination = ifelse(
        Res_W_Use_Assessor_Ac_Ft > 0 | 
          Res_GW_Use_Prelim_Ac_Ft > 0 | 
          Res_GW_Use_Modified_Ac_Ft > 0, "Yes", "No"
      ),
      Commercial_Water_Use_Determination = ifelse(
        Commercial_W_Use_Assessor_Ac_Ft > 0 | 
          Commercial_GW_Use_Prelim_Ac_Ft > 0 | 
          Commercial_GW_Use_Modified_Ac_Ft > 0, "Yes", "No"
      ),
      Urban_Landscape_Irrigation_Water_Use_Determination = ifelse(
        Urban_Irrigation_GW_Use_Ac_Ft > 0, "Yes", "No"
      ),
      Ag_Irrigation_Water_Use_Determination = ifelse(
        Water_Use_Ag_Rate_Ac_Ft  > 0, "Yes", "No"
      ),
      Recycled_Water_Use_Determination = ifelse(
        Recycled_Water_Use_Ac_Ft > 0, "Yes", "No"
      ),
      Surface_Water_Use_Determination = ifelse(
        Surface_Water_Use_Ac_Ft > 0, "Yes", "No"
      ),
      School_GolfCourse_Water_Use_Determination = ifelse(
        School_Golf_GW_Use_Ac_Ft > 0, "Yes", "No"
      )
    ) %>% 
    # make all NAs in these columns go to zero
    mutate(across(ends_with("Determination"), ~ifelse(is.na(.x), "No", .x)))
  
  return(parcel)
}

total_use_calc <- function(parcel, gw_use_rate){
  
  # ensure NA values go to 0 so the result is calculable
  parcel <- parcel %>% 
    mutate(across(ends_with("GW_Use_Ac_Ft"), ~ifelse(is.na(.x), 0, .x))) 
  
  # calculate total groundwater use
  parcel <- parcel %>% 
    mutate(
      Total_Groundwater_Use_Ac_Ft =
        Res_GW_Use_Ac_Ft + 
        Commercial_GW_Use_Ac_Ft + 
        Ag_GW_Use_Ac_Ft + 
        School_Golf_GW_Use_Ac_Ft + 
        Urban_Irrigation_GW_Use_Ac_Ft,
      Total_Groundwater_Use_PublicView = NA,
    )
  
  # set to zero parcels with less than 0.1 AF
  parcel <- mutate(parcel,Total_Groundwater_Use_Ac_Ft = 
                   ifelse(Total_Groundwater_Use_Ac_Ft<0.1, 0,
                          Total_Groundwater_Use_Ac_Ft),
                 Parcel_fee = Total_Groundwater_Use_Ac_Ft*unsub_gw_sub_rate,
                 Fee_Rate = gw_use_rate,
                 Parcel_Fee_Subsidized = Total_Groundwater_Use_Ac_Ft*gw_use_rate
  
      
                 )
  return(parcel)
  
}