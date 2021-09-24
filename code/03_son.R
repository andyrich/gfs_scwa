# SON database: script to generate the SON database

library(tidyverse)
library(sf)
library(here)
library(fs)


# load data ---------------------------------------------------------------

# preprocessed spatial parcels from Sonoma Co parcels
pson <- read_rds(path(data_path, "data_output/son_parcel.rds"))
ppet <- read_rds(path(data_path, "data_output/pet_parcel.rds"))
psrp <- read_rds(path(data_path, "data_output/srp_parcel.rds"))

# final fields to use
fields <- path(data_path, "srp/parcel/Proposed update to Data Dictionary April 2021.xlsx") %>% 
  readxl::read_xlsx(sheet = 3, range = cellranger::cell_cols("C")) %>% 
  set_names("name") %>% 
  filter(!str_detect(name, " ")) %>% 
  pull(name)

# GSA spatial data
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsas: petaluma, sonoma valley, santa rosa plain
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")


# fields to keep, add, remove ---------------------------------------------

done <- fields[fields %in% colnames(pson)]
add  <- fields[!fields %in% colnames(pson)]
rem  <- colnames(pson)[!colnames(pson) %in% fields]
rem  <- rem[-length(rem)] # keep geometry column


# parcel and contact info -------------------------------------------------

pson <- pson %>% 
  mutate(
    # parcel and contact info
    LandSizeAcres       = LndSzAcre,
    UseCode_Description = UseCDesc,
    UseCode_Category    = UseCType,
    CurrentOwnerName    = NA,
    MailingAddress1     = MailAdr1,
    MailingAddress2     = MailAdr2,
    MailingAddress3     = MailAdr3,
    MailingAddress4     = MailAdr4
  ) %>% 
  select(-all_of(rem))
    

# basin boundary parcels --------------------------------------------------

# Does the parcel overlap SRP, Petaluma, or Sonoma Valley basins?
# we expect SON overlaps only with PET, see map below:
# mapview::mapview(list(son, pet, srp)) + mapview::mapview(p) 

# parcels that intersect multiple basins have duplicate APN across databases
boundary_parcels <- pson$APN[pson$APN %in% ppet$APN]

pson <- pson %>% 
  mutate(
    Basin_Boundary_Parcel = ifelse(APN %in% boundary_parcels,
                                   "Petaluma Valley", NA),
    # area of the total APN that spans GSAs
    Intersect_GSA_Bndry_Sum_Acres = ifelse(APN %in% boundary_parcels,
                                           LandSizeAcres, NA),
    # area of the bisected parcels within the GSA 
    LandSizeAcres = ifelse(APN %in% boundary_parcels,
                       as.numeric(units::set_units(st_area(geometry), acres)), 
                       LandSizeAcres),
    # proportion of the APN in this GSA, used to assign a GSA
    area_prop_apn = LandSizeAcres / Intersect_GSA_Bndry_Sum_Acres,
    GSA_Jurisdiction_Prelim = ifelse(area_prop_apn > 0.5, 
                                     "Sonoma Valley", "Petaluma Valley"),
    # intentionally left blank for clients to evaluate and populate
    GSA_Jurisdiction_Modified = NA,
    GSA_Jurisdiction_Mod_Value = NA,
    GSA_Jurisdiction = NA
  ) %>% 
  # remove intermediate vars
  select(-area_prop_apn)


# water sources -----------------------------------------------------------

# read ewrims data, filter to SON, transform, select relevant cols
ewrims <- dir_ls(path(data_path, "general/ewrims")) %>% 
  read_csv(col_select = c("longitude", "latitude", "face_value_amount", "county"), 
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
  st_transform(3310) %>% 
  select(-county)

# add surface water use (AF/year) to parcels 
pson <- st_intersection(pson, ewrims) %>% 
  mutate(Surface_Water_Connection = ifelse(Surface_Water_Use_Ac_Ft > 0, "Yes", "No"))

# Recycled_Water_Connection
# Recycled_Water_Use_Ac-Ft 
Surface_Water_Connection
Surface_Water_Use_Ac-Ft
# Active_Well 
# Shared_Well 
# Shared_Well_APN 
# Well_Records_Available 
# Onsite_Well 
# Urban_well 
# CA_DrinkingWater_SvcArea_Name 
# CA_DrinkingWater_SvcArea_Within 
# Public_Wat_Connection_Modified 
# Public_Wat_Connection 
# Water_Source_Comment 




# residential water use ---------------------------------------------------


# Water_Use_Residential_Rate_Ac-Ft 
# Residential_Confidence 
# Residential_GW_Use_Assessor_Ac-Ft 
# Res_GW_Use_Modified 
# Res_GW_Use_Modified_Ac_Ft 
# Res_GW_Use_Ac-Ft 
# Res_GW_Use_Comment 



# commercial water use ----------------------------------------------------

# Water_Use_Commercial_Rate_Ac-Ft 
# Commercial_Confidence 
# Commercial_GW_Use_Assessor_Ac-Ft 
# Commercial_GW_Use_Modified 
# Commercial_GW_Use_Modified_Ac-Ft 
# Commercial_GW_Use_Ac-Ft 
# Commercial_GW_Use_Comment     

    

# commercial or residential irrigation only -------------------------------

# Urban_Irrigation_GW_Use_Prelim_Ac-Ft 
# Urban_Irrigation_Modified 
# Urban_Irrigation_Modified_Ac-Ft 
# Urban_Irrigation_GW_Use_Ac-Ft 
# Urban_Irrigation_GW_Use_Comment 
    

# commercial or residential irrigation only -------------------------------

# School_Golf_GW_Use_prelim_Ac-Ft 
# School_Golf_Modified 
# School_Golf_Modified_Ac-Ft 
# School_Golf_GW_Use_Ac-Ft 
# School_Golf_GW_Use_Comment 
    

# ag water use ------------------------------------------------------------
# Raftelis report document pgs 25-27

# crop data - intersect to son. For reference, crop classes:
### C = Citrus and subtropical
### D = Deciduous Fruits and Nuts
### G = Grain
### P = Pasture
### T = Truck Nursery and Berry Crops
### V = Vineyard
### X = other
crop <- path(data_path, "general/crops/i15_Crop_Mapping_2018.shp") %>% 
  st_read() %>% 
  filter(COUNTY == "Sonoma") %>% 
  st_transform(epsg) %>% 
  st_make_valid() %>% 
  st_intersection(son) %>% 
  select(crop_class = CLASS2)

# get crops per apn and recalculate area
crops_per_apn <- st_intersection(select(pson, APN), crop) %>% 
  mutate(crop_acres = as.numeric(units::set_units(st_area(geometry), "acres")))

# applied water (in acre feet per acre) per crop from the Raftellis report
# crop class "X" (other) does not have a value in the report, so assume 0.6
crop_applied_water <- tibble(crop_class = c("C", "D", "T", "V", "G", "P", "X"),
                             applied_af_acre = c(1.8, 1.8, 1.8, 0.6, 0.3, 0.04, 0.6))

# add applied water and calcualte acre feet used per parcel
crops_per_apn <- left_join(crops_per_apn, crop_applied_water) %>% 
  mutate(applied_af = crop_acres * applied_af_acre)

# join crops to parcels in the specified format and calculate consumptive rate
pson <- pson %>% 
  left_join(st_drop_geometry(crops_per_apn)) %>% 
  mutate(
    # crop area per parcel
    Grain_Area_Ac                    = ifelse(crop_class == "G", crop_acres, 0),
    Vine_Area_Ac                     = ifelse(crop_class == "V", crop_acres, 0),
    Truck_and_Berry_Crops_Area_Ac    = ifelse(crop_class == "T", crop_acres, 0),
    Deciduous_Fruit_and_Nuts_Area_Ac = ifelse(crop_class == "D", crop_acres, 0),
    Citrus_and_Subtropical_Area_Ac   = ifelse(crop_class == "C", crop_acres, 0), 
    Cannabis_Outdoor_Area_Ac         = 0, # no cannabis
    Cannabis_Indoor_Area_Ac          = 0, # no cannabis
    Pasture_Area_Ac                  = ifelse(crop_class == "P", crop_acres, 0),
    
    # crop consumptive use (AF/year)
    Grain_rate                       = ifelse(crop_class == "G", applied_af, 0),
    Vine_rate                        = ifelse(crop_class == "V", applied_af, 0),
    Truck_and_Berry_Crops_rate       = ifelse(crop_class == "T", applied_af, 0),
    Deciduous_Fruit_and_Nuts_rate    = ifelse(crop_class == "D", applied_af, 0),
    Citrus_and_Subtropical_rate      = ifelse(crop_class == "C", applied_af, 0),
    Cannabis_Outdoor_rate            = 0, # no cannabis
    Cannabis_Indoor_rate             = 0, # no cannabis
    Pasture_rate                     = ifelse(crop_class == "P", applied_af, 0),
    
    # summation columns
    Total_Crop_Area_prelim_Ac = rowSums(across(ends_with("Area_Ac")), na.rm = TRUE),
    Total_Crop_Area_Ac        = NA, 
    Water_Use_Ag_Rate_Ac_Ft   = rowSums(across(ends_with("_rate")), na.rm = TRUE)
  ) %>% 
  # remove intermediate vars 
  select(-all_of(c("crop_class", "crop_acres", "applied_af_acre", "applied_af")))


# calculate ag water use from other columns
# pson %>% 
#   mutate(
#     # Ag use - surface use + recycled water (all negative values set to 0)
#     Ag_GW_Use_GIS_Ac_Ft      = NA,
#     Ag_GW_Use_Modified       = NA,
#     Ag_GW_Use_Modified_Ac_Ft = NA,
#     Ag_GW_Use_Ac_Ft          = NA,
#     Ag_GW_Use_Comment        = NA
#   )
 


# determination for GIS survey --------------------------------------------

# Residential_Water_Use_Determination 
# Commercial_Water_Use_Determination 
# Urban_Landscape_Irrigation_Water_Use_Determination 
# Ag_Irrigation_Water_Use_Determination 
# Recycled_Water_Use_Determination 
# Surface_Water_Use_Determination 
# School_GolfCourse_Water_Use_Determination 

    
    
# determination for GIS survey --------------------------------------------

# Total_Groundwater_Use_Ac-Ft 
# Jurisdiction 
# Situs_Address 
