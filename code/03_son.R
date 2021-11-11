# generate the SON database

library(tidyverse)
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)


# load data ---------------------------------------------------------------

# preprocessed spatial parcels from Sonoma Co parcels
pson <- read_rds(path(data_path, "data_output/son_parcel.rds"))
ppet <- read_rds(path(data_path, "data_output/pet_parcel.rds"))
psrp <- read_rds(path(data_path, "data_output/srp_parcel.rds"))
cat("Loaded preprocedded spatial parcels from Sonoma County.\n")

# final fields to use
fields <- path(data_path, "schema/2021_11_03_schema.xlsx") %>% 
  readxl::read_xlsx(sheet = 2, range = cellranger::cell_cols("D")) %>% 
  set_names("name") %>% 
  filter(!is.na(name)) %>% 
  pull(name)

# GSA spatial data
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsas: petaluma, sonoma valley, santa rosa plain
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")
cat("Loaded B118 spatial boundaries per region.\n")


# fields to keep, add, remove ---------------------------------------------

done <- fields[fields %in% colnames(pson)]  # cols already there
add  <- fields[!fields %in% colnames(pson)] # cols to add
rem  <- colnames(pson)[!colnames(pson) %in% fields] # cols to remove
rem  <- rem[-length(rem)] # don't remove the geometry column

# empty vector to track added fields
added <- c()
f_progress <- function(){cat(round(length(intersect(add, added))/length(add)*100), 
                             "% complete.\n")}

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
    MailingAddress4     = MailAdr4)
cat("Added 8 fields to data:\n   ", paste(add[1:8], collapse = "\n    "))
added <- c(added, add[1:8])
f_progress()

# remove fields -----------------------------------------------------------

pson <- pson %>% select(-all_of(rem))
cat("Removed", length(rem), "fields from parcel database.\n   ",
    paste(rem, collapse = "\n    "))


# basin boundary parcels --------------------------------------------------

# Does the parcel overlap SRP, Petaluma, or Sonoma Valley basins?
# we expect SON overlaps only with PET, see map below:
# mapview::mapview(list(son, pet, srp))

# parcels that intersect multiple basins have duplicate APN across databases
boundary_parcels <- pson$APN[pson$APN %in% ppet$APN]

pson <- pson %>% 
  mutate(
    # Is the parcel a boundary parcel
    Basin_Boundary_Parcel = ifelse(APN %in% boundary_parcels, "Yes", "No"),
    # area of the total APN across both GSAs is the recorded APN area
    Intersect_GSA_Bndry_Sum_Acres = ifelse(APN %in% boundary_parcels,
                                           LandSizeAcres, NA),
    # adjust area of the bisected parcels in the GSA. remember, we clipped 
    # to the B118 basin polygon, so the area is just the calculated area!
    LandSizeAcres = ifelse(
      APN %in% boundary_parcels,
      as.numeric(units::set_units(st_area(geometry), acres)), 
      LandSizeAcres
    ),
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
cat("Added 6 fields to data:\n   ", paste(add[9:14], collapse = "\n    "))
added <- c(added, add[9:14])
f_progress()

# sanity check
# mapview(pet, alpha.regions = 0) + 
#   mapview(son, alpha.regions = 0) + 
#   mapview(pson, zcol = "Basin_Boundary_Parcel")


# water sources -----------------------------------------------------------
## recycled water ---------------------------------------------------------
# load delivery data from recycled water treatment plants

# recycled water delivered to parcels in 2016 (from billy.dixon@scwa.ca.gov)
recy <- path(data_path, "son/recycled_water/SVCSD Recycled Water Use APNs.xlsx") %>% 
  readxl::read_xlsx(sheet = 4) %>% 
  rename(Recycled_Water_Use_Ac_Ft = af_yr_2016)

# add recycled water parcels to parcel data
pson <- left_join(pson, recy, by = "APN") %>% 
  mutate(Recycled_Water_Connection = ifelse(!is.na(Recycled_Water_Use_Ac_Ft), "Yes", "No"))
cat("Added 2 fields to data:\n   ", paste(add[15:16], collapse = "\n    "))
added <- c(added, add[15:16]) # update add vector
f_progress()


## surface water connection -----------------------------------------------
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
  st_transform(epsg) %>% 
  select(-county)

# add surface water use (AF/year) to parcels
pson <- st_join(pson, ewrims) %>% 
  mutate(Surface_Water_Connection = ifelse(!is.na(Surface_Water_Use_Ac_Ft), "Yes", "No"))
cat("Added 2 fields to data:\n   ", paste(add[17:18], collapse = "\n    "))
added <- c(added, add[17:18]) # update add vector
f_progress()


## wells ------------------------------------------------------------------
# Sonoma county wells - deduplicate
scwells <- path(data_path, "general", 
                "soco_wells/all_soco_wells_spatial.shp") %>% 
  st_read() %>% 
  st_transform(epsg) %>% 
  st_intersection(son) %>%
  group_by(Log_No) %>% 
  slice(1) %>% 
  ungroup()

# Sonoma count well data - deduplicate
scwells_data <- path(data_path, "general", 
                     "soco_wells/all_sonoma_county_wells.xlsx") %>% 
  readxl::read_xlsx() %>% 
  select(APN:OtherObservations) %>% 
  group_by(Log_No) %>% 
  slice(1) %>% 
  ungroup()

# combine and retain important cols
scwells <- left_join(scwells, scwells_data, by = "Log_No") %>% 
  select(Data_Source) %>% 
  st_intersection(select(pson, APN)) %>% # wells in parcels
  # select(Data_Source, APN) %>% # data sources: sweetkind/scwa, oswcr, prmd
  mutate(well_present = TRUE) # placeholder for logical test below (drop)

# populate database columns
pson <- pson %>% 
  left_join(st_drop_geometry(scwells), by = "APN") %>% 
  mutate(
    Active_Well = ifelse(!is.na(well_present), "Yes", "No"),
    Shared_Well = NA, # placeholder for future review
    Shared_Well_APN = NA, # placeholder for future review
    Well_Records_Available = ifelse(!is.na(Data_Source), "Yes", "No"),
    Onsite_Well = 
      ifelse(Active_Well == "Yes" | Well_Records_Available == "Yes", "Yes", "No"),
    Urban_well = "No" # placeholder for future review
  ) %>% 
  select(-all_of(c("well_present", "Data_Source"))) # drop unnecessary cols

cat("Added 6 fields to data:\n   ", paste(add[19:24], collapse = "\n    "))
added <- c(added, add[19:24]) # update add vector
f_progress()


## water service areas ----------------------------------------------------

# water service areas in SON
wsa <- path(data_path, "general", "water_system_boundaries",
            "SABL_Public_083121/SABL_Public_083121.shp") %>% 
  st_read() %>% 
  st_transform(epsg) %>% 
  st_intersection(son) %>% 
  select(CA_DrinkingWater_SvcArea_Name = WATER_SY_1)

# add water service areas to parcel data
pson <- st_join(pson, wsa) %>% 
  mutate(CA_DrinkingWater_SvcArea_Within = 
           ifelse(!is.na(CA_DrinkingWater_SvcArea_Name), "Yes", "No"),
         Public_Wat_Connection_Modified = NA,
         Public_Wat_Connection = 
           ifelse(CA_DrinkingWater_SvcArea_Within == "Yes", "Yes", "No"), 
         Water_Source_Comment = NA)

# add explicit connection data from Sonoma City
socity <- path(data_path, "son", "public_water_connection", "city_of_sonoma", 
               "Sonoma City Water Service Connections within the GSA.xlsx") %>% 
  readxl::read_xlsx() %>% 
  select(APN = `APN Dash`) 

# if an explicit connection is present, ensure it is represented
pson <- pson %>% 
  mutate(Public_Wat_Connection = 
           ifelse(APN %in% socity$APN | Public_Wat_Connection == "Yes", "Yes", "No"))

cat("Added 5 fields to data:\n   ", paste(add[25:29], collapse = "\n    "))
added <- c(added, add[25:29]) # update add vector
f_progress()


# residential water use ---------------------------------------------------

# From Raftellis Fee Study: Any residential (or residential and agricultural 
# use) parcels remaining in areas outside of water service provider systems 
# are assumed to have or use groundwater from a private well. Available records 
# of water wells within the Subbasin are incomplete [and not spatially accurate
# enough] and not used to assess which parcels extract groundwater... used an 
# estimate of 0.5 AF of water use per year for each developed rural residential 
# parcel... An additional 0.25 AF were added for any parcels that listed 
# additional residences on the parcel, up to a maximum of 1 AF per year. 

# The Urban Residential Groundwater user class represents residential properties
# in areas served by water service providers that also have a well on the 
# property... Raftelis and Staff assumed that these wells would primarily be 
# used for irrigation purposes... it is assumed that Urban Residential Groundwater 
# Users extract on average 0.1 AF per parcel per year for irrigation purposes.

res_rate_rural_single <- 0.50 # acre feet/year for 1 building
res_rate_rural_double <- 0.75 # acre feet/year for 2 buildings
res_rate_rural_triple <- 1.00 # acre feet/year for >=3 buildings

res_rate_urban <- 0.1 # acre feet/year for urban parcels with a well

# use code descriptions mapped to single, double, or 3+ buildings
res_single <- c("SINGLE FAMILY DWELLING", "RURAL RES/SINGLE RES", 
                "ATTACHED UNIT", "CONDOMINIUM UNIT", "ONE DUPLEX (ONE STRUCTURE)",
                "DETACHED UNIT IN A PUD", "ENFORCEABLY RESTRICTED DWELLING",
                "MANUFACTURED HOME ON URBAN LOT", "RURAL RES W/MISC RES IMP",
                "RURAL RES/MANUFACTURED HOME", "SINGLE LIVE/WORK UNIT",
                "RURAL RES/SECONDARY USE", "TAXABLE MANUFACTURED HOME/RENTED SITE",
                "COOPERATIVE")
res_double <- c("RURAL RES/2 OR MORE RES", "RURAL RES SFD W/GRANNY UNIT",
                "SFD W/GRANNY UNIT", "TWO SFD ON SINGLE PARCEL", "DUET")
res_triple <- c("COMMON AREA WITH STRUCTURES")

# rural and urban groundwater use: 4 cases
pson <- pson %>% 
  mutate(
    # case 1: urban residential: within water system with onsite well
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Wat_Connection == "Yes" & 
               Onsite_Well         == "Yes", 
             res_rate_urban, NA),
    # case 2: rural residential outside water system with 1 building
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Wat_Connection == "No" & 
               UseCode_Description %in% res_double &
               UseCode_Description == "Residential",
             res_rate_rural_single, Res_GW_Use_Prelim_Ac_Ft),
    # case 3: rural residential outside water system with 2 buildings
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Wat_Connection == "No" & 
               UseCode_Description %in% res_double &
               UseCode_Description == "Residential",
             res_rate_rural_double, Res_GW_Use_Prelim_Ac_Ft),
    # case 4: rural residential outside water system with 3 buildings
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Wat_Connection == "No" & 
               UseCode_Description %in% res_triple &
               UseCode_Description == "Residential",
             res_rate_rural_triple, Res_GW_Use_Prelim_Ac_Ft)
  )

# blank fields to permit revision of the data
pson <- pson %>% 
  mutate(Res_GW_Use_Modified       = "No",
         Res_GW_Use_Modified_Ac_Ft = NA,
         Res_GW_Use_Comment        = NA,
         Res_GW_Use_Ac_Ft = ifelse(Res_GW_Use_Modified == "Yes", 
                                   Res_GW_Use_Modified_Ac_Ft, 
                                   Res_GW_Use_Prelim_Ac_Ft))


# TODO: Res_W_Use_Assessor_Ac_Ft = ? Asked Rob P on 2021-11-09
# I think we don't need to find this because we use the UseCodes already
# to find the GW use from Raftellis' previous work. 

cat("Added 5 fields to data:\n   ", paste(add[31:35], collapse = "\n    "))
added <- c(added, add[31:35]) # update add vector
f_progress()

# commercial water use ----------------------------------------------------

# TODO: Commercial_W_Use_Assessor_Ac_Ft 

# TODO: Commercial_GW_Use_Prelim_Ac_Ft


# blank fields to permit revision of the data
pson <- pson %>% 
  mutate(Commercial_GW_Use_Modified       = "No",
         Commercial_GW_Use_Modified_Ac_Ft = NA,
         Commercial_GW_Use_Comment        = NA,
         Commercial_GW_Use_Ac_Ft = ifelse(Commercial_GW_Use_Modified == "Yes", 
                                          Commercial_GW_Use_Modified_Ac_Ft, 
                                          Commercial_GW_Use_Prelim_Ac_Ft))


# urban irrigation --------------------------------------------------------

# all urban wells are set to "No" for now, per instructions in the data 
# dictionary: '(default value is "No")... Parcel sets from GUIDE Survey or 
# Cities will be used in the future to set to "Yes"'

# if there’s an urban well & public water connection, assume 0.1 AF/yr, else 0
pson <- pson %>% 
  mutate(
    Urban_Irrigation_GW_Use_Prelim_Ac_Ft = ifelse(
      Urban_well == "Yes" & Public_Wat_Connection == "Yes", 0.1, 0))

# blank fields to permit revision of the data
pson <- pson %>% 
  mutate(Urban_Irrigation_GW_Use_Modified       = "No",
         Urban_Irrigation_GW_Use_Modified_Ac_Ft = NA,
         Urban_Irrigation_GW_Use_Comment        = NA,
         Urban_Irrigation_GW_Use_Ac_Ft = ifelse(Urban_Irrigation_GW_Use_Modified == "Yes", 
                                                Urban_Irrigation_GW_Use_Modified_Ac_Ft, 
                                                Urban_Irrigation_GW_Use_Prelim_Ac_Ft))

cat("Added 5 fields to data:\n   ", paste(add[42:46], collapse = "\n    "))
added <- c(added, add[42:46]) # update add vector
f_progress()
    

# schools and golf courses ------------------------------------------------

# Calculate applied water at schools from ET assuming 
# application efficiency = 0.65 (65%) from Sandoval, 2010. 
# http://watermanagement.ucdavis.edu/research/application-efficiency/
et <- 3.9 # feet, from avg annual ET0 at nearby CIMIS stations 77 and 109
aw <- et / (1 - 0.65) # feet
school_codes <- filter(pson, str_detect(UseCode_Description, "SCHOOL")) %>% mapview()

# School_Golf_GW_Use_prelim_Ac_Ft 
pson <- pson %>% 
  mutate(School_Golf_GW_Use_prelim_Ac_Ft = 
           ifelse(str_detect(UseCode_Description, "SCHOOL"),
                  aw/LandSizeAcres, 0)) 

# sanity check: SRP estimate was 200 AF/yr, and we calculate 201 here. passes.
# pson %>% 
#   pull(School_Golf_GW_Use_prelim_Ac_Ft) %>% 
#   sum(na.rm = TRUE)
  
# blank fields to permit revision of the data
pson <- pson %>% 
  mutate(School_Golf_GW_Use_Modified       = "No",
         School_Golf_GW_Use_Modified_Ac_Ft = NA,
         School_Golf_GW_Use_Comment        = NA,
         School_Golf_GW_Use_Ac_Ft = ifelse(School_Golf_GW_Use_Modified == "Yes", 
                                           School_Golf_GW_Use_Modified_Ac_Ft, 
                                           School_Golf_GW_Use_Prelim_Ac_Ft)) 

cat("Added 5 fields to data:\n   ", paste(add[47:51], collapse = "\n    "))
added <- c(added, add[47:51]) # update add vector
f_progress()


# crop water use ----------------------------------------------------------
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

# get crops per APN and recalculate area
crops_per_apn <- st_intersection(select(pson, APN), crop) %>% 
  mutate(crop_acres = as.numeric(units::set_units(st_area(geometry), "acres")))

# applied water (in acre feet per acre) per crop from the Raftellis report
# crop class "X" (other) does not have a value in the report, so assume 0.6
crop_applied_water <- tibble(
  crop_class = c("C", "D", "T", "V", "G", "P", "X"),
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

# calculate groundwater ag water use
pson <- pson %>% 
  mutate(
    # first ensure that NA values in water budget components go to 0
    Water_Use_Ag_Rate_Ac_Ft  = ifelse(is.na(Water_Use_Ag_Rate_Ac_Ft), 
                                      0, Water_Use_Ag_Rate_Ac_Ft),
    Surface_Water_Use_Ac_Ft  = ifelse(is.na(Surface_Water_Use_Ac_Ft), 
                                      0, Surface_Water_Use_Ac_Ft),
    Recycled_Water_Use_Ac_Ft = ifelse(is.na(Recycled_Water_Use_Ac_Ft), 
                                      0, Recycled_Water_Use_Ac_Ft),
    # Ag GW use is the following mass balance:
    Ag_GW_Use_GIS_Ac_Ft = 
           Water_Use_Ag_Rate_Ac_Ft - 
           Surface_Water_Use_Ac_Ft +
           Recycled_Water_Use_Ac_Ft)

# modifications
pson <- pson %>%
  mutate(
    # Ag use - surface use + recycled water (all negative values set to 0)
    Ag_GW_Use_Modified       = "No",
    Ag_GW_Use_Modified_Ac_Ft = NA,
    Ag_GW_Use_Ac_Ft          = NA,
    Ag_GW_Use_Comment        = NA
  )

# blank fields to permit revision of the data
pson <- pson %>% 
  mutate(Ag_GW_Use_Modified       = "No",
         Ag_GW_Use_Modified_Ac_Ft = NA,
         Ag_GW_Use_Comment        = NA,
         Ag_GW_Use_Ac_Ft = ifelse(Ag_GW_Use_Modified == "Yes", 
                                  Ag_GW_Use_Modified_Ac_Ft, 
                                  Ag_GW_Use_Prelim_Ac_Ft)) 

cat("Added 20 fields to data:\n   ", paste(add[52:71], collapse = "\n    "))
added <- c(added, add[52:76]) # update add vector
f_progress()


# calc ag water use -------------------------------------------------------


 


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


# sanity check: cols that still need to be added
add[!add %in% added]
