# generate the PET database

library(tidyverse)
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)

# area of interest object to make helper functions work
aoi = "ppet"

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
fields <- c(fields, "UseCode") # add use code and drop it later

# GSA spatial data
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsa spatial data: petaluma, sonoma valley, santa rosa plain
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")
cat("Loaded B118 spatial boundaries per region.\n")


# fields to keep, add, remove ---------------------------------------------

done <- fields[fields %in% colnames(ppet)]  # cols already there
add  <- fields[!fields %in% colnames(ppet)] # cols to add
rem  <- colnames(ppet)[!colnames(ppet) %in% fields] # cols to remove
rem  <- rem[-length(rem)] # don't remove the geometry column


# parcel and contact info -------------------------------------------------

ppet <- ppet %>% 
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
f_progress()


# remove fields -----------------------------------------------------------

# remove fields that should not be in the final database
ppet <- ppet %>% select(-all_of(rem))
cat("Removed", length(rem), "fields from parcel database.\n   ",
    paste(rem, collapse = "\n    "))


# basin boundary parcels --------------------------------------------------

# Does the parcel overlap SRP, Petaluma, or Sonoma Valley basins?
# we expect PET overlaps only with SRP to the north and SON to east:
# mapview::mapview(list(son, pet, srp))

# parcels that intersect multiple basins have duplicate APN across databases
boundary_parcels_son <- ppet$APN[ppet$APN %in% pson$APN]
boundary_parcels_srp <- ppet$APN[ppet$APN %in% psrp$APN]

# boundary parcels for SON
ppet <- ppet %>% 
  mutate(
    # Is the parcel a boundary parcel
    Basin_Boundary_Parcel = ifelse(
      APN %in% boundary_parcels_son, "Yes", "No"),
    # area of the total APN across both GSAs is the recorded APN area
    Intersect_GSA_Bndry_Sum_Acres = ifelse(
      APN %in% boundary_parcels_son, LandSizeAcres, NA),
    # adjust area of the bisected parcels in the GSA. remember, we clipped 
    # to the B118 basin polygon, so the area is just the calculated area!
    LandSizeAcres = ifelse(
      APN %in% boundary_parcels_son,
      as.numeric(units::set_units(st_area(geometry), acres)), 
      LandSizeAcres
    ),
    # proportion of the APN in this GSA, used to assign a GSA
    area_prop_apn = LandSizeAcres / Intersect_GSA_Bndry_Sum_Acres,
    GSA_Jurisdiction_Prelim = ifelse(
      area_prop_apn > 0.5, 
      "Petaluma Valley", "Sonoma Valley"),
    # intentionally left blank for clients to evaluate and populate
    GSA_Jurisdiction_Modified = NA,
    GSA_Jurisdiction_Mod_Value = NA,
    GSA_Jurisdiction = NA
  ) %>% 
  # remove intermediate vars
  select(-area_prop_apn)

# boundary parcels for SRP
ppet <- ppet %>% 
  mutate(
    # Is the parcel a boundary parcel
    Basin_Boundary_Parcel = ifelse(
      APN %in% boundary_parcels_srp, "Yes", Basin_Boundary_Parcel),
    # area of the total APN across both GSAs is the recorded APN area
    Intersect_GSA_Bndry_Sum_Acres = ifelse(
      APN %in% boundary_parcels_srp, 
      LandSizeAcres, Intersect_GSA_Bndry_Sum_Acres),
    # adjust area of the bisected parcels in the GSA. remember, we clipped 
    # to the B118 basin polygon, so the area is just the calculated area!
    LandSizeAcres = ifelse(
      APN %in% boundary_parcels_srp,
      as.numeric(units::set_units(st_area(geometry), acres)), 
      LandSizeAcres
    ),
    # proportion of the APN in this GSA, used to assign a GSA
    area_prop_apn = LandSizeAcres / Intersect_GSA_Bndry_Sum_Acres,
    GSA_Jurisdiction_Prelim = ifelse(
      area_prop_apn > 0.5, 
      "Petaluma Valley", "Santa Rosa Plain"),
    # intentionally left blank for clients to evaluate and populate
    GSA_Jurisdiction_Modified = NA,
    GSA_Jurisdiction_Mod_Value = NA,
    GSA_Jurisdiction = NA
  ) %>% 
  # remove intermediate vars
  select(-area_prop_apn)
f_progress()

# sanity check
# mapview(srp, alpha.regions = 0) +
#   mapview(son, alpha.regions = 0) +
#   mapview(ppet, zcol = "Basin_Boundary_Parcel")


# water sources -----------------------------------------------------------

# there are many sources (inputs) of water to each parcel, and here
# we account for them based on water input type (e.g., recycled, diversion)

## recycled water ---------------------------------------------------------
# load delivery data from recycled water treatment plants

# recycled water delivered to parcels in 2016 (from billy.dixon@scwa.ca.gov)
recy <- path(
  data_path, 
  "pet/recycled_water/Petaluma_2016_Recyled Water_APN.xlsx") %>% 
  readxl::read_xlsx(sheet = 3) %>%
  select(APN, cubic_feet_adj) %>% 
  group_by(APN) %>% 
  summarise(cubic_feet_adj = sum(cubic_feet_adj, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # convert feet^3 to AF
  mutate(Recycled_Water_Use_Ac_Ft = cubic_feet_adj * 2.29569e-5) %>% 
  select(-cubic_feet_adj)

# add recycled water parcels to parcel data
ppet <- left_join(ppet, recy, by = "APN") %>% 
  mutate(Recycled_Water_Connection = ifelse(
    !is.na(Recycled_Water_Use_Ac_Ft), "Yes", "No")
  )
f_progress()
f_verify_non_duplicates()


## surface water connection -----------------------------------------------
# read ewrims data, filter to SON, transform, select relevant cols
ewrims <- dir_ls(path(data_path, "general/ewrims")) %>% 
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

# add surface water use (AF/year) to parcels, but be careful, as some 
# parcels have MULTIPLE ewrims points and these must be summarized
ewrims_key <- st_join(select(ppet, APN), ewrims) %>% 
  st_drop_geometry() %>% 
  group_by(APN) %>% 
  summarise(Surface_Water_Use_Ac_Ft = sum(
    Surface_Water_Use_Ac_Ft, na.rm = TRUE)
  ) %>% 
  ungroup()

ppet <- left_join(ppet, ewrims_key) %>% 
  mutate(Surface_Water_Connection = ifelse(
    !is.na(Surface_Water_Use_Ac_Ft) & Surface_Water_Use_Ac_Ft > 0, 
    "Yes", "No"))
f_progress()
f_verify_non_duplicates()


## wells ------------------------------------------------------------------

# Sonoma county wells - deduplicate
sc_wells <- path(data_path, "general", 
                "soco_wells/all_soco_wells_spatial.shp") %>% 
  st_read() %>% 
  st_transform(epsg) %>% 
  st_intersection(select(ppet, APN)) %>%
  add_count(APN, name = "Well_Count") %>% 
  group_by(APN) %>% 
  mutate(Well_Log_Nos = paste(Log_No, collapse = "; ")) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(APN, Well_Count, Well_Log_Nos) %>% 
  st_drop_geometry()

# petaluma wells cleaned
pet_wells <- path(data_path, "pet/public_water_connection",
                  "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
  readxl::read_xlsx(sheet = 2) %>% 
  select(APN = APN_NoHyp) %>% 
  mutate(
    APN = glue::glue(
      "{substr(APN, 1, 3)}-{substr(APN, 4, 6)}-{substr(APN, 7, 9)}")) %>% 
  # de-duplicate: remove APNs already recorded in the SC wells data
  filter(!APN %in% sc_wells$APN) %>% 
  count(APN, sort = TRUE) %>% 
  rename(Well_Count = n) %>% 
  mutate(Well_Log_Nos = NA) # well logs not provided

all_wells <- bind_rows(sc_wells, pet_wells)

# Permit Sonoma wells to remove
ps_wells <- path(data_path, "pet/public_water_connection",
                 "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
  readxl::read_xlsx(sheet = 3) %>% 
  select(APN = APN_NoHyp) %>% 
  mutate(
    APN = glue::glue(
      "{substr(APN, 1, 3)}-{substr(APN, 4, 6)}-{substr(APN, 7, 9)}")) 

# remove permit sonoma wells
all_wells <- all_wells %>% filter(!APN %in% ps_wells$APN)

# DEPRICATED: just know that more detailed well data is here
# Sonoma count well data - deduplicate
# sc_wells_data <- path(data_path, "general", 
#                      "soco_wells/all_sonoma_county_wells.xlsx") %>% 
#   readxl::read_xlsx() %>% 
#   select(APN:OtherObservations) %>% 
#   group_by(Log_No) %>% 
#   slice(1) %>% 
#   ungroup()

# DEPRICATED: simplified by joining spatial well data to ppet above
# combine and return vector of APNs with a well present
# sc_wells_apn <- left_join(sc_wells, sc_wells_data, by = "Log_No") %>% 
#   # There is a lot of WCR data we're dropping, but it's all there!
#   select(Log_No) %>% 
#   st_intersection(select(ppet, APN)) %>% 
#   pull(APN) %>% 
#   unique()

# populate database columns
ppet <- ppet %>% 
  left_join(all_wells) %>% 
  mutate(
    # no well count means 0 onsite wells
    Well_Count = ifelse(is.na(Well_Count), 0, Well_Count),
    Active_Well = ifelse(Well_Count > 0, "Yes", "No"),
    Shared_Well = NA, # placeholder for future review
    Shared_Well_APN = NA, # placeholder for future review
    Well_Records_Available = ifelse(Well_Count > 0, "Yes", "No"),
    Onsite_Well = ifelse(
      Active_Well == "Yes" | Well_Records_Available == "Yes", 
      "Yes", "No"),
    Urban_Well = "No" # placeholder for future review
  ) 

# special deactivated wells 
deactivated_wells <- path(data_path, "pet/public_water_connection",
                          "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
  readxl::read_xlsx(sheet = 4) %>% 
  select(APN = APN_NoHyp) %>% 
  mutate(
    APN = glue::glue(
      "{substr(APN, 1, 3)}-{substr(APN, 4, 6)}-{substr(APN, 7, 9)}")) 

f_progress()
f_verify_non_duplicates()


## water service areas ----------------------------------------------------

# water service areas in SON
wsa <- path(data_path, "general", "water_system_boundaries",
            "SABL_Public_083121/SABL_Public_083121.shp") %>% 
  st_read() %>% 
  st_transform(epsg) %>% 
  st_intersection(pet) %>% 
  select(CA_DrinkingWater_SvcArea_Name = WATER_SY_1)

# sanity check
# mapview(pet) + mapview(wsa)

# add water service areas to parcel data, first need to summarize data
# to avoid duplicates where a parcel falls within more than one water system!
wsa_key <- st_join(ppet, wsa) %>% 
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

ppet <- left_join(ppet, wsa_key) %>% 
  mutate(CA_DrinkingWater_SvcArea_Within = 
           ifelse(!is.na(CA_DrinkingWater_SvcArea_Name), "Yes", "No"),
         Public_Water_Connection_Modified = NA,
         Public_Water_Connection = 
           ifelse(CA_DrinkingWater_SvcArea_Within == "Yes", "Yes", "No"), 
         Water_Source_Comment = NA)

f_verify_non_duplicates()

# -------------------------------------------------------------------------
# TODO: add explicit connection data (blocked by Shelly)
socity <- path(
  data_path, "son", "public_water_connection", "city_of_sonoma", 
  "Sonoma City Water Service Connections within the GSA.xlsx") %>% 
  readxl::read_xlsx() %>% 
  select(APN = `APN Dash`) 

# if an explicit connection is present, ensure it is represented
ppet <- ppet %>% 
  mutate(Public_Water_Connection = ifelse(
    APN %in% socity$APN | Public_Water_Connection == "Yes",
    "Yes", "No"))
# -------------------------------------------------------------------------

# ensure public water connection is listed for specified Accessor Use Codes
accessor_key_path <- path(data_path, "general/water_use_by_accessor_code/Water  Use from Assessor Land Use Code 8_27_2021.xlsx")
pwc_accessor_key <- accessor_key_path %>% 
  readxl::read_xlsx(sheet = 3, range = "B2:C27") %>% 
  janitor::clean_names() %>% 
  mutate(use_code = str_pad(use_code, 4, "left", "0"))

# if the parcel is within a water service area and the use code is listed, 
# mark a public water service connection even if not explicitly listed
ppet <- ppet %>% 
  mutate(Public_Water_Connection = ifelse(
    CA_DrinkingWater_SvcArea_Within == "Yes" & 
      UseCode %in% pwc_accessor_key$use_code,
    "Yes", Public_Water_Connection)
  )

f_progress()
f_verify_non_duplicates()


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
ppet <- ppet %>% 
  mutate(
    # case 1: urban residential: within water system with onsite well
    # that's not in the deactivated wells list specific to PET
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Water_Connection == "Yes" & 
               Onsite_Well == "Yes" & 
               # specific to PET
               !APN %in% deactivated_wells$APN, 
             res_rate_urban, NA),
    # case 2: rural residential outside water system with 1 building
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Water_Connection == "No" & 
               UseCode_Description %in% res_double &
               UseCode_Description == "Residential",
             res_rate_rural_single, Res_GW_Use_Prelim_Ac_Ft),
    # case 3: rural residential outside water system with 2 buildings
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Water_Connection == "No" & 
               UseCode_Description %in% res_double &
               UseCode_Description == "Residential",
             res_rate_rural_double, Res_GW_Use_Prelim_Ac_Ft),
    # case 4: rural residential outside water system with 3 buildings
    Res_GW_Use_Prelim_Ac_Ft = 
      ifelse(Public_Water_Connection == "No" & 
               UseCode_Description %in% res_triple &
               UseCode_Description == "Residential",
             res_rate_rural_triple, Res_GW_Use_Prelim_Ac_Ft)
  )

 # blank fields to permit revision of the data
ppet <- ppet %>% 
  mutate(Res_GW_Use_Modified       = "No",
         Res_GW_Use_Modified_Ac_Ft = NA,
         Res_GW_Use_Comment        = NA,
         Res_GW_Use_Ac_Ft = ifelse(Res_GW_Use_Modified == "Yes", 
                                   Res_GW_Use_Modified_Ac_Ft, 
                                   Res_GW_Use_Prelim_Ac_Ft))


# Res_W_Use_Assessor_Ac_Ft = Water use rate based off assessor use code
# Dependency provided by Rob P on 2021-11-16
res_use_accessor_key <- readxl::read_xlsx(accessor_key_path, 
                                          sheet = 2) %>% 
  janitor::clean_names() %>% 
  mutate(UseCode = str_pad(land_use_code, 4, "left", "0")) %>% 
  select(UseCode, 
         Res_W_Use_Assessor_Ac_Ft = residential_use, 
         Commercial_W_Use_Assessor_Ac_Ft = commercial_industrial_misc_use)

# add Residential and Commercial Water Use based on Accessor Code
ppet <- left_join(ppet, res_use_accessor_key) %>% 
  # NA values overwritten with 0
  mutate(
    Res_W_Use_Assessor_Ac_Ft = ifelse(
      is.na(Res_W_Use_Assessor_Ac_Ft), 
      0, Res_W_Use_Assessor_Ac_Ft),
    Commercial_W_Use_Assessor_Ac_Ft = ifelse(
      is.na(Commercial_W_Use_Assessor_Ac_Ft), 
      0, Commercial_W_Use_Assessor_Ac_Ft)
  )

f_progress()
f_verify_non_duplicates()


# commercial water use ----------------------------------------------------

# reported commercial uses
deactivated_wells <- path(data_path, "pet/public_water_connection",
                          "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
  readxl::read_xlsx(sheet = 6) %>% 
  select(APN, Commercial_GW_Use_Prelim_Ac_Ft = `ac-ft/yr`) 

ppet <- ppet %>% 
  left_join(deactivated_wells)

# blank fields to permit revision of the data
ppet <- ppet %>% 
  mutate(Commercial_GW_Use_Modified       = "No",
         Commercial_GW_Use_Modified_Ac_Ft = NA,
         Commercial_GW_Use_Comment        = NA,
         Commercial_GW_Use_Ac_Ft = ifelse(Commercial_GW_Use_Modified == "Yes", 
                                          Commercial_GW_Use_Modified_Ac_Ft, 
                                          Commercial_GW_Use_Prelim_Ac_Ft))

f_progress()
f_verify_non_duplicates()


# urban irrigation --------------------------------------------------------

# all urban wells are set to "No" for now, per instructions in the data 
# dictionary: '(default value is "No")... Parcel sets from GUIDE Survey or 
# Cities will be used in the future to set to "Yes"'

# if there’s an urban well & public water connection, assume 0.1 AF/yr, else 0
ppet <- ppet %>% 
  mutate(
    Urban_Irrigation_GW_Use_Prelim_Ac_Ft = ifelse(
      Urban_Well == "Yes" & Public_Water_Connection == "Yes", 0.1, 0))

# blank fields to permit revision of the data
ppet <- ppet %>% 
  mutate(Urban_Irrigation_Modified       = "No",
         Urban_Irrigation_Modified_Ac_Ft = NA,
         Urban_Irrigation_GW_Use_Comment = NA,
         Urban_Irrigation_GW_Use_Ac_Ft   = ifelse(
           Urban_Irrigation_Modified == "Yes", 
           Urban_Irrigation_GW_Use_Modified_Ac_Ft, 
           Urban_Irrigation_GW_Use_Prelim_Ac_Ft))

f_progress()
f_verify_non_duplicates()


# schools and golf courses ------------------------------------------------

# Reference ET0 in feet, via CIMIS ET0 zone 8: 
# https://cimis.water.ca.gov/App_Themes/images/etozonemap.jpg
et <- 4.1 

# Calculate applied water at schools from ET assuming 
# application efficiency = 0.65 (65%) from Sandoval, 2010. 
# http://watermanagement.ucdavis.edu/research/application-efficiency/
aw <- et / (1 - 0.65) # feet

# school locations
# school_codes <- filter(ppet, str_detect(UseCode_Description, "SCHOOL")) %>% mapview()

# School_Golf_GW_Use_prelim_Ac_Ft 
ppet <- ppet %>% 
  mutate(School_Golf_GW_Use_Prelim_Ac_Ft = ifelse(
    str_detect(UseCode_Description, "SCHOOL|GOLF"),
    aw/LandSizeAcres, 0)) 

# blank fields to permit revision of the data
ppet <- ppet %>% 
  mutate(School_Golf_Modified       = "No",
         School_Golf_Modified_Ac_Ft = NA,
         School_Golf_GW_Use_Comment = NA,
         School_Golf_GW_Use_Ac_Ft = ifelse(School_Golf_Modified == "Yes", 
                                           School_Golf_GW_Use_Modified_Ac_Ft, 
                                           School_Golf_GW_Use_Prelim_Ac_Ft)) 

f_progress()
f_verify_non_duplicates()


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
  st_intersection(pet) %>% 
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

# get crops per APN and recalculate area, and as before, because there many
# APN with > 1 crop, we need to make summarize the data before joining!
crops_per_apn <- st_intersection(select(ppet, APN), crop) %>% 
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

# applied water (in acre feet per acre) per crop from the Raftellis report
# crop class "X" (other) does not have a value in the report, so assume 0.6
crop_applied_water <- tibble(
  crop_class = c("Citrus_and_Subtropical", "Deciduous_Fruit_and_Nuts", 
                 "Truck_and_Berry_Crops", "Vine", "Grain", "Pasture", "X"),
  applied_af_acre = c(1.8, 1.8, 1.8, 0.6, 0.3, 0.04, 0.6))

# add applied water and calculate acre feet used per parcel, but because there
# are multiple crops per field, we need to make sure the output dataframe has
# only one row per APN, and is in wide format ready for the join to `ppet`
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
ppet <- ppet %>% 
  left_join(crops_per_apn_key) %>% 
  mutate(
    # crop area per parcel
    Cannabis_Outdoor_Area_Ac         = 0, # no cannabis
    Cannabis_Indoor_Area_Ac          = 0, # no cannabis
    
    # crop consumptive use (AF/year)
    Cannabis_Outdoor_Rate            = 0, # no cannabis
    Cannabis_Indoor_Rate             = 0, # no cannabis
    
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

f_verify_non_duplicates()

# calculate groundwater ag water use
ppet <- ppet %>% 
  mutate(
    # first ensure that NA values in water budget components go to 0
    Water_Use_Ag_Rate_Ac_Ft  = ifelse(
      is.na(Water_Use_Ag_Rate_Ac_Ft), 0, Water_Use_Ag_Rate_Ac_Ft),
    Surface_Water_Use_Ac_Ft  = ifelse(
      is.na(Surface_Water_Use_Ac_Ft), 0, Surface_Water_Use_Ac_Ft),
    Recycled_Water_Use_Ac_Ft = ifelse(
      is.na(Recycled_Water_Use_Ac_Ft), 0, Recycled_Water_Use_Ac_Ft),
    # Ag GW use is the following mass balance:
    Ag_GW_Use_GIS_Ac_Ft = 
      Water_Use_Ag_Rate_Ac_Ft - 
      (Surface_Water_Use_Ac_Ft + Recycled_Water_Use_Ac_Ft))

# if a parcel receives more water from surface and recycled sources 
# than estimated demand, the calculated groundwater use is negative, so
# we coerce this to zero
ppet <- ppet %>% 
  mutate(Ag_GW_Use_GIS_Ac_Ft = ifelse(
    Ag_GW_Use_GIS_Ac_Ft < 0, 0, Ag_GW_Use_GIS_Ac_Ft))

# No Idle Acres to start - this is reported
ppet <- ppet %>% mutate(Idle_Ac = 0)

# modifications
ppet <- ppet %>%
  mutate(
    # Ag use - surface use + recycled water (all negative values set to 0)
    Ag_GW_Use_Modified       = "No",
    Ag_GW_Use_Modified_Ac_Ft = NA,
    Ag_GW_Use_Ac_Ft          = NA,
    Ag_GW_Use_Comment        = NA
  )

# blank fields to permit revision of the data
ppet <- ppet %>% 
  mutate(Ag_GW_Use_Modified       = "No",
         Ag_GW_Use_Modified_Ac_Ft = NA,
         Ag_GW_Use_Comment        = NA,
         Ag_GW_Use_Ac_Ft = ifelse(Ag_GW_Use_Modified == "Yes", 
                                  Ag_GW_Use_Modified_Ac_Ft, 
                                  Ag_GW_Use_GIS_Ac_Ft)) 

f_progress()
f_verify_non_duplicates()


# determination for GIS survey --------------------------------------------

ppet <- ppet %>% 
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

f_progress()
f_verify_non_duplicates()


# total calculations ------------------------------------------------------

# ensure NA values go to 0 so the result is calculable
ppet <- ppet %>% 
  mutate(across(ends_with("GW_Use_Ac_Ft"), ~ifelse(is.na(.x), 0, .x))) 

# calculate total groundwater use
ppet <- ppet %>% 
  mutate(
    Total_Groundwater_Use_Ac_Ft =
      Res_GW_Use_Ac_Ft + 
      Commercial_GW_Use_Ac_Ft + 
      Ag_GW_Use_Ac_Ft + 
      School_Golf_GW_Use_Ac_Ft + 
      Urban_Irrigation_GW_Use_Ac_Ft,
    Total_Groundwater_Use_PublicView = NA
  )

# additional columns

# Jurisdiction 
# Situs_Address 


# final manual tests ------------------------------------------------------

# drop no longer needed UseCode column
ppet <- select(ppet, -UseCode)

# sanity check: cols that still need to be added
add[!add %in% colnames(ppet)]

f_progress()
f_verify_non_duplicates()


# write complete parcel data ----------------------------------------------

ppet %>% 
  write_rds(path(data_path, "data_output/pet_parcel_complete.rds"))
