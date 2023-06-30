# generate the PET database

library(tidyverse)
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)

dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

data_path <- Sys.getenv("DATA_PATH")
epsg <- as.numeric(Sys.getenv("EPSG"))

# area of interest object to make helper functions work
aoi = "ppet"

# delete complete DBs
print('deleting...')
gjson_out <- path(data_path, "data_output/pet_parcel_complete.rds")
if(file_exists(gjson_out)) file_delete(gjson_out)
print('done deleting')

gw_use_rate = 40.00 #$ per AF
unsub_gw_sub_rate = 147.0 #unsubsidized rate

accessor_key_path <- path(data_path, "general", "water_use_by_accessor_code",
                          "Final 2022 Water Use from Assessor Land Use Code.xlsx")

# load data ---------------------------------------------------------------


# preprocessed spatial parcels from Sonoma Co parcels
pson <- read_rds(path(data_path, "data_output/son_parcel.rds"))
ppet <- read_rds(path(data_path, "data_output/pet_parcel.rds"))
psrp <- read_rds(path(data_path, "data_output/srp_parcel.rds"))
cat("Loaded preprocedded spatial parcels from Sonoma County.\n")

# final fields to use
fields <- get_schema_fields(data_path)
fields <- c(fields, "UseCode", 'edge') # add use code and drop it later

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
ppet <-parcel_contact(ppet)


f_progress()


### add parcel land size
# ppet <- load_land_size(data_path, ppet)

# remove fields -----------------------------------------------------------

# remove fields that should not be in the final database
ppet <- ppet %>% select(-all_of(rem))
cat("Removed", length(rem), "fields from parcel database.\n   ",
    paste(rem, collapse = "\n    "))


# basin boundary parcels --------------------------------------------------

# Does the parcel overlap SRP, Petaluma, or Sonoma Valley basins?
# we expect PET overlaps only with SRP to the north and SON to east:
# mapview::mapview(list(son, pet, srp))

# # parcels that intersect multiple basins have duplicate APN across databases

ppet <- fill_parcel_info(ppet, 'Petaluma Valley')

check_use_codes(ppet)

f_progress()


# water sources -----------------------------------------------------------

# there are many sources (inputs) of water to each parcel, and here
# we account for them based on water input type (e.g., recycled, diversion)

## recycled water ---------------------------------------------------------
# load delivery data from recycled water treatment plants

# recycled water delivered to parcels in 2018-2022 (from billy.dixon@scwa.ca.gov)
recy <- path(
  data_path, 
  "pet/recycled_water/output/City of Petaluma 2018-2022 Recycled Water Summary.xlsx") %>% 
  readxl::read_xlsx(sheet = 1) %>%
  select(APN, Mean) %>% 
  mutate(Recycled_Water_Use_Ac_Ft = Mean) %>% 
  select(-Mean)

# add recycled water parcels to parcel data
ppet <- left_join(ppet, recy, by = "APN") %>% 
  mutate(Recycled_Water_Connection = ifelse(
    !is.na(Recycled_Water_Use_Ac_Ft), "Yes", "No")
  )

ppet <- add_recycled_water_modified(ppet)

ppet <- add_recycled_water_connection_modified(ppet)

f_progress()
f_verify_non_duplicates()


## surface water connection -----------------------------------------------
# read ewrims data, filter to SON, transform, select relevant cols
ewrims_key <- f_load_surface_water(data_path)

ppet <- left_join(ppet, ewrims_key) %>% 
  mutate(Surface_Water_Connection = ifelse(
    !is.na(Surface_Water_Use_Ac_Ft) & Surface_Water_Use_Ac_Ft > 0, 
    "Yes", "No"))

ppet <- add_surface_water_modified(ppet)

ppet <- add_surface_water_connection_modified(ppet)

f_progress()
f_verify_non_duplicates()


## wells ------------------------------------------------------------------

# Sonoma county wells - deduplicate
sc_wells <- add_wells(ppet)

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

# remove permit sonoma wells
# line removed - incorrectly removed wells from the above cross-connection when 
#it should have changed parcels to no connection instead
#all_wells <- all_wells %>% filter(!APN %in% ps_wells$APN)

ppet <- calc_wells(ppet, all_wells)


ppet <- ppet %>% replace_Onsite_Well_modified() %>%
  replace_Well_Records_Available_modified() %>%
  replace_shared_well_APN_modified() %>%
  replace_shared_well_modified() %>%
  replace_active_well_modified()

## special deactivated wells 
#deactivated_wells <- path(data_path, "pet/public_water_connection",
#                          "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
#  readxl::read_xlsx(sheet = 4) %>% 
#  select(APN) 

f_progress()
f_verify_non_duplicates()


## water service areas ----------------------------------------------------

# water service areas in SON
# # add water service areas to parcel data, first need to summarize data
# # to avoid duplicates where a parcel falls within more than one water system!


wsa_key <- get_wsa_key(ppet, pet)

ppet <- left_join(ppet, wsa_key) %>% 
  mutate(CA_DrinkingWater_SvcArea_Within = 
           ifelse(!is.na(CA_DrinkingWater_SvcArea_Name),"Yes", "No"))


f_verify_non_duplicates()


ppet <-add_public_water_connection(ppet)
ppet <-pwc_use_code_fix(ppet)

f_progress()
f_verify_non_duplicates()


# residential water use ---------------------------------------------------

# The Urban Residential Groundwater user class represents residential properties
# in areas served by water service providers that also have a well on the 
# property... Raftelis and Staff assumed that these wells would primarily be 
# used for irrigation purposes... it is assumed that Urban Residential Groundwater 
# Users extract on average 0.1 AF per parcel per year for irrigation purposes.

# Res_W_Use_Assessor_Ac_Ft = Water use rate based off assessor use code
# Dependency provided by Rob P on 2021-11-16
res_use_accessor_key <- readxl::read_xlsx(accessor_key_path, 
                                          sheet = 2) %>% 
  janitor::clean_names() %>% 
  mutate(UseCode = str_pad(land_use_code, 4, "left", "0")) %>% 
  select(UseCode, 
         Res_W_Use_Assessor_Ac_Ft = residential_use, 
         Commercial_W_Use_Assessor_Ac_Ft = commercial_industrial_misc_use)

#TODO check UseCode Modified option
ppet <- replace_use_code(ppet)

# add Residential and Commercial Water Use based on Accessor Code
ppet <- left_join(ppet, res_use_accessor_key) 

# Res_GW_Use_Prelim_Ac_Ft is Res_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
ppet <- ppet %>% 
  mutate(Res_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No", 
    Res_W_Use_Assessor_Ac_Ft,
    0
  ))

# load modified fields
ppet <- join_with_modified(ppet)

# blank fields to permit revision of the data
ppet <- ppet %>% 
  mutate(
         Res_GW_Use_Ac_Ft = ifelse(Res_GW_Use_Modified == "Yes", 
                                   Res_GW_Use_Modified_Ac_Ft, 
                                   Res_GW_Use_Prelim_Ac_Ft))

f_progress()
f_verify_non_duplicates()


# commercial water use ----------------------------------------------------


# Commercial_GW_Use_Prelim_Ac_Ft is Commercial_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
ppet <- ppet %>%
  mutate(Commercial_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No",
    Commercial_W_Use_Assessor_Ac_Ft,
    0
  ))


ppet <- ppet %>%
  mutate(
         Commercial_GW_Use_Ac_Ft = ifelse(Commercial_GW_Use_Modified == "Yes",
                                          Commercial_GW_Use_Modified_Ac_Ft,
                                          Commercial_GW_Use_Prelim_Ac_Ft))

f_progress()
f_verify_non_duplicates()

# urban irrigation --------------------------------------------------------

# all urban wells are set to "No" for now, per instructions in the data 
# dictionary: '(default value is "No")... Parcel sets from GUIDE Survey or 
# Cities will be used in the future to set to "Yes"'

ppet <- load_urban_wells(data_path, ppet)
ppet <- replace_urban_well_modified(ppet)

ppet <- calc_urban_irrigation(ppet)
ppet <- add_urban_irrigation_modified(ppet)

f_progress()
f_verify_non_duplicates()


# schools and golf courses ------------------------------------------------

# Reference ET0 in feet, via CIMIS ET0 zone 8: 
# https://cimis.water.ca.gov/App_Themes/images/etozonemap.jpg
# et <- 4.1 

ppet <- calculate_lawn(ppet, pet)

ppet <- school_golf_calc(ppet)

f_progress()
f_verify_non_duplicates()


# crop water use ----------------------------------------------------------
# Raftelis report document pgs 25-27

ppet <-add_cannabis_modified(ppet)
crop <-load_land_use(ppet)
ppet <-calc_crop_water_use(ppet, crop)



f_verify_non_duplicates()

# calculate groundwater ag water use
ppet <- calc_ag_use(ppet)



f_progress()
f_verify_non_duplicates()


# determination for GIS survey --------------------------------------------
ppet <- gis_survey_determination(ppet)


f_progress()
f_verify_non_duplicates()


# total calculations ------------------------------------------------------
ppet <- total_use_calc(ppet, gw_use_rate)


# final manual tests ------------------------------------------------------

# sanity check: cols that still need to be added
add[!add %in% colnames(ppet)]

f_progress()
f_verify_non_duplicates()


# write complete parcel data ----------------------------------------------

ppet %>% 
  write_rds(path(data_path, "data_output/pet_parcel_complete.rds"))
cat("Complete PET.\n")
