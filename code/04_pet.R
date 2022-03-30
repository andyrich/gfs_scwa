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

# load data ---------------------------------------------------------------

# preprocessed spatial parcels from Sonoma Co parcels
pson <- read_rds(path(data_path, "data_output/son_parcel.rds"))
ppet <- read_rds(path(data_path, "data_output/pet_parcel.rds"))
psrp <- read_rds(path(data_path, "data_output/srp_parcel.rds"))
cat("Loaded preprocedded spatial parcels from Sonoma County.\n")

# final fields to use
fields <- path(data_path, "schema/2022_03_28_schema.xlsx") %>% 
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
    GSA_Jurisdiction_Prelim = ifelse(area_prop_apn > 0.5, 
                                     "Petaluma Valley", "Sonoma Valley"),
    GSA_Jurisdiction_Prelim = ifelse(is.na(GSA_Jurisdiction_Prelim), 
                                     "Petaluma Valley", GSA_Jurisdiction_Prelim),
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

# if not a basin boundary parcel, it's an interior GSA parcel
ppet <- ppet %>% 
  mutate(GSA_Jurisdiction_Prelim = ifelse(
    is.na(GSA_Jurisdiction_Prelim),
    "Petaluma Valley", GSA_Jurisdiction_Prelim))

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

# remove permit sonoma wells
# line removed - incorrectly removed wells from the above cross-connection when 
#it should have changed parcels to no connection instead
#all_wells <- all_wells %>% filter(!APN %in% ps_wells$APN)

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
    #Urban_Well = "No" # placeholder for future review
    Urban_Well = ifelse(Well_Count>0,'Yes','No')
  ) 


## special deactivated wells 
#deactivated_wells <- path(data_path, "pet/public_water_connection",
#                          "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
#  readxl::read_xlsx(sheet = 4) %>% 
#  select(APN) 

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

# list of water service areas to remove
wsa_remove = c('PETALUMA, CITY OF','PENNGROVE WATER COMPANY (PUC)')

# add water service areas to parcel data, first need to summarize data
# to avoid duplicates where a parcel falls within more than one water system!
wsa_key <- st_join(ppet, wsa) %>% 
  select(APN, CA_DrinkingWater_SvcArea_Name) %>% 
  subset(!(CA_DrinkingWater_SvcArea_Name %in% wsa_remove)) %>%
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
           ifelse(!is.na(CA_DrinkingWater_SvcArea_Name),"Yes", "No"))


f_verify_non_duplicates()


# add explicit connection data from Petaluma, Sebastapol, Sonoma, Penngrove,
# and Valley of the Moonb WD - from Shelly on 2022-01-04, Email Subject:
# Data Revision/Addition | Permit Sonoma GIS: GSA Water Service Connection | ID APN-to-Address
shelly_path <- path(data_path, "general", "address_apn.gdb")
cat("Reading explicit connection data for:\n", 
    paste(rgdal::ogrListLayers(shelly_path), collapse = "\n "), "\n")

print('using the following layer from shellys list')
print(rgdal::ogrListLayers(shelly_path)[1])

#adding all of the connections to the database, not just the first one.
connections_shelly <- rgdal::ogrListLayers(shelly_path) %>% 
  purrr::map_df(
    ~rgdal::readOGR(dsn = shelly_path, layer = .x) %>% 
      # st_drop_geometry() %>%
      st_as_sf() %>%
      select(APN))

print('these are the connections_shelly')
print(connections_shelly)
print(dim(connections_shelly))

petaluma_connect_path <- path(data_path, "pet","public_water_connection", "WaterServiceParcels_2020.xlsx")
petaluma_connect <- petaluma_connect_path %>% 
  readxl::read_xlsx(sheet = 1) %>% 
  select(APN)

print('these are the petaluma_connect')
print(petaluma_connect)
print(dim(petaluma_connect))

allconnects <- bind_rows(petaluma_connect, connections_shelly)

print('these are the allconnects')
print(allconnects)
print(dim(allconnects))

# if an explicit connection is present, ensure it is represented
ppet <- ppet %>% 
  mutate(
    Public_Water_Connection = 
      ifelse(APN %in% allconnects$APN |
               !is.na(CA_DrinkingWater_SvcArea_Name), 
             "Yes", "No"),
    Public_Water_Connection_Modified = NA,
    Water_Source_Comment = NA
  )


# ensure public water connection is listed for specified Accessor Use Codes
#accessor_key_path <- path(data_path, "general", "water_use_by_accessor_code",
#                          "Water  Use from Assessor Land Use Code 8_27_2021.xlsx")
accessor_key_path <- path(data_path, "general", "water_use_by_accessor_code",
                          "Final 2022 Water  Use from Assessor Land Use Code.xlsx")
pwc_accessor_key <- accessor_key_path %>% 
  readxl::read_xlsx(sheet = 3, range = "B2:C28") %>% 
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

# add public water connections for modified APNs:
apn_add_pwc <- path(data_path, "general/modified_apns.xlsx") %>% 
  readxl::read_xlsx(sheet = 1) %>% 
  pull(APN)

ppet <- ppet %>% 
  mutate(Public_Water_Connection = ifelse(
    APN %in% apn_add_pwc,
    "Yes", Public_Water_Connection)
  )

# Permit Sonoma wells to remove
ps_wells <- path(data_path, "pet/public_water_connection",
                 "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
  readxl::read_xlsx(sheet = 3) %>% 
  #select(APN) 
  pull(APN)

#Use the Petaluma CROSSCONNECTION DATA CLEANED #3 to remove parcels from 
#the Public_Water_Connection list. set them to 'no' 
ppet <- ppet %>% 
  mutate(Public_Water_Connection = ifelse(
    APN %in% ps_wells,
     "No", Public_Water_Connection)
  )


f_progress()
f_verify_non_duplicates()


# residential water use ---------------------------------------------------

# The Urban Residential Groundwater user class represents residential properties
# in areas served by water service providers that also have a well on the 
# property... Raftelis and Staff assumed that these wells would primarily be 
# used for irrigation purposes... it is assumed that Urban Residential Groundwater 
# Users extract on average 0.1 AF per parcel per year for irrigation purposes.

# res_rate_urban <- 0.1 # acre feet/year for urban parcels with a well
# 
# # rural and urban groundwater use: 4 cases
# ppet <- ppet %>% 
#   mutate(
#     # case 1: urban residential: within water system with onsite well
#     # that's not in the deactivated wells list specific to PET
#     Res_GW_Use_Prelim_Ac_Ft = 
#       ifelse(Public_Water_Connection == "Yes" & 
#                Onsite_Well == "Yes" & 
#                # specific to PET
#                !APN %in% deactivated_wells$APN, 
#              res_rate_urban, NA)
#   )

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
ppet <- left_join(ppet, res_use_accessor_key) 

# Res_GW_Use_Prelim_Ac_Ft is Res_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
ppet <- ppet %>% 
  mutate(Res_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No", 
    Res_W_Use_Assessor_Ac_Ft,
    0
  ))

# blank fields to permit revision of the data
ppet <- ppet %>% 
  mutate(Res_GW_Use_Modified       = "No",
         Res_GW_Use_Modified_Ac_Ft = NA,
         Res_GW_Use_Comment        = NA,
         Res_GW_Use_Ac_Ft = ifelse(Res_GW_Use_Modified == "Yes", 
                                   Res_GW_Use_Modified_Ac_Ft, 
                                   Res_GW_Use_Prelim_Ac_Ft))

f_progress()
f_verify_non_duplicates()


# commercial water use ----------------------------------------------------

# reported commercial uses from Petaluma
large_commercial_uses <- path(data_path, "pet/public_water_connection",
                          "Petaluma CROSSCONNECTION DATA CLEANED.xlsx") %>% 
  readxl::read_xlsx(sheet = 6) %>% 
  select(APN, replacement_commercial_use = `ac-ft/yr`) 

# add these reported large commercial uses
# ppet <- ppet %>% 
#   left_join(large_commercial_uses) %>% 
#   mutate(Commercial_W_Use_Assessor_Ac_Ft = ifelse(
#     !is.na(replacement_commercial_use),
#     replacement_commercial_use,
#     Commercial_W_Use_Assessor_Ac_Ft
#   )) %>% 
#   select(-replacement_commercial_use)
# keep assessor use codes for updated commercial values. USED BELOW.
ppet <- ppet %>%
  left_join(large_commercial_uses) 


# Commercial_GW_Use_Prelim_Ac_Ft is Commercial_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
ppet <- ppet %>% 
  mutate(Commercial_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No", 
    Commercial_W_Use_Assessor_Ac_Ft,
    0
  ))

# blank fields to permit revision of the data
# ppet <- ppet %>% 
#   mutate(Commercial_GW_Use_Modified       = "No",
#          Commercial_GW_Use_Modified_Ac_Ft = NA,
#          Commercial_GW_Use_Comment        = NA,
#          Commercial_GW_Use_Ac_Ft = ifelse(Commercial_GW_Use_Modified == "Yes", 
#                                           Commercial_GW_Use_Modified_Ac_Ft, 
#                                           Commercial_GW_Use_Prelim_Ac_Ft))
# for commercial, check if there is a replacement_commercial_use. if there is 
# then set Commercial_GW_Use_Modified == 'yes', and 
# Commercial_GW_Use_Modified_Ac_Ft=Commercial_GW_Use_Modified_Ac_Ft
# and then set Commercial_GW_Use_Ac_Ft = replacement_commercial_use.
# otherwise set Commercial_GW_Use_Ac_Ft = Commercial_W_Use_Assessor_Ac_Ft
ppet <- ppet %>%
mutate(Commercial_GW_Use_Modified       = ifelse(!is.na(replacement_commercial_use),
                                                 "Yes","No"),
       Commercial_GW_Use_Modified_Ac_Ft = ifelse(Commercial_GW_Use_Modified=='Yes',
                                                 replacement_commercial_use, 0),
       Commercial_GW_Use_Comment        = ifelse(Commercial_GW_Use_Modified=='Yes',
                                                 'Value From Petaluma Data Large water user dat', NA),
       Commercial_GW_Use_Ac_Ft = ifelse(Commercial_GW_Use_Modified == "Yes", 
                                        Commercial_GW_Use_Modified_Ac_Ft, 
                                        Commercial_GW_Use_Prelim_Ac_Ft))  %>% 
    select(-replacement_commercial_use)

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
#Todo Remove '&Public_Water_connection=='Yes' in order remove requirement that parcel has PWC and a well

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
# et <- 4.1 

# Calculate applied water at schools from ET assuming 
# application efficiency = 0.65 (65%) from Sandoval, 2010. 
# http://watermanagement.ucdavis.edu/research/application-efficiency/
# aw <- et / (1 - 0.65) # feet
aw <- 3.5 # feet/yr from Andy Rich

lawn <- path(data_path, "general/crops/i15_LandUse_Sonoma2012_SHP/i15_LandUse_Sonoma2012.shp") %>% 
  st_read() %>% 
  filter(CLASS1 == "UL") %>% #filter to only Urban Landscape
  filter(SUBCLASS1 != "5") %>%  # urban landscape except class 5, which is non-irrigated
  filter(WATERSOURC != "1") %>%  # watersource 1 is surface water
  filter(WATERSOURC != "5") %>%  # watersource 5 is reclaimed, not excluding SW sources.
  filter(WATERSOURC != "6") %>%  # watersource 6 is recycled
  st_transform(epsg) %>% 
  st_make_valid() %>% 
  st_intersection(pet) %>% 
  select(crop_class = CLASS1)

# get lawn area per APN and recalculate area, and as before, because there many
# APN with > 1 crop, we need to summarize the data before joining!
lawn_per_apn <- st_intersection(select(ppet, APN), lawn) %>% 
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

ppet<- left_join(ppet, lawn_per_apn) %>%
  mutate(lawn_acres = ifelse(is.na(lawn_acres), 0, lawn_acres))

# School_Golf_GW_Use_prelim_Ac_Ft 
ppet <- ppet %>% 
  mutate(School_Golf_GW_Use_Prelim_Ac_Ft = 
    ifelse(str_detect(tolower(UseCode_Description), 
    "school|golf|country club|winery|cemeter|city park|county park|privately owned park|business park common area"),
    # aw*LandSizeAcres*0.5, 0),
    aw*lawn_acres, 0),)   %>%
    select(-lawn_acres)
# # following meeting with Marcus, Rob, and Andy: schools with
# # a public water connection are assumed to NOT draw from groundwater
# School_Golf_GW_Use_Prelim_Ac_Ft = 
#   ifelse(str_detect(tolower(UseCode_Description), "school") &
#            Public_Water_Connection == "Yes",
#          0, School_Golf_GW_Use_Prelim_Ac_Ft),
# # following meeting with Marcus, Rob, and Andy: golf courses with
# # NO onsite well(s) are assumed to NOT draw from groundwater
# School_Golf_GW_Use_Prelim_Ac_Ft = 
#   ifelse(str_detect(tolower(UseCode_Description), "golf|country club") &
#            Onsite_Well == "No",
#          0, School_Golf_GW_Use_Prelim_Ac_Ft)


# # school locations
# # school_codes <- filter(pson, str_detect(UseCode_Description, "SCHOOL")) %>% mapview()
# 
# # School_Golf_GW_Use_prelim_Ac_Ft 
# pson <- pson %>% 
#   mutate(School_Golf_GW_Use_Prelim_Ac_Ft = 
#            ifelse(str_detect(tolower(UseCode_Description), 
#                              "school|golf|country club"),
#                   aw*LandSizeAcres*0.5, 0),
#          # following meeting with Marcus, Rob, and Andy: schools with
#          # a public water connection are assumed to NOT draw from groundwater
#          School_Golf_GW_Use_Prelim_Ac_Ft = 
#            ifelse(str_detect(tolower(UseCode_Description), "school") &
#                     Public_Water_Connection == "Yes",
#                   0, School_Golf_GW_Use_Prelim_Ac_Ft),
#          # following meeting with Marcus, Rob, and Andy: golf courses with
#          # NO onsite well(s) are assumed to NOT draw from groundwater
#          School_Golf_GW_Use_Prelim_Ac_Ft = 
#            ifelse(str_detect(tolower(UseCode_Description), "golf|country club") &
#                     Onsite_Well == "No",
#                   0, School_Golf_GW_Use_Prelim_Ac_Ft)
#   ) 
#   

print(colnames((ppet)))
####
print('adding school stuff')
# Code to use to incorporate surface water/recycled water uses.
# calculate groundwater School_Golf_GW_Use_Prelim_Ac_Ft water use
ppet <- ppet %>%
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
print('modifying school stuff')
print(colnames((ppet)))
# if a parcel receives more water from surface and recycled sources
# than estimated demand, the calculated groundwater use is negative, so
# we coerce this to zero
ppet <- ppet %>%
  mutate(School_Golf_GW_Use_Prelim_Ac_Ft = ifelse(
    School_Golf_GW_Use_Prelim_Ac_Ft < 0, 0, School_Golf_GW_Use_Prelim_Ac_Ft))

print('modifying School_Golf_Surface_Recycled_Use_Ac_Ft')
# if a parcel receives more water from surface and recycled sources
# than estimated demand, then set the
# total School_Golf_Surface_Recycled_Use_Ac_Ft = School_Golf_GW_Use_Prelim_Ac_Ft
ppet <- ppet %>%
  mutate(School_Golf_Surface_Recycled_Use_Ac_Ft = ifelse(
    School_Golf_Surface_Recycled_Use_Ac_Ft > school_golf_gw_demand,
    school_golf_gw_demand, School_Golf_Surface_Recycled_Use_Ac_Ft))
####

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
# APN with > 1 crop, we need to summarize the data before joining!
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

# applied water (in acre feet per acre) per crop, for references, see Rob
# Pennington's Sheet path(data_path, "general/crop_water_use/Crop Type Vs Water Use v3.xlsx")
# and email 2022-01-10 Re: Ongoing technical question log
crop_applied_water <- tibble(
  crop_class = c("Citrus_and_Subtropical", "Deciduous_Fruit_and_Nuts", 
                 "Truck_and_Berry_Crops", "Vine", "Grain", "Pasture", "X"),
  applied_af_acre = c(1.85, 1.83, 1.78, 0.6, 0, 0.04, 0))

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
    #calculate Ag_Surface_Recycled_Actual_Use_Ac_Ft
    Ag_Surface_Recycled_Actual_Use_Ac_Ft = Surface_Water_Use_Ac_Ft + Recycled_Water_Use_Ac_Ft,
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

# if a parcel receives more water from surface and recycled sources
# than estimated demand, then set the
# total School_Golf_Surface_Recycled_Use_Ac_Ft = School_Golf_GW_Use_Prelim_Ac_Ft
ppet <- ppet %>%
  mutate(Ag_Surface_Recycled_Actual_Use_Ac_Ft = ifelse(
    Ag_Surface_Recycled_Actual_Use_Ac_Ft > Water_Use_Ag_Rate_Ac_Ft,
    Water_Use_Ag_Rate_Ac_Ft, Ag_Surface_Recycled_Actual_Use_Ac_Ft))

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
cat("Complete PET.\n")
