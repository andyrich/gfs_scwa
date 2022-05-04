# generate the SON database

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
aoi = "psrp"


# delete complete DBs
gjson_out <- path(data_path, "data_output/srp_parcel_complete.rds")
print('deleting...')
if(file_exists(gjson_out)) file_delete(gjson_out)

gw_use_rate = 1.00 #$ per AF

# load data ---------------------------------------------------------------


# preprocessed spatial parcels from Sonoma Co parcels
pson <- read_rds(path(data_path, "data_output/son_parcel.rds"))
ppet <- read_rds(path(data_path, "data_output/pet_parcel.rds"))
psrp <- read_rds(path(data_path, "data_output/srp_parcel_shelly.rds"))
cat("Loaded preprocedded spatial parcels from Sonoma County.\n")

# final fields to use
fields <- path(data_path, "schema/GSA Schema 20220503.xlsx") %>% 
  readxl::read_xlsx(sheet = 1, range = cellranger::cell_cols("B")) %>% 
  set_names("name") %>% 
  filter(!is.na(name)) %>% 
  pull(name)
fields <- c(fields, "UseCode", 'edge') # add use code and drop it later

# GSA spatial data
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsa spatial data: petaluma, sonoma valley, santa rosa plain
son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")
cat("Loaded B118 spatial boundaries per region.\n")


# fields to keep, add, remove ---------------------------------------------

done <- fields[fields %in% colnames(psrp)]  # cols already there
add  <- fields[!fields %in% colnames(psrp)] # cols to add
add  <- add[-which(add == "UseCode")] # remove unneeded col
rem  <- colnames(psrp)[!colnames(psrp) %in% fields] # cols to remove
rem  <- rem[-length(rem)] # don't remove the geometry column


# remove fields -----------------------------------------------------------

# remove fields that should not be in the final database
psrp <- psrp %>% select(-all_of(rem))
cat("Removed", length(rem), "fields from parcel database.\n   ",
    paste(rem, collapse = "\n    "))

colstodrop = c(
  "Res_GW_Use_Modified",
  "Res_GW_Use_Modified_Ac_Ft",
  "Res_GW_Use_Comment",
  "Commercial_GW_Use_Modified",
  "Commercial_GW_Use_Modified_Ac_Ft",
  "Commercial_GW_Use_Comment",
  "Urban_Irrigation_Modified",
  "Urban_Irrigation_Modified_Ac_Ft",
  "Urban_Irrigation_GW_Use_Comment",
  "School_Golf_Modified",
  "School_Golf_Modified_Ac_Ft",
  "School_Golf_GW_Use_Comment")

# if you want to see if these fields should be dropped, uncomment here:
# for (val in cols){
#   print(val)
#   print(unique(st_drop_geometry(psrp[,val])))
# }
# all fields are either null or "No' for Modified

#drop the following columns
psrp <- select(psrp, -colstodrop)

# ad hoc cleaning post-shelly's work --------------------------------------

psrp <- psrp %>% 
  mutate(
    # fix NA water systems to match SON and PET
    CA_DrinkingWater_SvcArea_Name = ifelse(
      CA_DrinkingWater_SvcArea_Name == "Not In Public Water System",
      NA,
      CA_DrinkingWater_SvcArea_Name)
  ) %>% 
  separate(UseCode_Description, 
           into = c("UseCode", "UseCode_Description"), 
           sep = " ", 
           extra = "merge") %>% 
  # remove brackets
  mutate(UseCode_Description = str_remove_all(UseCode_Description, "\\[|\\]"))



# basin boundary parcels --------------------------------------------------

# Does the parcel overlap SRP, Petaluma, or Sonoma Valley basins?
# we expect SON overlaps only with PET, see map below:
# mapview::mapview(list(son, pet, srp))

# parcels that intersect multiple basins have duplicate APN across databases
boundary_parcels <- psrp$APN[psrp$APN %in% ppet$APN]

psrp <- psrp %>% 
  mutate(
    # Add parcel size
    LandSizeParcelAcres = LandSizeAcres,
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
                                     "Santa Rosa Plain", "Petaluma Valley"),
    GSA_Jurisdiction_Prelim = ifelse(is.na(GSA_Jurisdiction_Prelim), 
                                     "Santa Rosa Plain", GSA_Jurisdiction_Prelim),
    # intentionally left blank for clients to evaluate and populate
    GSA_Jurisdiction_Modified = NA,
    GSA_Jurisdiction_Mod_Value = NA,
    GSA_Jurisdiction = NA
  ) %>% 
  # remove intermediate vars
  select(-area_prop_apn)

# overwrite all of the above work, have default be that basin is Santa Rosa Plain Will
# be recalculated at combine_db
psrp <- psrp %>% 
  mutate(GSA_Jurisdiction_Prelim = "Santa Rosa Plain",
         Basin_Boundary_Parcel = edge)%>% select(-edge)


f_progress()


### add parcel land size
# psrp <- load_land_size(data_path, psrp)

# sanity check - requires ppet parcels in memory
# mapview(pet, alpha.regions = 0) +
#   mapview(srp, alpha.regions = 0) +
#   mapview(filter(psrp, Basin_Boundary_Parcel == "Yes") %>% 
#             select(APN, GSA_Jurisdiction_Prelim), 
#           zcol = "GSA_Jurisdiction_Prelim",
#           layer.name = "boundary parcel srp") +
#   mapview(filter(ppet, Basin_Boundary_Parcel == "Yes") %>% 
#             select(APN, GSA_Jurisdiction_Prelim), 
#           zcol = "GSA_Jurisdiction_Prelim",
#           layer.name = "boundary parcel pet")


# water sources -----------------------------------------------------------

# there are many sources (inputs) of water to each parcel, and here
# we account for them based on water input type (e.g., recycled, diversion)

## recycled water ---------------------------------------------------------
# load delivery data from recycled water treatment plants

# recycled water delivered to parcels in 2016 (from billy.dixon@scwa.ca.gov)
# recy <- path(data_path, 
#              "son/recycled_water/SVCSD Recycled Water Use APNs.xlsx") %>% 
#   readxl::read_xlsx(sheet = 4) %>% 
#   rename(Recycled_Water_Use_Ac_Ft = af_yr_2016)

# add recycled water parcels to parcel data
# psrp <- left_join(psrp, recy, by = "APN") %>% 
#   mutate(Recycled_Water_Connection = ifelse(
#     !is.na(Recycled_Water_Use_Ac_Ft), "Yes", "No"))
# f_progress()


## surface water connection -----------------------------------------------
# # read ewrims data, filter to SON, transform, select relevant cols
# ewrims <- path(data_path, "general/ewrims", 'water_rights_list_2021-09-23.csv') %>%
#   read_csv(col_select = c("longitude", "latitude",
#                           "face_value_amount", "county"),
#            col_types = list(
#              longitude         = "d",
#              latitude          = "d",
#              face_value_amount = "d",
#              county            = "c")) %>%
#   rename(Surface_Water_Use_Ac_Ft = face_value_amount) %>%
#   filter(county == "Sonoma" | is.na(county)) %>%
#   # remove a few rows without location data
#   filter(!is.na(latitude), !is.na(longitude),
#          !is.nan(latitude), !is.nan(longitude)) %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>%
#   st_transform(epsg) %>%
#   select(-county)
ewrims <- f_load_surface_water(data_path)

ewrims_out <- path(data_path, "general/ewrims/exported_water_rights.geojson")
if(file_exists(ewrims_out)) file_delete(ewrims_out)
st_write(ewrims, ewrims_out)
print('done writing water rights')

# add surface water use (AF/year) to parcels, but be careful, as some
# parcels have MULTIPLE ewrims points and these must be summarized
ewrims_key <- st_join(select(psrp, APN), ewrims) %>%
  st_drop_geometry() %>%
  group_by(APN) %>%
  summarise(Surface_Water_Use_Ac_Ft = sum(
    Surface_Water_Use_Ac_Ft, na.rm = TRUE)
  ) %>%
  ungroup()


#remove columns Surface_Water_Connection and Surface_Water_Use_Ac_Ft
psrp <- subset(psrp, select = -c(Surface_Water_Use_Ac_Ft, Surface_Water_Connection))

psrp <- left_join(psrp, ewrims_key) %>%
  mutate(Surface_Water_Connection = ifelse(
    !is.na(Surface_Water_Use_Ac_Ft) & Surface_Water_Use_Ac_Ft > 0,
    "Yes", "No"))
f_progress()
f_verify_non_duplicates()
print('done loading surface water data')
# print(unique(st_drop_geometry(psrp[,c('Surface_Water_Use_Ac_Ft')])))

## wells ------------------------------------------------------------------
# Sonoma county wells - deduplicate
# scwells <- path(data_path, "general", 
#                 "soco_wells/all_soco_wells_spatial.shp") %>% 
#   st_read() %>% 
#   st_transform(epsg) %>% 
#   st_intersection(select(psrp, APN)) %>%
#   add_count(APN, name = "Well_Count") %>% 
#   group_by(APN) %>% 
#   mutate(Well_Log_Nos = paste(Log_No, collapse = "; ")) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   select(APN, Well_Count, Well_Log_Nos) %>% 
#   st_drop_geometry()

# DEPRICATED: just know that more detailed well data is here
# Sonoma count well data - deduplicate
# scwells_data <- path(data_path, "general", 
#                      "soco_wells/all_sonoma_county_wells.xlsx") %>% 
#   readxl::read_xlsx() %>% 
#   select(APN:OtherObservations) %>% 
#   group_by(Log_No) %>% 
#   slice(1) %>% 
#   ungroup()

# DEPRICATED: simplified by joining spatial well data to psrp above
# combine and return vector of APNs with a well present
# scwells_apn <- left_join(scwells, scwells_data, by = "Log_No") %>% 
#   # There is a lot of WCR data we're dropping, but it's all there!
#   select(Log_No) %>% 
#   st_intersection(select(psrp, APN)) %>% 
#   pull(APN) %>% 
#   unique()

# populate database columns
# psrp <- psrp %>% 
#   left_join(scwells) %>% 
#   mutate(
#     # no well count means 0 onsite wells
#     Well_Count = ifelse(is.na(Well_Count), 0, Well_Count),
#     Active_Well = ifelse(Well_Count > 0, "Yes", "No"),
#     Shared_Well = NA, # placeholder for future review
#     Shared_Well_APN = NA, # placeholder for future review
#     Well_Records_Available = ifelse(Well_Count > 0, "Yes", "No"),
#     Onsite_Well = 
#       ifelse(Active_Well == "Yes" | Well_Records_Available == "Yes", 
#              "Yes", "No")
#   ) 
# f_progress()
# f_verify_non_duplicates()


## water service areas ----------------------------------------------------

# water service areas in SON
wsa <- path(data_path, "general", "water_system_boundaries",
            "SABL_Public_083121/SABL_Public_083121.shp") %>%
  st_read() %>%
  st_transform(epsg) %>%
  st_make_valid() %>% 
  st_intersection(srp) %>%
  select(CA_DrinkingWater_SvcArea_Name = WATER_SY_1,
         pwsid = SABL_PWSID)

# sanity check
# mapview(pet) + mapview(wsa)

# rm Shelly's work
psrp$CA_DrinkingWater_SvcArea_Name <- NULL
psrp$CA_DrinkingWater_SvcArea_Within <- NULL
psrp$Urban_Well <- NULL
psrp$Urban_Irrigation_GW_Use_Prelim_Ac_Ft <- NULL

# list of water service areas to remove
# these providers depend on explicit connection data for their Public_Water_Connection
wsa_remove = c('SEBASTOPOL, CITY OF',
               'WINDSOR, TOWN OF',
               'PENNGROVE WATER COMPANY (PUC)')

# add water service areas to parcel data, first need to summarize data
# to avoid duplicates where a parcel falls within more than one water system!
wsa_key <- st_join(psrp, wsa) %>% 
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

# print(unique(wsa_key$CA_DrinkingWater_SvcArea_Name))

# # add water service areas to parcel data, first need to summarize data
# # to avoid duplicates where a parcel falls within more than one water system!
# wsa_key <- st_join(psrp, wsa) %>% 
#   select(APN, CA_DrinkingWater_SvcArea_Name) %>% 
#   group_by(APN) %>% 
#   # for parcels with > 1 water system, combine water system names
#   mutate(CA_DrinkingWater_SvcArea_Name = paste(
#     CA_DrinkingWater_SvcArea_Name, collapse = "; ")) %>%
#   ungroup() %>% 
#   distinct() %>% 
#   st_drop_geometry() %>% 
#   select(APN, CA_DrinkingWater_SvcArea_Name) %>% 
#   # coerce character "NA" to NA
#   mutate(CA_DrinkingWater_SvcArea_Name = ifelse(
#     CA_DrinkingWater_SvcArea_Name == "NA", 
#     NA, CA_DrinkingWater_SvcArea_Name))
# 
# psrp <- left_join(psrp, wsa_key) %>% 
#   mutate(CA_DrinkingWater_SvcArea_Within = 
#            ifelse(!is.na(CA_DrinkingWater_SvcArea_Name), "Yes", "No"))

f_verify_non_duplicates()

# add explicit connection data from Petaluma, Sebastapol, Sonoma, Penngrove,
# and Valley of the Moonb WD - from Shelly on 2022-01-04, Email Subject:
# Data Revision/Addition | Permit Sonoma GIS: GSA Water Service Connection | ID APN-to-Address
windsor_path <- path(data_path, "srp", "public_water_connection",
                     "Windsor Water Service.gdb")
cat("Reading in explicit connection data for:\n",
    paste(rgdal::ogrListLayers(windsor_path), collapse = "\n "))

explicit_connections <- rgdal::ogrListLayers(windsor_path)[2] %>%
  purrr::map_df(
    ~rgdal::readOGR(dsn = windsor_path, layer = .x) %>%
      st_as_sf() %>%
      select(APN))

# if an explicit connection is present, ensure it is represented
psrp <- psrp %>%
  mutate(Public_Water_Connection = ifelse(
    APN %in% explicit_connections$APN | Public_Water_Connection == "Yes",
    "Yes", "No"))

# add public water connections for modified APNs:
apn_add_pwc <- path(data_path, "general/modified_apns.xlsx") %>% 
  readxl::read_xlsx(sheet = 1) %>% 
  pull(APN)

psrp <- psrp %>% 
  mutate(Public_Water_Connection = ifelse(
    APN %in% apn_add_pwc,
    "Yes", Public_Water_Connection)
  )

# # ensure public water connection is listed for specified Accessor Use Codes
# accessor_key_path <- path(data_path, "general", "water_use_by_accessor_code",
#                           "Water  Use from Assessor Land Use Code 8_27_2021.xlsx")
# pwc_accessor_key <- readxl::read_xlsx(accessor_key_path, 
#                                       sheet = 3, range = "B2:C27") %>% 
#   janitor::clean_names() %>% 
#   mutate(use_code = str_pad(use_code, 4, "left", "0"))
# 
# # if the parcel is within a water service area and the use code is listed, 
# # mark a public water service connection even if not explicitly listed
# psrp <- psrp %>% 
#   mutate(Public_Water_Connection = ifelse(
#     CA_DrinkingWater_SvcArea_Within == "Yes" & UseCode %in% pwc_accessor_key$use_code,
#     "Yes", Public_Water_Connection
#   )
#   )
# 
# f_progress()
# f_verify_non_duplicates()


# urban wells -------------------------------------------------------------

# explicit connection data from VOMWD (Valley of Moon water district)
# vomwd_apn_st <- path(data_path, "son", "public_water_connection", 
#                      "VOMWD Data August 2021", 
#                      "Master Location & Backflow data.xlsx") %>% 
#   readxl::read_xlsx(sheet = 1) %>% 
#   select(APN = `Parcel Number`, st_no = `Street Number`, st = `Street Name`) %>% 
#   filter(!is.na(APN)) %>% 
#   mutate(st = paste(st_no, st)) %>% 
#   select(-st_no) %>% 
#   distinct()

# get APNs of parcels with an urban well connection, see 2021-09-13 email
# from Rob Pennington for methods: VOMWD  Urban Well Use Logic
# vomwd_urban_wells <- path(data_path, "son", "public_water_connection", 
#                           "VOMWD Data August 2021", 
#                           "Master Location & Backflow data.xlsx") %>% 
#   readxl::read_xlsx(sheet = 2) %>% 
#   select(st_no = `Street Number`, st = `Street Name`,
#          well = `Well on Site`, bf = `BF Type`, notes = Notes) %>% 
#   mutate(st = paste(st_no, st)) %>% 
#   select(-st_no) %>% 
#   distinct() %>% 
#   left_join(vomwd_apn_st) %>% 
#   group_by(APN) %>% 
#   summarise(well = paste(well, collapse = " "), 
#             bf = paste(bf, collapse = " "),
#             notes = paste(notes, collapse = " ")) %>% 
#   ungroup() %>% 
#   mutate(
#     Urban_Well = ifelse(str_detect(well, "Yes"), "Yes", "No"), 
#     Urban_Well = ifelse(str_detect(notes, "HOA") & 
#                           str_detect(bf, "RP") & 
#                           str_detect(well, "NA"), 
#                         "Yes", Urban_Well)) %>% 
#   filter(Urban_Well == "Yes")

# add urban wells from VOMWD
# psrp <- psrp %>% 
#   mutate(Urban_Well = ifelse(APN %in% vomwd_urban_wells$APN, "Yes", "No"))
# 
# f_progress()
# f_verify_non_duplicates()


# public water system reported use from SWRCB -----------------------------

# wsu <- path(data_path, "general/pws_water_use",
#             "2020 EAR Water Suppy and Deilvery data for Sonoma District PWS.xlsx"
#             ) %>%
#   readxl::read_xlsx() %>%
#   select(pwsid = PwsID, name = PWSName, gw = WPAnnualGW, unit = WPUnitsofMeasure) %>%
#   filter(!is.na(unit)) %>%
#   # convert gallons and million gallons to acre-feet
#   mutate(gw_af = case_when(
#     unit == "G"  ~ gw * 3.06889e-6,
#     unit == "MG" ~ gw * 3.06889)
#   ) %>%
#   filter(gw_af > 0)
# 
# wsa <- left_join(wsa, wsu)


# residential water use ---------------------------------------------------

# The Urban Residential Groundwater user class represents residential properties
# in areas served by water service providers that also have a well on the 
# property... Raftelis and Staff assumed that these wells would primarily be 
# used for irrigation purposes... it is assumed that Urban Residential Groundwater 
# Users extract on average 0.1 AF per parcel per year for irrigation purposes.

# res_rate_urban <- 0.1 # acre feet/year for urban parcels with a well
# 
# psrp <- psrp %>% 
#   mutate(
#     # case 1: urban residential: within water system with onsite well
#     Res_GW_Use_Prelim_Ac_Ft = 
#       ifelse(Public_Water_Connection == "Yes" & 
#                Onsite_Well         == "Yes", 
#              res_rate_urban, NA)
#   )

# Res_W_Use_Assessor_Ac_Ft = Water use rate based off assessor use code
# Dependency provided by Rob P on 2021-11-16
#accessor_key_path <- path(data_path, "general", "water_use_by_accessor_code",
#                          "Water  Use from Assessor Land Use Code 8_27_2021.xlsx")
accessor_key_path <- path(data_path, "general", "water_use_by_accessor_code",
                          "Final 2022 Water  Use from Assessor Land Use Code.xlsx")
res_use_accessor_key <- readxl::read_xlsx(accessor_key_path,
                                          sheet = 2) %>%
  janitor::clean_names() %>%
  mutate(UseCode = str_pad(land_use_code, 4, "left", "0")) %>%
  select(UseCode,
         Res_W_Use_Assessor_Ac_Ft = residential_use,
         Commercial_W_Use_Assessor_Ac_Ft = commercial_industrial_misc_use)

# add Residential and Commercial Water Use based on Accessor Code
psrp <- psrp %>% 
  select(-all_of(c("Res_W_Use_Assessor_Ac_Ft",
                   "Commercial_W_Use_Assessor_Ac_Ft")))
psrp <- left_join(psrp, res_use_accessor_key)

# Res_GW_Use_Prelim_Ac_Ft is Res_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
psrp <- psrp %>%
  mutate(Res_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No", 
    Res_W_Use_Assessor_Ac_Ft,
    0
  ))

# load modified fields
psrp <- join_with_modified(psrp)

# blank fields to permit revision of the data
psrp <- psrp %>%
  mutate(
         Res_GW_Use_Ac_Ft = ifelse(Res_GW_Use_Modified == "Yes",
                                   Res_GW_Use_Modified_Ac_Ft,
                                   Res_GW_Use_Prelim_Ac_Ft))
# 
# f_progress()
# f_verify_non_duplicates()


# commercial water use ----------------------------------------------------


# Commercial_GW_Use_Prelim_Ac_Ft is Commercial_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
psrp <- psrp %>%
  mutate(Commercial_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No",
    Commercial_W_Use_Assessor_Ac_Ft,
    0
  ))
 
# # blank fields to permit revision of the data
psrp <- psrp %>%
  mutate(
         Commercial_GW_Use_Ac_Ft          = ifelse(
           Commercial_GW_Use_Modified == "Yes",
           Commercial_GW_Use_Modified_Ac_Ft,
           Commercial_GW_Use_Prelim_Ac_Ft))
# 
# f_progress()
# f_verify_non_duplicates()


# urban irrigation --------------------------------------------------------

# all urban wells are set to "No" for now, per instructions in the data 
# dictionary: '(default value is "No")... Parcel sets from GUIDE Survey or 
# Cities will be used in the future to set to "Yes"'

psrp <- load_urban_wells(data_path, psrp)

# if there’s an urban well & public water connection, assume 0.1 AF/yr, else 0
psrp <- psrp %>%
  mutate(
    Urban_Irrigation_GW_Use_Prelim_Ac_Ft = ifelse(
      Urban_Well == "Yes" & Public_Water_Connection == "Yes", 0.1, 0))
# 
# # blank fields to permit revision of the data
psrp <- psrp %>%
  mutate(
         Urban_Irrigation_GW_Use_Ac_Ft   = ifelse(
           Urban_Irrigation_Modified == "Yes",
           Urban_Irrigation_GW_Use_Modified_Ac_Ft,
           Urban_Irrigation_GW_Use_Prelim_Ac_Ft))
# 
# f_progress()
# f_verify_non_duplicates()


# schools and golf courses ------------------------------------------------

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
  st_intersection(srp) %>% 
  select(crop_class = CLASS1)

# get lawn area per APN and recalculate area, and as before, because there many
# APN with > 1 crop, we need to summarize the data before joining!
lawn_per_apn <- st_intersection(select(psrp, APN), lawn) %>% 
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

psrp<- left_join(psrp, lawn_per_apn) %>%
  mutate(lawn_acres = ifelse(is.na(lawn_acres), 0, lawn_acres))

#if no public connection, remove lawns <0.2 acres
psrp<- psrp %>%
  mutate(lawn_acres = ifelse(Public_Water_Connection == 'No' & lawn_acres<0.2,
                             0, lawn_acres))

#if there is public connection, remove lawns <0.5 acres
psrp<- psrp %>%
  mutate(lawn_acres = ifelse(Public_Water_Connection == 'Yes' & lawn_acres<0.5,
                             0, lawn_acres))

# School_Golf_GW_Use_prelim_Ac_Ft 
psrp <- psrp %>% 
  mutate(School_Golf_GW_Use_Prelim_Ac_Ft = 
           aw*lawn_acres)   %>%
  select(-lawn_acres)

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


####
print('adding school stuff')
# Code to use to incorporate surface water/recycled water uses.
# calculate groundwater School_Golf_GW_Use_Prelim_Ac_Ft water use
psrp <- psrp %>%
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
psrp <- psrp %>%
  mutate(School_Golf_GW_Use_Prelim_Ac_Ft = ifelse(
    School_Golf_GW_Use_Prelim_Ac_Ft < 0, 0, School_Golf_GW_Use_Prelim_Ac_Ft))
print('modifying School_Golf_Surface_Recycled_Use_Ac_Ft')
# if a parcel receives more water from surface and recycled sources
# than estimated demand, then set the
# total School_Golf_Surface_Recycled_Use_Ac_Ft = School_Golf_GW_Use_Prelim_Ac_Ft
psrp <- psrp %>%
  mutate(School_Golf_Surface_Recycled_Use_Ac_Ft = ifelse(
    School_Golf_Surface_Recycled_Use_Ac_Ft > school_golf_gw_demand,
    school_golf_gw_demand, School_Golf_Surface_Recycled_Use_Ac_Ft)) %>%
  select(-school_golf_gw_demand)


# blank fields to permit revision of the data
psrp <- psrp %>% 
  mutate(
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
  st_intersection(srp) %>% 
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
crops_per_apn <- st_intersection(select(psrp, APN), crop) %>% 
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
# only one row per APN, and is in wide format ready for the join to `psrp`
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
psrp <- psrp %>% 
  # remove columns before the join
  select(-ends_with("Area_Ac")) %>% 
  select(-ends_with("_Rate")) %>% 
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
psrp <- psrp %>% 
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
psrp <- psrp %>% 
  mutate(Ag_GW_Use_GIS_Ac_Ft = ifelse(
    Ag_GW_Use_GIS_Ac_Ft < 0, 0, Ag_GW_Use_GIS_Ac_Ft))

# if a parcel receives more water from surface and recycled sources
# than estimated demand, then set the
# total Ag_Surface_Recycled_Actual_Use_Ac_Ft = Ag_GW_Use_GIS_Ac_Ft
psrp <- psrp %>%
  mutate(Ag_Surface_Recycled_Actual_Use_Ac_Ft = ifelse(
    Ag_Surface_Recycled_Actual_Use_Ac_Ft > Water_Use_Ag_Rate_Ac_Ft,
    Water_Use_Ag_Rate_Ac_Ft, Ag_Surface_Recycled_Actual_Use_Ac_Ft))

# No Idle Acres to start - this is reported
psrp <- psrp %>% mutate(Idle_Ac = 0)



# blank fields to permit revision of the data
psrp <- psrp %>% 
  mutate(
         Ag_GW_Use_Ac_Ft = ifelse(Ag_GW_Use_Modified == "Yes", 
                                  Ag_GW_Use_Modified_Ac_Ft, 
                                  Ag_GW_Use_GIS_Ac_Ft)) 

f_progress()
f_verify_non_duplicates()


# determination for GIS survey --------------------------------------------

psrp <- psrp %>% 
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
psrp <- psrp %>% 
  mutate(across(ends_with("GW_Use_Ac_Ft"), ~ifelse(is.na(.x), 0, .x))) 

# calculate total groundwater use
psrp <- psrp %>% 
  mutate(
    Total_Groundwater_Use_Ac_Ft =
      Res_GW_Use_Prelim_Ac_Ft + 
      Commercial_GW_Use_Prelim_Ac_Ft + 
      Ag_GW_Use_Ac_Ft + 
      School_Golf_GW_Use_Ac_Ft + 
      Urban_Irrigation_GW_Use_Ac_Ft,
    Total_Groundwater_Use_PublicView = NA,
    Parcel_fee = Total_Groundwater_Use_Ac_Ft*gw_use_rate
  )

# additional columns

# Jurisdiction 
# Situs_Address 



# drop UseCode ------------------------------------------------------------

psrp <- psrp %>% select(-UseCode)

# final manual tests ------------------------------------------------------

# sanity check: cols that still need to be added
add[!add %in% colnames(psrp)]

f_progress()
f_verify_non_duplicates()


# write complete parcel data ----------------------------------------------

psrp %>% 
  write_rds(path(data_path, "data_output/srp_parcel_complete.rds"))
cat("Complete SRP.\n")

