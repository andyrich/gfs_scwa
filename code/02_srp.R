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

# remove the test values from the modified tables:
remove_test <- TRUE

# delete complete DBs
gjson_out <- path(data_path, "data_output/srp_parcel_complete.rds")
print('deleting...')
if(file_exists(gjson_out)) file_delete(gjson_out)

gw_use_rate = 40.00 #$ per AF
unsub_gw_sub_rate = 40. #unsubsidized rate

accessor_key_path <- path(data_path, "general", "water_use_by_accessor_code",
                          "Final 2022 Water Use from Assessor Land Use Code.xlsx")


# load data ---------------------------------------------------------------


# preprocessed spatial parcels from Sonoma Co parcels
# pson <- read_rds(path(data_path, "data_output/son_parcel.rds"))
# ppet <- read_rds(path(data_path, "data_output/pet_parcel.rds"))
parcel <- read_rds(path(data_path, "data_output/srp_parcel.rds"))
cat("Loaded preprocedded spatial parcels from Sonoma County.\n")


fields <- get_schema_fields(data_path)
fields <- c(fields, "UseCode", 'edge') # add use code and drop it later

# GSA spatial data
b118_path <- path(data_path, "general/b118/i08_B118_v6-1.shp") 

# gsa spatial data: petaluma, sonoma valley, santa rosa plain
# son <- f_load_b118_basin(b118_path, "NAPA-SONOMA VALLEY - SONOMA VALLEY")
# pet <- f_load_b118_basin(b118_path, "PETALUMA VALLEY")
srp <- f_load_b118_basin(b118_path, "SANTA ROSA VALLEY - SANTA ROSA PLAIN")
cat("Loaded B118 spatial boundaries per region.\n")


# fields to keep, add, remove ---------------------------------------------

done <- fields[fields %in% colnames(parcel)]  # cols already there
add  <- fields[!fields %in% colnames(parcel)] # cols to add
rem  <- colnames(parcel)[!colnames(parcel) %in% fields] # cols to remove
rem  <- rem[-length(rem)] # don't remove the geometry column


# remove fields -----------------------------------------------------------


# parcel and contact info -------------------------------------------------
parcel <-parcel_contact(parcel)

f_progress()


# remove fields that should not be in the final database
parcel <- parcel %>% select(-all_of(rem))
cat("Removed", length(rem), "fields from parcel database.\n   ",
    paste(rem, collapse = "\n    "))

nmissing<-check_use_codes(parcel)



# ## water service areas ----------------------------------------------------


# # ad hoc cleaning post-shelly's work --------------------------------------



# basin boundary parcels --------------------------------------------------

# Does the parcel overlap SRP, Petaluma, or Sonoma Valley basins?
# we expect SON overlaps only with PET, see map below:
# mapview::mapview(list(son, pet, srp))

# # parcels that intersect multiple basins have duplicate APN across databases
# boundary_parcels <- psrp$APN[psrp$APN %in% ppet$APN]

parcel <- fill_parcel_info(parcel, 'Santa Rosa Plain')


# overwrite all of the above work, have default be that basin is Santa Rosa Plain Will
# be recalculated at combine_db
parcel <- parcel %>% 
  mutate(GSA_Jurisdiction_Prelim = "Santa Rosa Plain",
         Basin_Boundary_Parcel = edge)%>% select(-edge)

nmissing<-check_use_codes(parcel,nmissing)

f_progress()


### add parcel land size


# water sources -----------------------------------------------------------

# there are many sources (inputs) of water to each parcel, and here
# we account for them based on water input type (e.g., recycled, diversion)

## recycled water ---------------------------------------------------------
# load delivery data from recycled water treatment plants

parcel$Recycled_Water_Use_Ac_Ft <- NULL


## recycled water ---------------------------------------------------------
# load delivery data from recycled water treatment plants

# recycled water delivered to parcels. from SCI 11/1/2022
recy <- path(data_path, 
             "srp/recycled_water/RW_Output_7_10_23.csv") %>% 
  read_csv() %>% 
  mutate(APN = str_remove(PARCEL, '-000'))  %>%
  rename(Recycled_Water_Use_Ac_Ft = RW_AF_23) %>%
  filter(Recycled_Water_Use_Ac_Ft>0) %>%
  select(-PARCEL)

# add recycled water parcels to parcel data
parcel <- left_join(parcel, recy, by = "APN") %>%
  mutate(Recycled_Water_Connection = ifelse(
    !is.na(Recycled_Water_Use_Ac_Ft), "Yes", "No"),
    Recycled_Water_Use_Ac_Ft = replace_na(Recycled_Water_Use_Ac_Ft,0))


nmissing<-check_use_codes(parcel,nmissing)

print('here is the sum of the recycled water before adding modified')
print(parcel %>%
          st_drop_geometry() %>%
          select(Recycled_Water_Use_Ac_Ft)  %>%
          colSums(na.rm = TRUE))

parcel <- add_recycled_water_modified(parcel, remove_test)

parcel <- add_recycled_water_connection_modified(parcel, remove_test)

print('here is the sum of the recycled water after adding modified')
print(parcel %>%
        st_drop_geometry() %>%
        select(Recycled_Water_Use_Ac_Ft)  %>%
        colSums(na.rm = TRUE))

nmissing<-check_use_codes(parcel,nmissing)

f_progress()


## surface water connection -----------------------------------------------
# # read ewrims data, filter to SON, transform, select relevant cols

ewrims_key <- f_load_surface_water(data_path)




# #remove columns Surface_Water_Connection and Surface_Water_Use_Ac_Ft
# psrp <- subset(psrp, select = -c(Surface_Water_Use_Ac_Ft, Surface_Water_Connection))

parcel <- left_join(parcel, ewrims_key) %>%
  mutate(Surface_Water_Connection = ifelse(
    !is.na(Surface_Water_Use_Ac_Ft) & Surface_Water_Use_Ac_Ft > 0,
    "Yes", "No"))

parcel <- add_surface_water_modified(parcel, remove_test)

parcel <- add_surface_water_connection_modified(parcel, remove_test)
nmissing<-check_use_codes(parcel,nmissing)


f_progress()
f_verify_non_duplicates()
print('done loading surface water data')
# print(unique(st_drop_geometry(psrp[,c('Surface_Water_Use_Ac_Ft')])))


# ## wells ------------------------------------------------------------------
# # Sonoma county wells - deduplicate
scwells <- add_wells(parcel)
parcel <-calc_wells(parcel, scwells)

f_progress()
f_verify_non_duplicates()

nmissing<-check_use_codes(parcel,nmissing)

parcel <- parcel %>% replace_Onsite_Well_modified( remove_test= remove_test) %>%
  replace_Well_Records_Available_modified(remove_test= remove_test) %>%
  replace_shared_well_APN_modified(remove_test= remove_test) %>%
  replace_shared_well_modified(remove_test= remove_test) %>%
  replace_active_well_modified(remove_test= remove_test)

nmissing<-check_use_codes(parcel,nmissing)
## water service areas ----------------------------------------------------

# water service areas in SON


# # rm Shelly's work


# # add water service areas to parcel data, first need to summarize data
# # to avoid duplicates where a parcel falls within more than one water system!



wsa_key <- get_wsa_key(parcel, srp)

parcel <- left_join(parcel, wsa_key) %>%
  mutate(CA_DrinkingWater_SvcArea_Within =
           ifelse(!is.na(CA_DrinkingWater_SvcArea_Name), "Yes", "No"))

f_verify_non_duplicates()

nmissing<-check_use_codes(parcel,nmissing)
# add explicit connection data from Petaluma, Sebastapol, Sonoma, Penngrove,
# and Valley of the Moonb WD - from Shelly on 2022-01-04, Email Subject:
# # if an explicit connection is present, ensure it is represented


parcel <-add_public_water_connection(parcel)
nmissing<-check_use_codes(parcel,nmissing)

nmissing<-check_use_codes(parcel,nmissing)
parcel <-pwc_use_code_fix(parcel)


# add public water connections for modified APNs: 
# TODO: verify this should  be here. it is already incorporated above. Maybe being re-done
# so that pwc_assessor work is overwrittine


f_progress()
f_verify_non_duplicates()


# urban wells -------------------------------------------------------------




# public water system reported use from SWRCB -----------------------------



# residential water use ---------------------------------------------------

# The Urban Residential Groundwater user class represents residential properties
# in areas served by water service providers that also have a well on the 
# property... Raftelis and Staff assumed that these wells would primarily be 
# used for irrigation purposes... it is assumed that Urban Residential Groundwater 
# Users extract on average 0.1 AF per parcel per year for irrigation purposes.

# Res_W_Use_Assessor_Ac_Ft = Water use rate based off assessor use code


res_use_accessor_key <- readxl::read_xlsx(accessor_key_path,
                                          sheet = 2) %>%
  janitor::clean_names() %>%
  mutate(UseCode = str_pad(land_use_code, 4, "left", "0")) %>%
  select(UseCode,
         Res_W_Use_Assessor_Ac_Ft = residential_use,
         Commercial_W_Use_Assessor_Ac_Ft = commercial_industrial_misc_use)

# # add Residential and Commercial Water Use based on Accessor Code
# psrp <- psrp %>% 
#   select(-all_of(c("Res_W_Use_Assessor_Ac_Ft",
#                    "Commercial_W_Use_Assessor_Ac_Ft")))


parcel <- replace_use_code(parcel, remove_test)

nmissing<-check_use_codes(parcel,nmissing)

parcel <- left_join(parcel, res_use_accessor_key)

# Res_GW_Use_Prelim_Ac_Ft is Res_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
parcel <- parcel %>%
  mutate(Res_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No", 
    Res_W_Use_Assessor_Ac_Ft,
    0
  ))

# load modified fields
parcel <- join_with_modified(parcel, remove_test)


# blank fields to permit revision of the data
parcel <- parcel %>%
  mutate(
         Res_GW_Use_Ac_Ft = ifelse(Res_GW_Use_Modified == "Yes",
                                   Res_GW_Use_Modified_Ac_Ft,
                                   Res_GW_Use_Prelim_Ac_Ft))
# 
# f_progress()
# f_verify_non_duplicates()

nmissing<-check_use_codes(parcel,nmissing)

# commercial water use ----------------------------------------------------


# Commercial_GW_Use_Prelim_Ac_Ft is Commercial_W_Use_Assessor_Ac_Ft if
# there's no public water connection, otherwise, it's 0
parcel <- parcel %>%
  mutate(Commercial_GW_Use_Prelim_Ac_Ft = ifelse(
    Public_Water_Connection == "No",
    Commercial_W_Use_Assessor_Ac_Ft,
    0
  ))
 
# # blank fields to permit revision of the data
parcel <- parcel %>%
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

parcel <- load_urban_wells(data_path, parcel)
parcel <- replace_urban_well_modified(parcel, remove_test)

parcel <- calc_urban_irrigation(parcel)
parcel <- add_urban_irrigation_modified(parcel, remove_test)

nmissing<-check_use_codes(parcel,nmissing)


# # schools and golf courses ------------------------------------------------

parcel <- calculate_lawn(parcel, srp)


parcel <- school_golf_calc(parcel)
f_progress()
f_verify_non_duplicates()


# crop water use ----------------------------------------------------------
# Raftelis report document pgs 25-27

parcel <-add_cannabis_modified(parcel, remove_test)

crop <-load_land_use(parcel)
parcel <-calc_crop_water_use(parcel, crop)

f_verify_non_duplicates()



# calculate groundwater ag water use
parcel <- calc_ag_use(parcel)

f_progress()
f_verify_non_duplicates()


# determination for GIS survey --------------------------------------------
parcel <- gis_survey_determination(parcel)

f_progress()
f_verify_non_duplicates()


# total calculations ------------------------------------------------------
parcel <- total_use_calc(parcel, gw_use_rate)


# final manual tests ------------------------------------------------------

# sanity check: cols that still need to be added
add[!add %in% colnames(parcel)]

f_progress()
f_verify_non_duplicates()


# write complete parcel data ----------------------------------------------

parcel %>%
  write_rds(path(data_path, "data_output/srp_parcel_complete.rds"))
cat("Complete SRP.\n")

