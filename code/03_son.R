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
fields <- readxl::read_xlsx(path(data_path, "srp/parcel/Proposed update to Data Dictionary April 2021.xlsx"), 
                            sheet = 3, range = cellranger::cell_cols("C")) %>% 
  setNames("name") %>% 
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


# rename existing fields --------------------------------------------------

# stage columns from the parcel database, renaming what we can
pson %>% 
  rename(
    LandSizeAcres = LndSzAcre,
    UseCode_Description = UseCDesc,
    UseCode_Category = UseCType,
    CurrentOwnerName = NA, 
    MailingAddress1 = MailAdr1,
    MailingAddress2 = MailAdr2,
    MailingAddress3 = MailAdr3,
    MailingAddress4 = MailAdr4,
    Recycled_Water_Connection = NA,
    Recycled_Water_Use_Ac-Ft = NA,
    Surface_Water_Connection = NA,
    Surface_Water_Use_Ac-Ft = NA,
    Active_Well = NA,
    Shared_Well = NA,
    Shared_Well_APN = NA,
    Well_Records_Available = NA,
    Onsite_Well = NA,
    Urban_well = NA,
    CA_DrinkingWater_SvcArea_Name = NA,
    CA_DrinkingWater_SvcArea_Within = NA,
    Public_Wat_Connection_Modified = NA,
    Public_Wat_Connection = NA,
    Water_Source_Comment = NA,
    Water_Use_Residential_Rate_Ac-Ft = NA,
    Residential_Confidence = NA,
    Residential_GW_Use_Assessor_Ac-Ft = NA,
    Res_GW_Use_Modified = NA,
    Res_GW_Use_Modified_Ac_Ft = NA,
    Res_GW_Use_Ac-Ft = NA,
    Res_GW_Use_Comment = NA,
    Water_Use_Commercial_Rate_Ac-Ft = NA,
    Commercial_Confidence = NA,
    Commercial_GW_Use_Assessor_Ac-Ft = NA,
    Commercial_GW_Use_Modified = NA,
    Commercial_GW_Use_Modified_Ac-Ft = NA,
    Commercial_GW_Use_Ac-Ft = NA,
    Commercial_GW_Use_Comment = NA,
    Urban_Irrigation_GW_Use_Prelim_Ac-Ft = NA,
    Urban_Irrigation_Modified = NA,
    Urban_Irrigation_Modified_Ac-Ft = NA,
    Urban_Irrigation_GW_Use_Ac-Ft = NA,
    Urban_Irrigation_GW_Use_Comment = NA,
    School_Golf_GW_Use_prelim_Ac-Ft = NA,
    School_Golf_Modified = NA,
    School_Golf_Modified_Ac-Ft = NA,
    School_Golf_GW_Use_Ac-Ft = NA,
    School_Golf_GW_Use_Comment = NA,
    Grain_Area_Ac = NA,
    Vine_Area_Ac = NA,
    Truck_and_Berry_Crops_Area_Ac = NA,
    Deciduous_Fruit_and_Nuts_Area_Ac = NA,
    Citrus_and_Subtropical_Area_Ac = NA,
    Cannabis_Outdoor_Area_Ac = NA,
    Cannabis_Indoor_Area_Ac = NA,
    Pasture_Area_Ac = NA,
    Grain_rate = NA,
    Vine_rate = NA,
    Truck_and_Berry_Crops_rate = NA,
    Deciduous_Fruit_and_Nuts_rate = NA,
    Citrus_and_Subtropical_rate = NA,
    Cannabis_Outdoor_rate = NA,
    Cannabis_Indoor_rate = NA,
    Pasture_rate = NA,
    Total_Crop_Area_prelim_Ac = NA,
    Total_Crop_Area_Ac = NA,
    Water_Use_Ag_Rate_Ac-Ft = NA,
    Ag_GW_Use_GIS_Ac-Ft = NA,
    Ag_GW_Use_Modified = NA,
    Ag_GW_Use_Modified_Ac_Ft = NA,
    Ag_GW_Use_Ac-Ft = NA,
    Ag_GW_Use_Comment = NA,
    Residential_Water_Use_Determination = NA,
    Commercial_Water_Use_Determination = NA,
    Urban_Landscape_Irrigation_Water_Use_Determination = NA,
    Ag_Irrigation_Water_Use_Determination = NA,
    Recycled_Water_Use_Determination = NA,
    Surface_Water_Use_Determination = NA,
    School_GolfCourse_Water_Use_Determination = NA,
    Total_Groundwater_Use_Ac-Ft = NA,
    Jurisdiction = NA,
    Situs_Address = NA
  )


# basin boundary parcels --------------------------------------------------

# Does the parcel overlap SRP, Petaluma, or Sonoma Valley basins?
# we expect SON overlaps only with PET based on the map below
# mapview::mapview(list(son, pet, srp)) + mapview::mapview(p) 

# parcels that intersect multiple basins have duplicate APN across databases
boundary_parcels <- pson$APN[pson$APN %in% ppet$APN]

pson <- pson %>% 
  mutate(
    Basin_Boundary_Parcel = ifelse(APN %in% boundary_parcels,
                                   "Petaluma Valley", NA),
    # area of the total APN that spans GSAs
    Intersect_GSA_Bndry_Sum_Acres = ifelse(APN %in% boundary_parcels,
                                           LndSzAcre, NA),
    # area of the bisected parcels within the GSA 
    LndSzAcre = ifelse(APN %in% boundary_parcels,
                       as.numeric(units::set_units(st_area(geometry), acres)), 
                       LndSzAcre),
    # proportion of the APN in this GSA, used to assign a GSA
    area_prop_apn = LndSzAcre / Intersect_GSA_Bndry_Sum_Acres,
    GSA_Jurisdiction_Prelim = ifelse(area_prop_apn > 0.5, 
                                     "Sonoma Valley", "Petaluma Valley"),
    GSA_Jurisdiction_Modified = NA,
    GSA_Jurisdiction_Mod_Value = NA, 
    GSA_Jurisdiction = NA
    ) %>% 
  select(-area_prop_apn)
