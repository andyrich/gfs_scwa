# SRP database: script to generate the SRP database

library(tidyverse)
library(sf)
library(here)
library(fs)


# load data ---------------------------------------------------------------

# preprocessed spatial parcels from Sonoma Co parcels
p <- read_rds(path(data_path, "data_output/srp_parcel.rds"))

# GUIDE data
g <- readxl::read_xlsx(path(data_path, "srp/parcel/Santa Rosa Plain GSA Qualified Parcel List 2021March9.xlsx"))

# final fields to use
fields <- readxl::read_xlsx(path(data_path, "srp/parcel/Proposed update to Data Dictionary April 2021.xlsx"), 
                            sheet = 3, range = cellranger::cell_cols("C")) %>% 
  setNames("name") %>% 
  filter(!str_detect(name, " ")) %>% 
  pull(name)


# fields to keep, add, remove ---------------------------------------------

done <- fields[fields %in% colnames(g)]
add  <- fields[!fields %in% colnames(g)]
rem  <- colnames(g)[!colnames(g) %in% fields]
