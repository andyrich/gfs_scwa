# PET database: script to generate the PET database

library(tidyverse)
library(sf)
library(here)
library(fs)


# load data ---------------------------------------------------------------

# preprocessed spatial parcels from Sonoma Co parcels
p <- read_rds(path(data_path, "data_output/pet_parcel.rds"))

# final fields to use
fields <- readxl::read_xlsx(path(data_path, "srp/parcel/Proposed update to Data Dictionary April 2021.xlsx"), 
                            sheet = 3, range = cellranger::cell_cols("C")) %>% 
  setNames("name") %>% 
  filter(!str_detect(name, " ")) %>% 
  pull(name)
