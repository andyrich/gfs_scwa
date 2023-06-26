library(tidyverse)
library(fs)
library(here)
library(sf)
library(zoo)

data_path <- Sys.getenv("DATA_PATH")
# epsg <- as.numeric(Sys.getenv("EPSG"))


# this script reads all of the ewrims annual reports. it filters reported water use > than face value
# then it joins with the POD_factors to assign surface amounts to each parcel
# date 05/16/2023
# author arich

frac<-read_csv(here(data_path,"general/ewrims/water_rights_v3/input/POD_fraction_of_water_rights.csv")) %>%
  rename(APPLICATION_NUMBER = application_number) %>%
  select(-Total_diversion)


# ddw reported data 2013-2019
l <- fs::dir_ls(path(data_path, "general/ewrims/annual_reports"), glob = "*.csv") %>%
  map(~read_delim(.x,  delim=',',escape_double=FALSE, escape_backslash=TRUE, quote="\"") %>%
          rename_with( ~ toupper(gsub("\"", "", .x, fixed = TRUE))) %>%
          select(APPLICATION_NUMBER,
          FACE_VALUE_OR_REPORTED_VALUE ,        YEAR, TOTAL_AMOUNT_USED) %>%
          mutate(across(colnames(.x), ~as.numeric(gsub("\"", "", .))))%>%
          mutate_at(vars(FACE_VALUE_OR_REPORTED_VALUE,  TOTAL_AMOUNT_USED), as.numeric))

l<-bind_rows(l)


sw <- l%>%mutate(
  TOTAL_AMOUNT_USED = if_else( is.na(TOTAL_AMOUNT_USED), 0, TOTAL_AMOUNT_USED),
                 FACE_VALUE_OR_REPORTED_VALUE = if_else( is.na(FACE_VALUE_OR_REPORTED_VALUE),0, FACE_VALUE_OR_REPORTED_VALUE)) %>%
              mutate( GSA_DIVERSION = if_else( TOTAL_AMOUNT_USED>FACE_VALUE_OR_REPORTED_VALUE, FACE_VALUE_OR_REPORTED_VALUE, TOTAL_AMOUNT_USED  )) %>%
                filter( !is.na(APPLICATION_NUMBER))

group_cols <- c("APPLICATION_NUMBER", "YEAR")

yearly<- sw %>%
  group_by(across(all_of(group_cols))) %>%
  summarize(GSA_DIVERSION = mean(GSA_DIVERSION)) %>%
  subset( (APPLICATION_NUMBER %in% frac$APPLICATION_NUMBER)) %>%
  pivot_wider(names_from = YEAR, values_from = GSA_DIVERSION)


yearly<-column_to_rownames(yearly, var = "APPLICATION_NUMBER")


# print(yearly)
#fill yearly values to the RIGHT after any reported values, keep na to left.
dfi<-yearly
c <- !is.na(yearly)
dfi[c]<-0

dfill<- t(na.locf(t(yearly), fromLast = FALSE ))

dfill[c]<-yearly[c]
yearly<-dfill

yearly<-data.frame(yearly) 


write_csv(yearly, here(data_path,"general/ewrims/water_rights_v3/ewrims_yearly_alldata.csv") )


yearly<-yearly[,(ncol(yearly)-5-1):ncol(yearly)]
yearly$MEAN_GSA_DIVERSION <- rowMeans(yearly, na.rm = TRUE) 


yearly <-rownames_to_column(yearly, var = "APPLICATION_NUMBER")

summary <- yearly %>%select(APPLICATION_NUMBER, MEAN_GSA_DIVERSION)


# write_csv(summary, here("data_output/ewrims_summary.csv"))
write_csv(yearly, here(data_path,"general/ewrims/water_rights_v3/ewrims_yearly.csv") )
write_csv(summary, here(data_path,"general/ewrims/water_rights_v3/ewrims_summary.csv") )


ewrims_final <- inner_join(summary, frac, by = 'APPLICATION_NUMBER')  %>%
  mutate(GSA_DIVERSION_FINAL = MEAN_GSA_DIVERSION * Fraction_of_Water_Right )  %>%
  group_by(APN) %>%
  summarise(GSA_DIVERSION_FINAL = sum(
    GSA_DIVERSION_FINAL, na.rm = TRUE)
  ) %>%
  ungroup()


# write_csv(ewrims_final, here("data_output/ewrims_totals.csv"))
write_csv(ewrims_final, here(data_path,"general/ewrims/water_rights_v3/ewrims_totals.csv"))


