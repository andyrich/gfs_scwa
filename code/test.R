---
title: "R Notebook"
output: html_notebook
---;
---

```{r}

library(tidyverse)
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)
library(stringr)

dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

df = load_all_modified()



# final fields to use
fields <- path(data_path, "schema/2022_03_28_schema.xlsx") %>% 
  readxl::read_xlsx(sheet = 2, range = cellranger::cell_cols("D")) %>% 
  set_names("name") %>% 
  filter(!is.na(name)) %>% 
  pull(name) 




# fields to keep, add, remove ---------------------------------------------
done <- colnames(df)[colnames(df) %in% fields]  # cols already there
add  <- colnames(df)[!(colnames(df) %in% fields)] # cols to add
print(done)
print(add)
```

```{r}

library(tidyverse)
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)
library(stringr)

dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

data_path <- Sys.getenv("DATA_PATH")
epsg <- as.numeric(Sys.getenv("EPSG"))

# area of interest object to make helper functions work
aoi = "psrp"


r = c(
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
 "School_Golf_GW_Use_Comment"        )

cols = r[str_detect(r, "Modified|Comment")]

psrp <- read_rds(path(data_path, "data_output/srp_parcel_shelly.rds"))

for (val in cols){
  print(val)
  print(unique(st_drop_geometry(psrp[,val])))
}

# colSums(psrp[,cols], na.rm=TRUE)
```

```{r}

#drop the following columns
psrp <- select(psrp, -c(
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
  "School_Golf_GW_Use_Comment"))
```

``` {library(tidyverse)}
library(sf)
library(here)
library(fs)
library(tidylog, warn.conflicts = FALSE)
library(mapview)
library(rlang)

dir_ls(here("code/functions")) %>% walk(~source(.x))
f_load_dotenv() 

data_path <- Sys.getenv("DATA_PATH")
epsg <- as.numeric(Sys.getenv("EPSG"))
# print(epsg)
# library(hash)
# h <- hash() 
# # set values
# h[["1"]] <- 42
# h[["foo"]] <- "bar"
# h[["4"]] <- list(a=1, b=2)


# library(hash)
# h <- hash() 
load_modified <- function(var){
  
  # d <- path(data_path, "general/modified_values")
  # d <- dir_ls(d, glob = '*Modified.xlsx', all = FALSE)

    # mutate(Res_GW_Use_Modified       = "No",
    #      Res_GW_Use_Modified_Ac_Ft = NA,
    #      Res_GW_Use_Comment        = NA,

  

  
  print(var)

  df <-  readxl::read_xlsx( path(data_path, "general/modified_values",
                                 paste(var,  '.xlsx', sep ='') ))
  
  cols = c('APN',
           str_replace(var, "Modified", 'Ac_Ft'),
           str_replace(var, "Modified", 'Comment'))
  
  # assigning new names to the columns of the data frame
  colnames(df) <- cols
  df[[var]] = 'Yes'

  
  return(df)}

func <- function(...){
  df1 = list(...)[[1]]
  df2 = list(...)[[2]]
  xxx = merge(..., by = 'APN', all=TRUE)
  return(xxx)
}


load_all <- function(){
  
    h = c('Ag_GW_Use_Modified',
    'Commercial_GW_Use_Modified',
    'Res_GW_Use_Modified',
    'School_Golf_Modified',
    'Urban_Irrigation_Modified')
  
  df1 = load_modified(h[1])
  df2 = load_modified(h[2])
  df3 = load_modified(h[3])
  df4 = load_modified(h[4])
  df5 = load_modified(h[5])
  
  df = list(df1, df2, df3, df4, df5)
  
  dfall = Reduce( func, df)
  print(colnames(dfall))
  
  return(dfall)
}

load_all()
```

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

```{r}
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
print(epsg)

# parcels from public sonoma county data portal
# https://gis-sonomacounty.hub.arcgis.com/pages/data
parcel <- st_read(path(data_path, "general/parcel/CDR_PARCEL_PUB_SHP_vw.shp") ) %>%
    mutate(  LandSizeParcelAcres = LndSzSF/43560.,) %>%
    select(c('APN','LandSizeParcelAcres')) %>%
    st_drop_geometry()


# # preprocessed spatial parcels from Sonoma Co parcels
# psrp <- read_rds(path(data_path, "data_output/srp_parcel_shelly.rds"))
# 
print(colnames(parcel))
```

'''

''' Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
