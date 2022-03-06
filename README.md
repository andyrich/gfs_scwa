# gfs

LWA is prime to SCI on a **G**SA **F**ee and rate **S**tudy (gfs) for Sonoma County GSAs: 

* Santa Rosa Plain  
* Sonoma Valley  
* Petaluma Valley  

This repo generates the parcel database of 93 fields (including geometry) for each GSA.  


## Getting started

Clone this repo. Open the `gfs.RProj` file.  

Configure environmental variables. The easiest way to do this is from within RStudio with the `{usethis}` package. Enter `usethis::edit_r_environ(scope = "project")` to edit the project-level `.Renviron` file, then edit the file and save with:

```R
DATA_PATH = "<local path to Dropbox data>"
EPSG = 3310
```

The `DATA_PATH` is where project data dependencies are stored on a synced Dropbox folder. On my computer it's `~/Dropbox (LWA)/data/sonoma_co_gsas_rate_fee`, so my environmental variable is: `DATA_PATH = "~/Dropbox (LWA)/data/sonoma_co_gsas_rate_fee"`. In reality, this can be anywhere, including an external drive for very large data.  

Use the following link to obtain Dropbox Data Dependencies used by the code: https://www.dropbox.com/sh/5lyd9r01zh62s7w/AABcQphVCHx41e-T5vTczu7va?dl=0


The `EPSG` is the projection used in this study that all spatial data are standardized to.  

Restart `R` for changes to take effect.  


## Running the pipeline

The pipeline constructs the database for SRP, SON, and PET in that order, and then glues all data together into one massive file. To execute the pipeline:  

```
cd gfs/code
bash run.sh
```

`run.sh` strings together the sequential steps that create the database. Inspect it to see the 4 scripts it calls in order.  


## Schema

Data schema that the database builds towards was defined by Permit Sonoma and is stored at `data_path/schema/2021_11_03_schema.xlsx`.  

For methods, see `docs/memo.docx`.



***

Last updated by *Rich Pauloo* on 2022-02-08.  
Transferred to SCWA github on 2022-3-6.
