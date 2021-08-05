# gfs

Rate and fee study (gfs) for Sonoma county GSAs: 

* Sonoma Valley  
* Santa Rosa Plain  
* Petaluma  

## Getting started

Clone this repo. Open the `gfs.RProj` file.  

Note that the project-level `.RProfile` automatically loads functions and stores environmental variables in `.Renviron` as objects in `.GlobalEnv`. View this file with `usethis::edit_r_profile(scope = "project")`.  

Configure environmental variables. The easiest way to do this is from within RStudio with the `{usethis}` package. Enter `usethis::edit_r_environ(scope = "project")` to edit the project-level `.Renviron` file, then edit the file and save with:

```R
DATA_PATH = "<local path to Dropbox data>"
EPSG = 3310
```

The `DATA_PATH` is where project data dependencies are stored on a synced Dropbox folder. On my computer it's `~/Dropbox (LWA)/data/sonoma_co_gsas_rate_fee`, so my environmental variable is: `DATA_PATH = "~/Dropbox (LWA)/data/sonoma_co_gsas_rate_fee"`. In reality, this can be anywhere, including an external drive for very large data.  

The `EPSG` is the projection used in this study that all spatial data are standardized to.  

Restart `R` for changes to take effect.  



***

Last updated by *Rich Pauloo* on 2021-08-03.  
