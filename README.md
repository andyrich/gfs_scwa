![](docs/sgma-logo.png)


# gfs

**G**SA **F**ee and rate **S**tudy (gfs) for Sonoma County GSAs: 

* Santa Rosa Plain  
* Sonoma Valley  
* Petaluma Valley  

This repo generates the parcel database of 93 fields (including geometry) for each GSA.  


# Getting started

Clone this repo. Open the `gfs.RProj` file.  

## Data Structure
I have set up the various folders in the following way. I have added the sharepoint data folder sonoma_co_gsas_rate_fee as a OneDrive folder on my machine. The sharepoint is maintained by West Yost. Data is copied from the sharepoint folder into the workspace (ie DATA_PATH) to ensure that the datasets used in the GUIDE code are up-to-date with the edits and inputs from others. The copy of the data is referred to in the code as the DATA_PATH directory. The final version of GUIDE database are then passed back manually to the sharepoint folder to share with others. One could use the actual OneDrive folder for the DATA_PATH if desired. Having separate copies allows for easier debugging but necessitates you update your version of the data regularly.

### Configure environmental variables.
The easiest way to do this is from within RStudio with the `{usethis}` package. Enter `usethis::edit_r_environ(scope = "project")` to edit the project-level `.Renviron` file, then edit the file and save with:

```R
DATA_PATH = "<local path to copy of Sharepoint data>"
EPSG = 3310
```

The `DATA_PATH` is where project data dependencies are stored on a copy of the sharepoint data. On my computer it's `T:/arich/data/sonoma_co_gsas_rate_fee`, so my environmental variable is: `DATA_PATH = "T:/arich/data/sonoma_co_gsas_rate_fee"`. In reality, this can be anywhere, including an external drive for very large data.  



The `EPSG` is the projection used in this study that all spatial data are standardized to.  

Restart `R` for changes to take effect.  



# How-to Run GUIDE Code




## To update input tables from the Sharepoint (optional)
This does not *need* to be ran. You can manually copy files from the sharepoint to your DATA_PATH. But doing so repeatedly can be tedious.

first edit ``` copy_input_files.R  ```  to reflect the location of the sharepoint for the *infolder*

<span style="color:red">**Before running, verify you want to overwrite local files with the sharepoint files**</span>.

```
cd gfs/code
bash run_ewrims.sh
```

## Updating parcel database
Update the shapefile located at *data_path/general/parcel*

## To update the Surface Water

There are 2 steps to update the surface water calculations. 

### Step 1:  
The first involves updating the fraction of a water right that goes to a parcel. For example, the water right A009378 place of use is on two parcels: 054-150-005	 054-150-010. The fraction of water rights that go individual parcels is listed in:  
*general/ewrims/water_rights_v3/input/POD_fraction_of_water_rights.csv.*  
Based on updated information the fraction of a water right used on a parcel can be updated in this csv. Additionally this file can be re-calculated based on points of diversion datasets from division of water rights, if necessary. **Be careful as this will overwrite the manually edited version used in previous guide years.** Note that the total_diversion column does not affect diversion volume allocated to any parcel, but is rather the amount from 2022 for reference.

### Step 2:  
Update the annual report from the previous year into ewrims>annual_reports. The data comes from ewrims Annual Reports exporter. Data can be downloaded from here:  
*https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/EWMenuPublic.jsp*  
*e-WRIMS - Water Rights Progress Reports*  
*https://ciwqs.waterboards.ca.gov/ciwqs/ewrims/reportingDiversionDownloadPublicSetup.do*  
Finally, Run: (**check to make sure the ewrims_calculate_POD_factors is commented/uncommented as desired**)

```
cd gfs/code
bash run_ewrims.sh
```

to produce: *general/ewrims/water_rights_v3/ewrims_totals.csv*

## To update the recycled Water

### for SRP and SON:
In the past, the following steps produce inputs for SRP GUIDE
1) GUIDE code is run with updated land use (to provide initial ag use estimates)
2) provide this data to SCI consultants
3) SCI uses this data to compare known recycled water deliveries (sometimes to multiple parcels), and then divides the RW to each parcel
4) this data is placed into the sonoma_co_gsas_rate_fee>SRP/SON>recycled_water folder as RW_output_{date}.csv
5) this data is then used by the ```  run_srp.sh  ```  and ```  run_son.sh  ```  files to add to the respective GUIDE databases

### for PET
obtain recycled water file from petaluma. the file is labeled: "2023 City of Petaluma 2016-2023 Recycled Water Use_APN.xlsx"
use the python jupyter notebook ``` recycled water petaluma calc.ipynb ```  to calculate recycled water deliveries
this creates the following file to be used in the GUIDE code:
"pet/recycled_water/output/City of Petaluma 2018-2022 Recycled Water Summary.xlsx"


## Modified Values
While most inputs into the GUIDE are automated, the modified values tables allow for the manual updating of fields.The tables are typicall modified by Indigo/Rob Pennington and should only be updated on the Shared drive. Your local copies will need to be periodically updated to reflect those changes. They can be updated using the code/copy_input_files. which will overwrite local changes.

The modified tables are listed below:  
- Active_Well_Modified.xlsx  
- Ag_GW_Use_Modified.xlsx  
- Cannabis_Water_Use_Modified.xlsx  
- Commercial_GW_Use_Modified.xlsx  
- GSA_Jurisdiction_Modified.xlsx  
- Onsite_Well_Modified.xlsx  
- Recycled_Water_Connection_Modified.xlsx  
- Recycled_Water_Use_Modified.xlsx  
- Res_GW_Use_Modified.xlsx  
- School_Golf_Modified.xlsx  
- Shared_Well_APN_Modified.xlsx  
- Shared_Well_Modified.xlsx  
- Surface_Water_Connection_Modified.xlsx  
- Surface_Water_Use_Modified.xlsx  
- Urban_Irrigation_Modified.xlsx  
- Urban_Well_Modified.xlsx  
- UseCode_Modified.xlsx  
- Well_Records_Available_Modified.xlsx

These values in these tables are added to the databases via ``` code/functions/load_modified.R```  script. See that script for how this is accomplished.

## Schema

Data schema that the database builds towards is stored at `data_path/schema/2023_05_31 schema from RP.xlsx`.  

The shema is loaded by get_schema function. 

## Combining databases
The 02_SRP.R, 03_SON.R and 04_PET.R each produce an R database to the path(data_path, "data_output") folder. The completed database is named the son_parcel_compete.rds, for example. These 3 files are then combined via the combine_dbs.R script to produce the soco_gsas_parcel_prmd.csv, soco_gsas_parcel_sci.csv, and soco_gsas_parcel.csv, and the soco_parcel_geom_only.geojson. The various outputs reflect the naming conventions preferred by each group and the **soco_gsas_parcel_prmd.csv is the final version for establishing fees**. The naming conventions and ordering are  established in the columns named: 'PRMD Public Database Field Name' and	'PRMD Public Database Order'. Fields left blank are removed from the final output databse.
The soco_parcel_geom_only.geojson is the geometry portion of the data for visualization or GIS purposes.

For methods, see `docs/memo.docx`.


## Running the pipeline

The pipeline constructs the database for SRP, SON, and PET in that order, and then glues all data together into one massive file. Each basin's script first deletes the output (eg pet_parcel_compete.rds for Petaluma) in order to ensure that the the final database does not reflect a previous execution of the code. This is important for debugging because when the entire ```bas run.sh``` is ran, if one the basin's script fails, you will not be incorporating output from a previous run. 

To execute the pipeline:  
```
cd gfs/code
bash run_pre.sh
bash run.sh
```

`run.sh` strings together the sequential steps that create the database. Inspect it to see the 4 scripts it calls in order.  



***

Last updated by *Rich Pauloo* on 2022-02-08.  
Transferred to SCWA github on 2022-3-6.
