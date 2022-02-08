# README

_Last updated 2022-02-08_  

## Contents

`2021_11_03_schema.xlsx` is metadata created by Rob Pennington (Permit Sonoma) and lists fields present in the database and how they are calculated.  

`ddw_muni_pumping_all_years.csv` is an unsummarizes sheet that shows average annual groundwater pumping (AF/yr) per water system and year in Sonoma County from 2013-2019.  

`ddw_muni_pumping.csv` is a summarized sheet that shows average annual groundwater pumping per water system (AF/yr) in Sonoma County from 2013-2019.  

`ddw_muni_pumping_summary.csv` is a summarized version of the sheet above and aggregates water system pumping at the basin scale for the period from 2013-2019. 

`ddw_muni_missing.csv` shows all pwsid and names for systems that are attached to parcels in the database, but that do not have pumping data in DDW data. 

`pumping_gsp.xlsx` was created by Rich Pauloo (LWA) and compares the results of 3 types of grouped aggregation to the GSP groundwater pumping budget model results  

`shp/soco_gsas_parcel.shp` is a shapefile of the final parcel database with fields described in the metadata from Rob Pennington. All 3 GSAs are included in the database. The EPSG is 3310, a projected coordinate reference system adequate for non-distorted spatial operations. Each row corresponds to a distinct parcel (APN). Note that due to OGR driver constraints, field names are abbreviated. To recover original names, use the column names in `soco_gsas_parcel.csv` or `soco_gsas_parcel.geojson`  

`soco_gsas_parcel.geojson` is the same as above but saved as geojson and does not abbreviate field names  

`soco_gsas_parcel.csv` is a tabular form of the spatial database without polygon geometries. As before, all 3 GSAs are represented and each row corresponds to a distinct parcel (APN).  

