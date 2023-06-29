fill_parcel_info <- function(parcel, basin_name) {
  parcel <- parcel%>% 
  mutate(
    # Add parcel size
    LandSizeParcelAcres = LandSizeAcres,
    # Is the parcel a boundary parcel
    Basin_Boundary_Parcel = edge,
    # Basin_Boundary_Parcel = ifelse(APN %in% boundary_parcels, "Yes", "No"),
    # area of the total APN across both GSAs is the recorded APN area
    Intersect_GSA_Bndry_Sum_Acres = ifelse(edge =='Yes',
                                           LandSizeAcres, NA),
    # adjust area of the bisected parcels in the GSA. remember, we clipped 
    # to the B118 basin polygon, so the area is just the calculated area!
    LandSizeAcres = ifelse(
      edge =='Yes',
      as.numeric(units::set_units(st_area(geometry), acres)), 
      LandSizeAcres
    ),
    # # proportion of the APN in this GSA, used to assign a GSA
    # area_prop_apn = LandSizeAcres / Intersect_GSA_Bndry_Sum_Acres,
    # GSA_Jurisdiction_Prelim = !!parse_quosure(basin_name),
    # intentionally left blank for clients to evaluate and populate
    # GSA_Jurisdiction_Modified = NA,
    # GSA_Jurisdiction_Mod_Value = NA,
    # GSA_Jurisdiction = NA
  ) 
  
  parcel$GSA_Jurisdiction_Prelim = basin_name
  
  return(parcel)}


parcel_contact <-function(parcel){
  
  parcel <- parcel %>% 
    mutate(
      # parcel and contact info
      LandSizeAcres       = LndSzAcre, # this value gets changed below
      LandSizeParcelAcres = LandSizeAcres,
      UseCode_Description = UseCDesc,
      UseCode_Category    = UseCType,
      CurrentOwnerName    = NA,
      MailingAddress1     = MailAdr1,
      MailingAddress2     = MailAdr2,
      MailingAddress3     = MailAdr3,
      MailingAddress4     = MailAdr4,
      Situs_Address       = SitusFmt1)
  
  return(parcel)
}