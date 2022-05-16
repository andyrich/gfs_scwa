# Load modified GW uses for individual parcels. These values will be assigned to their GW use components


load_modified <- function(var){
  # function to load modified table
  df <-  readxl::read_xlsx( path(data_path, "general/modified_values",
                                 paste(var,  '.xlsx', sep ='') ))
  

  
  l <- list(
    Ag_GW_Use_Modified= c('Ag_GW_Use_Modified_Ac_Ft',
                          'Ag_GW_Use_Comment'),
    Commercial_GW_Use_Modified=c('Commercial_GW_Use_Modified_Ac_Ft',
                                 'Commercial_GW_Use_Comment'),
    Res_GW_Use_Modified=c('Res_GW_Use_Modified_Ac_Ft',
                          'Res_GW_Use_Comment'),
    School_Golf_Modified=c('School_Golf_GW_Use_Modified_Ac_Ft',
                           'School_Golf_GW_Use_Comment'),
    Urban_Irrigation_Modified=c('Urban_Irrigation_Modified_Ac_Ft',
                                'Urban_Irrigation_GW_Use_Comment'            ))
  x <- l[var]
  

  cols = c('APN',
           unlist(x[[1]][1]),
           unlist(x[[1]][2]))
  
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



load_all_modified <- function(){
  
  # load all of the modified values from all of the sheets, then join together
  print('loading all modified values')
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

  
  return(dfall)}

join_with_modified <- function(parcel){
  # join modified fields to the parcel database
  modified <- load_all_modified()
  
  cols = colnames(modified)
  print("These are the colnames of the modified database")
  print(cols)
  
  parcel = left_join(parcel, modified) %>%
    mutate(Ag_GW_Use_Modified = ifelse(is.na(Ag_GW_Use_Modified),'No','Yes'),
           Commercial_GW_Use_Modified = ifelse(is.na(Commercial_GW_Use_Modified),'No','Yes'),
           Res_GW_Use_Modified = ifelse(is.na(Res_GW_Use_Modified),'No','Yes'),
           School_Golf_Modified = ifelse(is.na(School_Golf_Modified),'No','Yes'),
           Urban_Irrigation_Modified = ifelse(is.na(Urban_Irrigation_Modified),'No','Yes'),
           )

  
  return(parcel)
}