source("code/source.R")
#----Merge nc4 files----
#Excract and merge date,lon,lag and precipitation into data table
merge_nc4 <- function(i){
  
  file <-ncdf4::nc_open(paste0(instaldata_path,all_nc4[i]))
  precipitation_data <- ncdf4::ncvar_get(file)
  
  date <- as.POSIXlt("1970-01-01 00:00")+ file$dim$time$vals
  
  dimnames(precipitation_data)[[1]] <- file$dim$lat$vals
  dimnames(precipitation_data)[[2]] <- file$dim$lon$vals
  
  kk <- ncdf4::nc_close(file)
  
  
  precip_summary <- data.table::data.table(reshape2::melt(precipitation_data,
                                                          varnames = c("lat", "lon"),
                                                          value.name = "precipitation"))
  precip_summary[-is.na(precipitation),] 
  
  precip_summary[precipitation<1,precipitation:=0] #Set all values with precipitation < 1mm to 0
  
  if (any(precip_summary$precipitation >0)){
  
  precip_summary[precipitation>0,]
  
  precip_summary <- cbind(precip_summary,date)
  
  return (precip_summary)
  }
  else{ return() }
  
}
