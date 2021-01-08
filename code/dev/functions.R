source("code/source.R")
#----Merge nc4 files----
#Excract and merge date,lon,lag and precipitation into data table
merge_nc4 <- function(i){

  file <-ncdf4::nc_open(paste0(instaldata_path,all_nc4[i]))
    
  
  precipitation_data <-  ncdf4::ncvar_get(file)

  
  date <- as.POSIXlt("1970-01-01 00:00")+ file$dim$time$vals
  
  dimnames(precipitation_data)[[1]] <- file$dim$lat$vals
  dimnames(precipitation_data)[[2]] <- file$dim$lon$vals
  
  kk <- ncdf4::nc_close(file)
  
  
  precip_summary <- data.table::data.table(reshape2::melt(precipitation_data,
                                                          varnames = c("lat", "lon"),
                                                          value.name = "precipitation"))

  precip_summary <- precip_summary[complete.cases(precip_summary), ]
  
  precip_summary<-precip_summary[precipitation<1,precipitation:=0] #Set all values with precipitation < 1mm to 0
 
  
  
  if (any(precip_summary$precipitation >0)){
  
  precip_summary<-precip_summary[precip_summary$precipitation>0,]
  
  precip_summary <- cbind(precip_summary,date)
  
  return (precip_summary)
  }
  else{ return() }

  
}

add_seasons <- function(dt){
  dt[month == 12 | month == 1 | month == 2, season := 'winter']
  dt[month == 3 | month == 4 | month == 5, season := 'spring']
  dt[month == 6 | month == 7 | month == 8, season := 'summer']
  dt[month == 9 | month == 10 | month == 11, season := 'autumn']
  dt[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]
  
}

make_precip_class <- function(dt){
  quantile <- quantile(dt$precipitation)
  dt[, precipitation_class := factor('light')]
  dt[precipitation >= quantile[2] & precipitation < quantile[3], precipitation_class := factor('moderate')]
  dt[precipitation >= quantile[3] & precipitation < quantile[4], precipitation_class := factor('heavy')]
  dt[precipitation >= quantile[4] & precipitation < quantile[5], precipitation_class := factor('very_heavy')]
}


manipulate_plot <- function(precip){
  
  dsn <- "code/shapes/SPH_KRAJ.shp"
  wa.map <- readOGR(dsn)
  
  precip2 <-  as.data.frame(precip)
  coordinates(precip2) <- ~ lon + lat
  proj4string(precip2) <- proj4string(wa.map)
  
  daily_rain_df <- precip2[!is.na(over(precip2, as(wa.map, "SpatialPolygons"))), ]
  daily_rain_df <- as.data.frame(daily_rain_df)
  
  par_to_pick <- as.list(colnames(precip[,-c(1,2)]))
  
  manipulate(
    {ggplot() + 
        geom_tile(data = daily_rain_df, aes(x=lon, y = lat, fill=daily_rain_df[,factor])) + 
        coord_fixed(ratio = 1) +
        scale_fill_viridis(direction = -1) +
        theme_bw()+
        guides(fill=guide_legend(factor))+
        ggtitle("Plot showing chosen precipitation summary parameter")},
    factor = picker(par_to_pick))
  
}



