source("code/source.R")
#----Merge nc4 files----
#Excract and merge date,lon,lag and precipitation into data table
merge_nc4 <- function(i){

  
  file <-ncdf4::nc_open(paste0(instaldata_path,all_nc4[1]))  
  
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


calc_n_events <- function(precip){
  precip_no_events_year <- precip[, .(no_events_year = .N),by = .(lat, lon, year)]
  precip_no_events_season <- precip[, .(no_events_season = .N), by = .(lat, lon, season)]
  precip_no_events_season_year <- precip[, .(no_events_season = .N), by = .(lat, lon, year, season)]
  precip_avg_no_events_year <- precip_no_events_year[, .(wet_days_year = mean(no_events_year)),, 
                                                     by = .(lat,lon)]
  
  precip_summary <- precip_avg_no_events_year
  precip_avg_no_events_summer <- precip_no_events_season_year[season == "summer",
                                                              .(wet_days_summer = mean(no_events_season)), 
                                                              by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_avg_no_events_summer,by.x = c("lat","lon"),
                          by.y = c("lat","lon"))
  
  precip_avg_no_events_winter <- precip_no_events_season_year[season == "winter",
                                                              .(wet_days_winter = mean(no_events_season)), 
                                                              by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_avg_no_events_winter,by.x = c("lat", "lon"),
                          by.y = c("lat","lon"))
  
  
  precip_avg_no_events_autumn <- precip_no_events_season_year[season == "autumn",
                                                              .(wet_days_autumn = mean(no_events_season)), 
                                                              by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_avg_no_events_autumn,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  
  precip_avg_no_events_spring <- precip_no_events_season_year[season == "spring",
                                                              .(wet_days_spring = mean(no_events_season)), 
                                                              by = .(lat,lon)]
  precip_summary <- merge(precip_summary,precip_avg_no_events_spring,by.x = c("lat","lon"),
                          by.y = c("lat","lon"))
  precip_avg_no_events_year <- precip_no_events_year[, .(wet_events_year = mean(no_events_year)),, 
                                                     by = .(lat,lon)]

  
  return(precip_summary)
}

calc_by_precip_type <- function(precip){
  # types of precipitation ----
  
  precip_class <- make_precip_class(precip)
  
  # no event by type per year ====
  
  
  precip_class_year <- precip_class[, .(annual_number_wet_days = .N), by = .(lat, lon, year, precipitation_class)]
  
  precip_avg_class_year <- precip_class_year[precipitation_class == "light", 
                                             .(light_wet_days_year = mean(annual_number_wet_days)),
                                             by = .(lat, lon)]
  precip_summary <- precip_avg_class_year
  
  precip_avg_class_year <- precip_class_year[precipitation_class == "moderate", 
                                             .(moderate_wet_days_year = mean(annual_number_wet_days)),
                                             by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_avg_class_year,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  
  precip_avg_class_year <- precip_class_year[precipitation_class == "heavy", 
                                             .(heavy_wet_days_year = mean(annual_number_wet_days)),
                                             by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_avg_class_year,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  
  precip_avg_class_year <- precip_class_year[precipitation_class == "very_heavy", 
                                             .(very_heavy_wet_days_year = mean(annual_number_wet_days)),
                                             by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_avg_class_year,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  # no events by type per season ====
  
  precip_class_year_season <- precip_class[, .(no_wet_days_season = .N),
                                           by = .(lat, lon, year, season, precipitation_class)]
  precip_avg_class_year_season <- data.table()
  
  for (i in 1:length(levels(precip_class_year_season$season))){
    for (j in 1:length(levels(precip_class_year_season$precipitation_class))){
      season <- levels(precip_class_year_season$season)[i]
      class <- levels(precip_class_year_season$precipitation_class)[j]
      
      precip_avg_class_year_season <- precip_class_year_season[precipitation_class == class & season == season, 
                                                               mean(no_wet_days_season),
                                                               by = .(lat, lon)]
      
      precip_avg_class_year_season <- precip_avg_class_year_season[,paste(season,class,"wet_days",sep="_"):=V1]
      precip_avg_class_year_season <- precip_avg_class_year_season[,-3]
      
      precip_summary <- merge(precip_summary,precip_avg_class_year_season,by.x = c("lat", "lon"),
                              by.y = c("lat", "lon"))
      
    }
  }
  return(precip_summary)
  
}

int_precip <- function(precip_daily){
  # amount of precipitation by type per year
  precip_daily <- precip_events
  
  precip_annual <- precip_daily[,.(precipitation = sum(precipitation)),
                                  by = .(lat, lon,year,precipitation_class)]
  
  precip_year_sum <- precip_annual[precipitation_class == "light", 
                                  .(light_annual_sum_precip = mean(precipitation)),
                                  by = .(lat, lon)]
  precip_summary <- precip_year_sum
 
  precip_year_sum <- precip_annual[precipitation_class == "moderate", 
                                  .(moderate_annual_sum_precip = mean(precipitation)),
                                  by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  precip_year_sum <- precip_annual[precipitation_class == "heavy", 
                                  .(heavy_annual_sum_precip = mean(precipitation)),
                                  by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  precip_year_sum <- precip_annual[precipitation_class == "very_heavy", 
                                  .(very_heavy_annual_sum_precip = mean(precipitation)),
                                  by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  
  # amount of precipitation by year
  
  precip_annual<- precip_daily[, 
                               .(precipitation = sum(precipitation)),
                               by = .(lat, lon,year)]
  precip_year_sum <- precip_annual[,.(annual_sum_precip = mean(precipitation)),
                                  by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  
  # amount of precipitation by season
  precip_season <- precip_daily[,.(precipitation = sum(precipitation)), 
                                    by = .(lat, lon,year,season)]
  precip_season_sum <- precip_season[season == "summer",
                                    .(avg_summer_sum_precip = mean(precipitation)), 
                                    by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat","lon"),
                          by.y = c("lat","lon"))
  precip_season_sum <- precip_season[season == "winter",
                                    .(avg_winter_sum_precip = mean(precipitation)), 
                                    by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat", "lon"),
                          by.y = c("lat","lon"))
  precip_season_sum <- precip_season[season == "autumn",
                                    .(avg_autumn_sum_precip = mean(precipitation)), 
                                    by = .(lat, lon)]
  precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat", "lon"),
                          by.y = c("lat", "lon"))
  precip_season_sum <- precip_season[season == "spring",
                                    .(avg_spring_sum_precip = mean(precipitation)), 
                                    by = .(lat,lon)]
  precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat","lon"),
                          by.y = c("lat","lon"))
  precip_season <- precip_daily[,.(precipitation = sum(precipitation)), 
                                by = .(lat, lon,year,season,precipitation_class)]

  
  for (i in 1:length(levels(precip_season$season))){
    for (j in 1:length(levels(precip_season$precipitation_class))){
      season <- levels(precip_season$season)[i]
      class <- levels(precip_season$precipitation_class)[j]
      precip_season_aggr <- precip_season[precipitation_class == class & season == season, 
                                          sum(precipitation),
                                          by = .(lat, lon,year,season,precipitation_class)]
      precip_season_aggr <- precip_season_aggr[precipitation_class == class & season == season, 
                                          mean(V1),
                                          by = .(lat, lon)]
      precip_season_aggr <- precip_season_aggr[,paste("avg",season,class,"sum_precip",sep="_"):=V1]
      precip_season_aggr <- precip_season_aggr[,-3]
      precip_summary <- merge(precip_summary,precip_season_aggr,by.x = c("lat", "lon"),
                              by.y = c("lat", "lon"))
    }}
  return(precip_summary)
}





