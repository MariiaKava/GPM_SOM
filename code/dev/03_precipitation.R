source("code/source.R")
source("code/dev/functions.R")

precip <- readRDS("data/precipitation.rds")

# max(precipitation$lat)
# min(precipitation$lat)
# max(precipitation$lon)
# min(precipitation$lon)



precip_stats <- precip[, .(mean_05h = mean(precipitation),
                                             sd_05h = sd(precipitation),
                                             min_05h = min(precipitation),
                                             max_05h = max(precipitation)), by = c("lat","lon")]

precip_stats <- precip_stats[,coeff_of_var := mean_05h/sd_05h]


# seasonal and annual sums ----

precip[, year := year(date)]
precip[, month := month(date)]

# seasons ====

precip %>% add_seasons()

precip_winter <- precip[season == 'winter', .(value = mean(precipitation)), by = .(lat,lon, year)]
precip_summer <- precip[season == 'summer', .(value = mean(precipitation)), by = .(lat,lon, year)]

# number of the events ====

precip_no_events_year <- precip[, .(no_events_year = .N),by = .(lat, lon, year)]
precip_no_events_season <- precip[, .(no_events_season = .N), by = .(lat, lon, season)]
precip_no_events_season_year <- precip[, .(no_events_season = .N), by = .(lat, lon, year, season)]

# avg no events by year and seasons ====

precip_avg_no_events_year <- precip_no_events_year[, .(wet_days_year = mean(no_events_year)), 
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

# types of precipitation ----

precip_class <- make_precip_class(precip)

# no event by type per year ====

precip_class_year <- precip_class[, .(no_events_year = .N), by = .(lat, lon, year, precipitation_class)]

precip_avg_class_year <- precip_class_year[precipitation_class == "light", 
                                                        .(light_events_year = mean(no_events_year)),
                                                          by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_avg_class_year,by.x = c("lat", "lon"),
                               by.y = c("lat", "lon"))

precipn_avg_class_year <- precip_class_year[precipitation_class == "moderate", 
                                                        .(moderate_events_year = mean(no_events_year)),
                                                        by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_avg_class_year,by.x = c("lat", "lon"),
                               by.y = c("lat", "lon"))

precip_avg_class_year <- precip_class_year[precipitation_class == "heavy", 
                                                        .(heavy_events_year = mean(no_events_year)),
                                                        by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_avg_class_year,by.x = c("lat", "lon"),
                               by.y = c("lat", "lon"))

precip_avg_class_year <- precip_class_year[precipitation_class == "very heavy", 
                                                        .(very_heavy_events_year = mean(no_events_year)),
                                                        by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_avg_class_year,by.x = c("lat", "lon"),
                               by.y = c("lat", "lon"))

# no events by type per season ====

precip_class_year_season <- precip_class[, .(no_events_season = .N),
                                                           by = .(lat, lon, year, season, precipitation_class)]
precip_avg_class_year_season <- data.table()

for (i in 1:length(levels(precip_class_year_season$season))){
  for (j in 1:length(levels(precip_class_year_season$precipitation_class))){
    season <- levels(precip_class_year_season$season)[i]
    class <- levels(precip_class_year_season$precipitation_class)[j]
    
    precip_avg_class_year_season <- precip_class_year_season[precipitation_class == class & season == season, 
                                                            mean(no_events_season),
                                                             by = .(lat, lon)]
    
    precip_avg_class_year_season <- precip_avg_class_year_season[,paste(season,class,"events",sep="_"):=V1]
    precip_avg_class_year_season <- precip_avg_class_year_season[,-3]
    
    precip_summary <- merge(precip_summary,precip_avg_class_year_season,by.x = c("lat", "lon"),
                                   by.y = c("lat", "lon"))
    
  }
}

# amount of precipitations daily ----

precip_daily <- precip[,date:=as.Date(date)]
precip_daily <- precip_daily[, .(precipitation=sum(precipitation)),
                        by = .(lat, lon, year, season, month, date, precipitation_class)]

# amount of precipitations by type and season ----

precip_season <- precip_daily

for (i in 1:length(levels(precip_season$season))){
  for (j in 1:length(levels(precip_season$precipitation_class))){
    season <- levels(precip_season$season)[i]
    class <- levels(precip_season$precipitation_class)[j]
    
    precip_season_aggr <- precip_season[precipitation_class == class & season == season, 
                  median(precipitation),
                  by = .(lat, lon)]
    precip_season_aggr <- precip_season_aggr[,paste(season,class,"median",sep="_"):=V1]
    precip_season_aggr <- precip_season_aggr[,-3]
    precip_summary <- merge(precip_summary,precip_season_aggr,by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))
  }}

# amount of precipitation by type per year

precip_year_sum <- precip_daily[precipitation_class == "light", 
                                           .(light_amount_precip_year = median(precipitation)),
                                           by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))

precip_year_sum <- precip_daily[precipitation_class == "moderate", 
                                            .(moderate_amount_precip_year = median(precipitation)),
                                            by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))

precip_year_sum <- precip_daily[precipitation_class == "heavy", 
                                           .(heavy_amount_precip_year = median(precipitation)),
                                           by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))

precip_year_sum <- precip_daily[precipitation_class == "very heavy", 
                                           .(very_heavy_amount_precip_year = median(precipitation)),
                                           by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))

# amount of precipitation by year

precip_year_sum <- precip_daily[,.(amount_precip_year = median(precipitation)),
                                by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_year_sum,by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))

# amount of precipitation by season

precip_season_sum <- precip_daily[season == "summer",
                                  .(median_precip_summer = median(precipitation)), 
                                  by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat","lon"),
                        by.y = c("lat","lon"))
precip_season_sum <- precip_daily[season == "winter",
                                  .(median_precip_winter = median(precipitation)), 
                                  by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat", "lon"),
                        by.y = c("lat","lon"))
precip_season_sum <- precip_daily[season == "autumn",
                                  .(median_precip_autumn = median(precipitation)), 
                                  by = .(lat, lon)]
precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))
precip_season_sum <- precip_daily[season == "spring",
                                  .(median_precip_spring = median(precipitation)), 
                                  by = .(lat,lon)]
precip_summary <- merge(precip_summary,precip_season_sum,by.x = c("lat","lon"),
                        by.y = c("lat","lon"))
    



# draft graphs ----  
  
# delta_lat <- 0.1
# delta_lon <- 0.1
#   
# qmplot(lon, lat, data = precip_summary, geom = "tile", fill = wet_days_summer, 
#          alpha = wet_days_summer, zoom = 8,
#          legend = "bottomleft") +
#   geom_leg(aes(xend = lon + delta_lon, yend = lat + delta_lat)) 
# 
# 
# saveRDS(precipitation_summary,"data/precipitation_summary.rds")



