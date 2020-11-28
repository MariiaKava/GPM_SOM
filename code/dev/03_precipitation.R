source("code/source.R")
precipitation <- readRDS("data/precipitation.rds")

# max(precipitation$lat)
# min(precipitation$lat)
# max(precipitation$lon)
# min(precipitation$lon)



precipitation_stats_05h <- precipitation[, .(mean_05h = mean(precipitation),
                                             sd_05h = sd(precipitation),
                                             min_05h = min(precipitation),
                                             max_05h = max(precipitation)), by = c("lat","lon")]

precipitation_stats_05h <- precipitation_stats_05h[,coeff_of_var := mean_05h/sd_05h]


# seasonal and annual sums ----

precipitation[, year := year(date)]
precipitation[, month := month(date)]

# seasons ====

precipitation[month == 12 | month == 1 | month == 2, season := 'winter']
precipitation[month == 3 | month == 4 | month == 5, season := 'spring']
precipitation[month == 6 | month == 7 | month == 8, season := 'summer']
precipitation[month == 9 | month == 10 | month == 11, season := 'autumn']
precipitation[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]

precipitation_winter <- precipitation[season == 'winter', .(value = mean(precipitation)), by = .(lat,lon, year)]
precipitation_summer <- precipitation[season == 'summer', .(value = mean(precipitation)), by = .(lat,lon, year)]

# number of the events ====

precipitation_no_events_year <- precipitation[,.(no_events_year = .N),by = .(lat,lon,year)]
precipitation_no_events_season <- precipitation[,.(no_events_season = .N), by = .(lat,lon,season)]
precipitation_no_events_season_year <- precipitation[,.(no_events_season = .N), by = .(lat,lon,year,season)]

# avg no events by year and seasons ====

precipitation_avg_no_events_year <- precipitation_no_events_year[,.(wet_days_year = mean(no_events_year)), 
                                                                 by = .(lat,lon)]


precipitation_summary <- precipitation_avg_no_events_year


precipitation_avg_no_events_summer <- precipitation_no_events_season_year[season == "summer",
                                                                     .(wet_days_summer = mean(no_events_season)), 
                                                                      by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_no_events_summer,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))

precipitation_avg_no_events_winter <- precipitation_no_events_season_year[season == "winter",
                                                                     .(wet_days_winter = mean(no_events_season)), 
                                                                     by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_no_events_winter,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))


precipitation_avg_no_events_autumn <- precipitation_no_events_season_year[season == "autumn",
                                                                     .(wet_days_autumn = mean(no_events_season)), 
                                                                     by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_no_events_autumn,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))

precipitation_avg_no_events_spring <- precipitation_no_events_season_year[season == "spring",
                                                                     .(wet_days_spring = mean(no_events_season)), 
                                                                     by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_no_events_spring,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))

# types of precipitation ----

precipitation[,quantile(precipitation)]
precipitation_class <- precipitation
precipitation_class[, precipitation_class := factor('light')]
precipitation_class[precipitation >= 1.000000 & precipitation < 1.367653, precipitation_class := factor('moderate')]
precipitation_class[precipitation >= 1.949636 & precipitation < 3.144239, precipitation_class := factor('heavy')]
precipitation_class[precipitation >= 3.144239 & precipitation < 84.374619, precipitation_class := factor('very heavy')]

# no event by type per year ====

precipitaion_class_year <- precipitation_class[,.(no_events_year = .N),by = .(lat,lon,year,precipitation_class)]

precipitation_avg_class_year <- precipitaion_class_year[precipitation_class == "light", 
                                                        .(light_events_year = mean(no_events_year)),
                                                          by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_class_year,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))

precipitation_avg_class_year <- precipitaion_class_year[precipitation_class == "moderate", 
                                                        .(moderate_events_year = mean(no_events_year)),
                                                        by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_class_year,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))

precipitation_avg_class_year <- precipitaion_class_year[precipitation_class == "heavy", 
                                                        .(heavy_events_year = mean(no_events_year)),
                                                        by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_class_year,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))

precipitation_avg_class_year <- precipitaion_class_year[precipitation_class == "very heavy", 
                                                        .(very_heavy_events_year = mean(no_events_year)),
                                                        by = .(lat,lon)]
precipitation_summary <- merge(precipitation_summary,precipitation_avg_class_year,by.x = c("lat","lon"),
                               by.y = c("lat","lon"))

# no events by type per season ====

precipitation_class_year_season <- precipitation_class[,.(no_events_season = .N),
                                                           by = .(lat,lon,year,season,precipitation_class)]

precipitation_avg_class_year_season <- data.table()
precipitation_summary1 <- data.table()

levels(precipitation_class_year_season$season)[1]

for (i in 1:length(levels(precipitation_class_year_season$season))){
  for (j in 1:length(levels(precipitation_class_year_season$precipitation_class))){
    season <- levels(precipitation_class_year_season$season)[i]
    class <- levels(precipitation_class_year_season$precipitation_class)[j]
    
    precipitation_avg_class_year_season <- precipitation_class_year_season[precipitation_class == class & season == season, 
                                                            mean(no_events_season),
                                                             by = .(lat,lon)]
    
    precipitation_avg_class_year_season <- precipitation_avg_class_year_season[,paste(season,class,"events",sep="_"):=V1]
    precipitation_avg_class_year_season <- precipitation_avg_class_year_season[,-3]
    
    precipitation_summary <- merge(precipitation_summary,precipitation_avg_class_year_season,by.x = c("lat","lon"),
                                   by.y = c("lat","lon"))
    
  }
}


