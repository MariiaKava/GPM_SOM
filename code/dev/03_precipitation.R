source("code/source.R")
source("code/dev/functions.R")


precip <- readRDS("data/precipitation_new.rds")

# min(year(precip$date))
# max(year(precip$date))

precip_stats <- precip[, .(mean_05h = mean(precipitation),
                                             sd_05h = sd(precipitation),
                                             min_05h = min(precipitation),
                                             max_05h = max(precipitation)), by = c("lat","lon")]

precip_stats <- precip_stats[,coeff_of_var := mean_05h/sd_05h]


# add year and month ----

precip[, year := year(date)]
precip[, month := month(date)]

# seasons ====

precip %>% add_seasons()

# number of the events ====

precip[, hour := hour(date)]
precip_events <- precip
precip_events[,date:= as.Date(date,"%d/%m/%y")]

# sum to recieve avg intensity per hour
precip_events <- precip_events[,.(precipitation = mean(precipitation)),by = .(lat,lon,year,month,season,date,hour)]

# sum pecipitation daily
precip_events <- precip_events[,.(precipitation = sum(precipitation)), by = .(lat,lon,year,month,season,date)]


# calculate number of events annual and seasonal
precip_summary <- calc_n_events(precip_events)

#calculate annual number of events by type annually and seasonally
precip_summary <- merge(precip_summary,calc_by_precip_type(precip_events),by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))

# calculate amount of precipitation by type annually and seasonally

precip_summary <- merge(precip_summary,int_precip(precip_events),by.x = c("lat", "lon"),
                        by.y = c("lat", "lon"))




#----plot----
par_to_pick <- as.list(colnames(precip_summary[,-c(1,2)]))

dsn <- "code/shapes/SPH_KRAJ.shp"

wa.map <- readOGR(dsn)

precip2 <-  as.data.frame(precip_summary)
coordinates(precip2) <- ~ lon + lat
proj4string(precip2) <- proj4string(wa.map)

daily_rain_df <- precip2[!is.na(over(precip2, as(wa.map, "SpatialPolygons"))), ]

daily_rain_df <- as.data.frame(daily_rain_df)


ggplot(precip_summary)+
  geom_tile(aes(lon,lat,fill=precip_summary$wet_events_year))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)

precip_to_plot <- precip_daily[,.(annual_precip = sum(precipitation)), by = c("lat","lon","year")]

ggplot(precip_summary)+
  geom_tile(aes(lon,lat,fill=precip_summary$amount_avg_int_year))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)+
  facet_wrap(facets = vars(year),ncol = 2)

tiff(filename="wet_days_year.tiff", width=2300, height=2000, res=300)
ggplot(precip_summary)+
  geom_tile(aes(lon,lat,fill=wet_days_year))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)
dev.off()

mean(precip_summary$amount_avg_int_year)

manipulate(
  {ggplot() +
      geom_tile(data = daily_rain_df, aes(x=lon, y = lat, fill=daily_rain_df[,factor])) +
      coord_fixed(ratio = 1) +
      scale_fill_viridis(direction = -1) +
      theme_bw()+
      guides(fill=guide_legend(factor))+
      ggtitle("Plot showing chosen precipitation summary parameter")},
  factor = picker(par_to_pick))


tiff(filename="annual_precip.tiff", width=2300, height=2000, res=300)
ggplot(precip_to_plot)+
  geom_tile(aes(lon,lat,fill=annual_precip))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)+
  facet_wrap(facets = vars(year),ncol = 4)
dev.off()



saveRDS(precip_summary,"data/precipitation_summary_new.rds")



