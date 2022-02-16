source("code/source.R")

precip <- readRDS("data/precipitation_0122.rds")
precip <- precip[year(precip$date)>2000 & year(precip$date)<2021,]

# cut to CZ ----
precip <- precip[,coords:=""]
precip$coords <- paste(precip$lat," ",precip$lon)

precip_CR <- precip[precip$coords %in% precip_cut$coords,]

saveRDS(precip_CR,"data/precipitation_05h_CR.rds")
precip <- readRDS("data/precipitation_05h_CR.rds")
# adding missing 30min ----
date <- unique(precip$date) # checking datetime in df
days <- seq.Date(from = as.Date('2001-01-01'), to = as.Date('2020-12-31'), by = 'days') # creating dates for the whole period
hm <- merge(0:23, seq(0, 45, by = 30)) # creating time for a 24hours
datetime <- merge(days, chron(time = paste(hm$x, ':', hm$y, ':', 0))) # merge date and time
colnames(datetime) <- c('date', 'time')
# create datetime
datetime$dt <- as.POSIXct(paste(datetime$date, datetime$time))
# create right order
datetime <- datetime[order(datetime$dt), ]
row.names(datetime) <- NULL
rm(hm,days)

# creating vector with missing 30 minutes
`%notin%` <- Negate(`%in%`)
fill_dates <- datetime[datetime$dt %notin% date,]
# unique coordinates
coords <- precip[,.(precipitation = mean(precipitation)), by = .(lat,lon)]
# for each coordintae all missing dates filled with precip int 0
precip2 <- foreach(i=1:length(coords),.combine = rbind) %do% {
  temp <- cbind(coords[i,],fill_dates)
  temp <- temp[,precipitation:=0]
  temp <- temp[,-c(4,5)]
  return(temp)
}
# adding missing
precip <- precip[,-5]
colnames(precip2) <- c("lat","lon","precipitation","date")
precip <- rbind(precip,precip2)
dates <- unique(precip$date)

saveRDS(precip,"data/filled_missing_datetime/05h_precipitation.rds")

# cut coords closer to CZ ----
# dsn <- "code/shapes/SPH_KRAJ.shp"
# wa.map <- readOGR(dsn)
# wa.map_buff <- buffer(wa.map,width=0.06)
# precip2 <-  as.data.frame(precip)
# rm(precip)
# coordinates(precip2) <- ~ lon + lat
# proj4string(precip2) <- proj4string(wa.map)
# rm(wa.map)
# precip <- crop(precip2,wa.map_buff)
# precip <- as.data.frame(precip)
# rm(precip2,wa.map_buff)
# 
# daily_precip <- readRDS("results/zero_threshold/daily_precip_threshold-0.rds")
# precip <- as.data.table(precip)
# precip <- precip[,coords:=""]
# precip$coords <- paste(precip$lat," ",precip$lon)
# daily_precip <- daily_precip[,coords:=""]
# daily_precip$coords <- paste(daily_precip$lat," ",daily_precip$lon)
# daily_precip_cut <- daily_precip[daily_precip$coords %in% precip$coords,]
# cc <- unique(daily_precip_cut$coords)
# 
# saveRDS(daily_precip_cut,"results/zero_threshold/daily_precip_threshold-0.rds")
# saveRDS(precip,"results/zero_threshold/precipitation_summary_threshold-0.rds")
#---
