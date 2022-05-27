source("code/source.R")
source("code/dev/functions.R")

path <- "results/filled_missing_datetime/threshold_precipInt_01/" # folder to save files
precip <- readRDS("data/filled_missing_datetime/05h_precipitation.rds")

# add year and month ----
precip[, year := year(date)]
precip[, month := month(date)]

# adding threshold
#threshold_ex <- precip[precip$precipitation>50 | precip$precipitation<0.1,]
precip$precipitation[precip$precipitation<0.1] <- 0

# seasons ====

precip %>% add_seasons()

# number of the events ====

precip <- as.data.table(precip)
precip[, hour := hour(date)]
precip_events <- precip
precip_events[,date:= as.Date(date,"%d/%m/%y")]

rm(precip)
#rm(precip_cut)
  
# sum to receive avg intensity per hour
precip_events <- precip_events[,.(precipitation = mean(precipitation)),by = .(lat,lon,year,month,season,date,hour)]
  
# sum precipitation daily
precip_events <- precip_events[,.(precipitation = sum(precipitation)), by = .(lat,lon,year,month,season,date)]

# create sample table for latex format ----- 
# rows <- sample(1:5016002, 10, replace=TRUE)
# daily_precip_cut <- precip_events[rows,]
# daily_precip_cut$date <- as.character(daily_precip_cut$date)
# xtable(daily_precip_cut)
  
saveRDS(precip_events,paste0(path,"daily_precip.rds"))

precip_summary <- calc_n_events(precip_events)
precip <- calc_by_precip_type(precip_events)
precip_summary <- merge(precip_summary,precip)
rm(precip)
precip <- int_precip(precip_events)
precip_summary <- merge(precip_summary,precip)
saveRDS(precip_summary,paste0(path,"precipitation_summary.rds"))
