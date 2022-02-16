library(raster)

precip <- readRDS("results/zero_threshold/daily_precip_threshold-0.rds")
precip_threshold1 <- precip[precip$precipitation>2,]
hist(precip_threshold1$precipitation[precip_threshold1$precipitation<2],breaks = 10)
max(precip$precipitation)
out_precip <- precip_threshold1[precip_threshold1$precipitation<2,]
precip$month <- as.factor(precip$month)
precip$year <- as.factor(precip$year)
ggplot(precip_threshold1, aes(x=precip_threshold1$year,y = precip_threshold1$precipitation))+
  geom_boxplot()

ggplot(precip, aes(x=precip$year,y = precip$precipitation))+
  geom_boxplot()


daily_precip <- precip[,.(precipitation = mean(precipitation)),by = .(year,season,month,date)]
plot(daily_precip$date,daily_precip$precipitation)
outlier <- daily_precip[daily_precip$precipitation>30,]

monthly_precip <- precip[,.(precipitation = sum(precipitation)), by = .(lat,lon,year,season,month)]
avg_monthly_precip <- monthly_precip[,.(precipitation = mean(precipitation)), by = .(year,season,month)]
avg_monthly_precip[,date:=as.Date(paste0(year,"-",month,"-01"))]
plot(avg_monthly_precip$month,avg_monthly_precip$precipitation, type = "l")
ggplot(avg_monthly_precip, aes(x=avg_monthly_precip$month,y = avg_monthly_precip$precipitation))+
  geom_bar(stat='identity')+
  facet_wrap(facets = vars(year),ncol = 4)+
  scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))

outlier <- precip[precip$year=="2011" & precip$month=="11",]
avg_monthly_precip$precipitation[avg_monthly_precip$month=="11" & avg_monthly_precip$year=="2011"]

avg_monthly_precip$year <- as.factor(avg_monthly_precip$year)
ggplot(avg_monthly_precip, aes(x=avg_monthly_precip$year,y = avg_monthly_precip$precipitation))+
  geom_boxplot()+
  facet_wrap(facets = vars(season),ncol = 2)


outlier <- avg_monthly_precip[avg_monthly_precip$precipitation>150,]

season_precip <- precip[,.(precipitation = sum(precipitation)), by = .(lat,lon,year,season)]
avg_season_precip <- season_precip[,.(precipitation = mean(precipitation)), by = .(year,season)]
plot(avg_season_precip$season,avg_season_precip$precipitation)
outlier <- avg_season_precip[avg_season_precip$precipitation>350,]

ann_precip <- precip[,.(precipitation = sum(precipitation)), by = .(lat,lon,year)]
avg_ann_precip <- ann_precip[,.(precipitation = mean(precipitation)), by = .(year)]
plot(avg_ann_precip$year,avg_ann_precip$precipitation)
outlier <- avg_season_precip[avg_season_precip$precipitation>350,]

dsn <- "code/shapes/SPH_KRAJ.shp"

wa.map <- readOGR(dsn)

precip_summary_pol <-  as.data.frame(heavy_precip)
coordinates(precip_summary_pol) <- ~ lon + lat
proj4string(precip_summary_pol) <- proj4string(wa.map)


ggplot(heavy_precip)+
  geom_tile(aes(lon,lat,fill=heavy_precip$precipitation))+
  borders(wa.map,colour = "black")+
  ggtitle("Precipitation 2020-06-26")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)
  #labs(fill='Precipitation (mm)') +
  #theme(axis.text=element_text(size=6),
   #     axis.title=element_text(size=12))+

#monthly precip
precip %>% as.data.table()
precip_monthly <- precip[,.(monthly_precip=sum(precipitation)), by = .(lat,lon,year,month)]
lat <- unique(precip$lat)
lon <- unique(precip$lon)

precip_monthly <- precip_monthly[,.(ann_precip=sum(monthly_precip)), by = .(lat,lon,year)]
ann_precip <- precip_monthly[,.(ann_precip=mean(ann_precip)), by = .(lat,lon)]

dsn <- "code/shapes/SPH_KRAJ.shp"

wa.map <- readOGR(dsn)
wa.map_buff <- buffer(wa.map,width=0.06)

precip2 <-  as.data.frame(ann_precip)
coordinates(precip2) <- ~ lon + lat
proj4string(precip2) <- proj4string(wa.map)

r2 <- crop(precip2,wa.map_buff)
r2 <- as.data.frame(r2)

ggplot(r2)+
  geom_tile(aes(lon,lat,fill=r2$ann_precip))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)


precip <- readRDS("data/precipitation_05h_CR_sm.rds")
tiff(filename="results/zero_threshold/05Var_seasonally.tiff", width=2300, height=2000, res=300)
ggplot(precip, aes(x=precip$season,y = precip$precipitation))+
  geom_boxplot()
dev.off()

precip <- precip[precip$precipitation>0.1,]
precip[, hour := hour(date)]
precip_events <- precip
precip_events[,date:= as.Date(date,"%d/%m/%y")]

rm(precip)
#rm(precip_cut)

# sum to receive avg intensity per hour
precip_events <- precip_events[,.(precipitation = mean(precipitation)),by = .(lat,lon,year,month,season,date,hour)]
precip_events <- precip_events[precip_events$precipitation<20,]
tiff(filename="results/threshold_precipInt_01/hourlyIntVar_seasonally.tiff", width=2300, height=2000, res=300)
ggplot(precip_events, aes(x=precip_events$season,y = precip_events$precipitation))+
  geom_boxplot()
dev.off()
