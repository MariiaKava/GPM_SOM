#----plot----
daily_precip <- readRDS("results/filled_missing_datetime/threshold_precipInt_01/daily_precip.rds")
precip_summary <- readRDS("results/filled_missing_datetime/threshold_precipInt_01/precipitation_summary.rds")

par_to_pick <- as.list(colnames(precip_summary[,-c(1,2)]))

dsn <- "code/shapes/SPH_KRAJ.shp"

wa.map <- readOGR(dsn)

precip_summary_pol <-  as.data.frame(heavy_precip)
coordinates(precip_summary_pol) <- ~ lon + lat
proj4string(precip_summary_pol) <- proj4string(wa.map)

daily_rain_df <- precip_summary_pol[!is.na(over(precip_summary_pol, as(wa.map, "SpatialPolygons"))), ]

daily_rain_df <- as.data.frame(daily_rain_df)



ANN_precip <- daily_precip[,.(annual_precip = sum(precipitation)), by = c("lat","lon","year")]

tiff(filename="results/zero_threshold/Ann_precip_facetWrap.tiff", width=2300, height=2000, res=300)
ggplot(ANN_precip)+
  geom_tile(aes(lon,lat,fill=ANN_precip$annual_precip))+
  borders(wa.map,colour = "black")+
  ggtitle("Annual precipitation")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)+
  labs(fill='Precipitation (mm)') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=12))+
  facet_wrap(facets = vars(year),ncol = 4)
dev.off()


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

precip_to_plot <- ANN_precip[,.(annual_precip = mean(annual_precip)), by = c("lat","lon")]
tiff(filename="annual_precip.tiff", width=2300, height=2000, res=300)
ggplot(precip_to_plot)+
  geom_tile(aes(lon,lat,fill=annual_precip))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1)
dev.off()