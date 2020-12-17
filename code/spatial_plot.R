library(rgdal)
library(maps)
library(viridis)
map(regions="Asia")
map(xlim=c(12,18),ylim=c(48,51))

precip

abc <- precip %>% 
  ggplot() + 
  theme_bw() + 
  geom_raster(aes(lon, lat, fill = precipitation)) + 
  #geom_sf(data = spData::world, col = "black") +
  scale_fill_continuous(type = "viridis", option = "A", direction = -1) 

abc + map(xlim=c(12,18),ylim=c(48,51))


precip

dsn <- "data/WGS84/SPH_KRAJ.shp"
wa.map <- readOGR(dsn)

plot(wa.map)

precip2 <- as.data.frame(precip)

coordinates(precip2) <- ~ lon + lat
proj4string(precip2) <- proj4string(wa.map)


daily_rain_df <- precip2[!is.na(over(precip2, as(wa.map, "SpatialPolygons"))), ]

daily_rain_df <- as.data.frame(daily_rain_df)

ggplot() + geom_raster(data = daily_rain_df, aes(x=lon, y = lat, fill=precipitation)) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1) +
  theme_bw()
