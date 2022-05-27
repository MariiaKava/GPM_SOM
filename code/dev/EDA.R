source("code/source.R")
source("code/dev/functions.R")
path <- "results/Cluster analysis/final/"
path2 <- "results/SOM/final/clusters/seasonal/"
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())
precip <- readRDS(paste0(path2,"6_4-4_cluster_precip_summary.rds"))
precip_05h <- readRDS("results/filled_missing_datetime/threshold_precipInt_01/daily_precip.rds")
precip <- precip[,-1]
precip_05_cluster <- merge(precip_05h,precip[,c(1,2,59)],all.x=TRUE)

dsn <- "code/shapes/SPH_KRAJ.shp"
wa.map <- readOGR(dsn,encoding = "UTF-8")
png(filename=paste0(path,ncluster,"_5-5_cluster_map.png"), width=2300, height=1675, res=300)
ggplot() + 
  geom_tile(data = precip, aes(x=lon, y = lat, fill=cluster)) + 
  borders(wa.map,colour = "black")+
  coord_quickmap()+
  scale_fill_manual(values = rainbow_hcl(6))+
  scale_shape_manual(values=c(1,2,3,4,5,6))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(2, 1, 2, 1, "cm"),
  )
dev.off()

# monthly precip box plot----
precip_monthly <- precip_05_cluster[,.(precipitation=sum(precipitation)),by=.(lat,lon,cluster,month,year)]
precip_monthly <- precip_monthly[,.(precipitation = mean(precipitation)), by = .(cluster,month,year)]
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

dat <- precip_monthly %>% tibble::rownames_to_column(var="outlier") %>% group_by(month,cluster) %>% mutate(is_outlier=ifelse(is_outlier(precipitation), precipitation, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)
dat <- as.data.table(dat)
outlier <- dat[dat$is_outlier!="NA",c(2,3,4,5)]

png(filename=paste0(path,"monthly_precip_box.png"), width=2300, height=2000, res=300)
ggplot(precip_monthly, aes(x = factor(cluster), y = precipitation)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~month, scales = 'fixed', ncol = 4) + 
  theme_bw() +
  xlab("Cluster") + ylab("Precipitation (mm)") + 
  theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16,face="bold"))
dev.off()
outlier <- outlier[order(outlier$cluster),]
print(xtable(outlier, include.rownames=FALSE))

outlier[,count := .N,by = cluster]
month <- outlier[,.(count = .N),by = month]
max(outlier$precipitation)

# monthly precip box plot CR----
# precip_monthly1 <- precip_monthly[,.(precipitation = mean(precipitation)), by = .(month,year)]
# tiff(filename=paste0(path,"monthly_precip_boxCR.tiff"), width=2300, height=2000, res=300)
# ggplot(precip_monthly1, aes(x = factor(month), y = precipitation)) +
#   geom_boxplot(fill = colset_4[4]) +
#   ggtitle("Monthly precipitation CR") +
#   theme_bw()
# dev.off()
#ann precip trend----
precip_ann <- precip_05_cluster[, .(precipitation = sum(precipitation)), by = .(lat,lon,year, cluster)]
precip_ann <- precip_ann[, .(precipitation = mean(precipitation)), by = .(year, cluster)]
tiff(filename=paste0(path,"ann_precip.tiff"), width=2300, height=2000, res=300)
ggplot(precip_ann, aes(x = year, y = precipitation)) +
  geom_line(col = colset_4[1])+
  geom_point(col = colset_4[1])+
  facet_wrap(~cluster, scales = 'fixed') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, aes(col = "lm")) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, aes(col = "loess")) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitation (mm)") +
  theme_bw() +
  scale_colour_manual(name="Method", values=c("slategrey","steelblue3"))
  #stat_regline_equation(label.x = 2014,label.y = 1250, aes(label = ..eq.label..))
make_precip_class(precip_05_cluster)
precip_ann <- precip_05_cluster[, .(precipitation = sum(precipitation)), by = .(lat,lon,year,cluster,precipitation_class)]
precip_ann <- precip_ann[, .(precipitation = mean(precipitation)), by = .(year, cluster,precipitation_class)]

#tiff(filename=paste0(path,"ann_precip.tiff"), width=2300, height=2000, res=300)
ggplot(precip_ann[precipitation_class=="very_heavy",], aes(x = year, y = precipitation)) +
  geom_line(col = colset_4[1])+
  geom_point(col = colset_4[1])+
  facet_wrap(~cluster, scales = 'fixed') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, aes(col = "lm")) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, aes(col = "loess")) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitation (mm)") +
  theme_bw() +
  scale_colour_manual(name="Method", values=c("slategrey","steelblue3"))



dev.off()
precip_freq <- precip_05_cluster
precip_freq$precipitation <- round(precip_freq$precipitation,1)
precip_freq <- precip_freq[precip_freq$precipitation>0.1,]
make_precip_class(precip_freq)
precip_freq <- precip_freq[,.(wetDays=.N),by = .(lat,lon,precipitation_class,cluster,year)]
precip_freq <- precip_freq[,.(ann_wetDays = round(mean(wetDays),0)), by = .(year,precipitation_class,cluster)]

ggplot(precip_freq[precip_freq$precipitation_class=="very_heavy",], aes(x = year, y = ann_wetDays)) +
  geom_line(col = colset_4[1])+
  geom_point(col = colset_4[1])+
  facet_wrap(~cluster, scales = 'fixed') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, aes(col = "lm")) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, aes(col = "loess")) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Annual preicipitation days") +
  theme_bw() +
  scale_colour_manual(name="Method", values=c("slategrey","steelblue3"))


# seasonal box plots----
precip_season <- precip_05_cluster[,.(precipitation=sum(precipitation)),by=.(lat,lon,cluster,season,year)]
precip_season <- precip_season[,.(precipitation=mean(precipitation)),by=.(cluster,season,year)]
precip_season[year < 2006, period := factor("2001-2005")]
precip_season[year > 2005 & year < 2011, period := factor("2006-2010")]
precip_season[year > 2010 & year < 2016, period := factor("2011-2015")]
precip_season[year > 2015, period := factor("2016-2020")]

ggplot(precip_season, aes(season, precipitation, fill = period)) +
  geom_boxplot() +
  facet_wrap(~cluster, scales = 'fixed') +
  scale_fill_manual(values = c(colset_4[4], colset_4[1],"steelblue3","slategrey")) +
  xlab(label = "Season") +
  ylab(label = "Precipitation (mm)") +
  theme_bw()


precip_ann[year < 2006, period := factor("2001-2005")]
precip_ann[year > 2005 & year < 2011, period := factor("2006-2010")]
precip_ann[year > 2010 & year < 2016, period := factor("2011-2015")]
precip_ann[year > 2015, period := factor("2016-2020")]

precip_ann[year < 2011, period := factor("2001-2011")]
precip_ann[year > 2010, period := factor("2011-2020")]

ggplot(precip_season, aes(cluster, precipitation, fill = period)) +
  geom_boxplot() +
  scale_fill_manual(values = c(colset_4[4], colset_4[1],"steelblue3","slategrey")) +
  xlab(label = "Cluster") +
  ylab(label = "Precipitation (mm)") +
  theme_bw()

ggplot(precip_ann, aes(cluster, precipitation, fill = period)) +
  geom_boxplot() +
  scale_fill_manual(values = c(colset_4[4], colset_4[1],"steelblue3","slategrey")) +
  xlab(label = "Cluster") +
  ylab(label = "Precipitation (mm)") +
  theme_bw()

precip_monthly[year < 2006, period := factor("2001-2005")]
precip_monthly[year > 2005 & year < 2011, period := factor("2006-2010")]
precip_monthly[year > 2010 & year < 2016, period := factor("2011-2015")]
precip_monthly[year > 2015, period := factor("2016-2020")]

ggplot(precip_monthly, aes(cluster, precipitation, fill = period)) +
  geom_boxplot() +
  facet_wrap(~month, scales = 'fixed') +
  scale_fill_manual(values = c(colset_4[4], colset_4[1],"steelblue3","slategrey")) +
  xlab(label = "Cluster") +
  ylab(label = "Precipitation (mm)") +
  theme_bw()



#tiff(filename=paste0(path,"season_precip_box.tiff"), width=2300, height=2000, res=300)
ggplot(precip_season, aes(x = factor(cluster), y = precipitation)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~season, scales = 'fixed',ncol = 2) + 
  xlab("Cluster") + ylab("Precipitation (mm)") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+
  theme_bw()
dev.off()

# #seasonal box plots CR----
# precip_season <- precip_season[,.(precipitation=mean(precipitation)),by=.(season,year)]
# tiff(filename=paste0(path,"season_precip_boxCR.tiff"), width=2300, height=2000, res=300)
# ggplot(precip_season, aes(x = factor(season), y = precipitation)) +
#   geom_boxplot(fill = colset_4[4]) +
#   ggtitle("Seasonal box plot CR")+
#   theme_bw()
# dev.off()

# precip_monthlyVar_CR <- precip_monthly[,.(precipitation = mean(precipitation)), by = .(month)]
# tiff(filename=paste0(path,"Ann_monthly_precipVar_CR.tiff"), width=2300, height=2000, res=300)
# ggplot(data=precip_monthlyVar_CR,aes(x=precip_monthlyVar_CR$month,y=precip_monthlyVar_CR$precipitation))+
#   xlab("month") + ylab("precipitation(mm)")+
#   geom_bar(stat="identity") +
#   ggtitle("Annual variance in mean monthly precipitation CR")+
#   scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))
# dev.off()


precip_monthlyVar_cl <- precip_monthly[,.(precipitation = mean(precipitation)), by = .(cluster,month)]
tiff(filename=paste0(path,"Ann_monthly_precipVar_cl.tiff"), width=2300, height=2000, res=300)
ggplot(data=precip_monthlyVar_cl,aes(x=precip_monthlyVar_cl$month,y=precip_monthlyVar_cl$precipitation))+
  xlab("month") + ylab("precipitation(mm)")+
  geom_bar(stat="identity") +
  facet_wrap(~cluster, scales = 'free') + 
  ggtitle("Annual variance in mean monthly precipitation by clusters")+
  scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))
dev.off()

#annual precipitation summary
precip_stats <- precip_ann[, .(mean = round(mean(precipitation), 2),
                                      sd = round(sd(precipitation), 2),
                                      min = round(min(precipitation), 2),
                                      max = round(max(precipitation), 2),
                                      CV = round(sd(precipitation)/mean(precipitation),2)), by = .(cluster)]
precip_stat_CR <- precip_ann[, .( cluster = "CR",
                                mean = round(mean(precipitation), 2),
                                 sd = round(sd(precipitation), 2),
                                 min = round(min(precipitation), 2),
                                 max = round(max(precipitation), 2),
                                 CV = round(sd(precipitation)/mean(precipitation),2)),]
precip_stats <- rbind(precip_stats,precip_stat_CR)
precip_stats <- precip_stats[order(precip_stats$cluster),]
xtable(precip_stats)
png(paste0(path,"precip_stats.png"))
p<-tableGrob(precip_stats)
grid.arrange(p)
dev.off()
# merging with elevation
elevation <- read_feather("data/elevation.feather")
colnames(elevation) <- c("lon","lat","elevation")
precip_daily <- merge(precip_05_cluster,elevation,all.x=TRUE )
# 
# dsn <- "code/shapes/SPH_KRAJ.shp"
# wa.map <- readOGR(dsn)
clusters_el <- precip_daily[,.(precipitation = mean(precipitation)),by = .(lat,lon,cluster,elevation)]
precip_stats_el <- clusters_el[, .(mean = round(mean(elevation), 2),
                               sd = round(sd(elevation), 2),
                               min = round(min(elevation), 2),
                               max = round(max(elevation), 2),
                               CV = round(sd(elevation)/mean(elevation),2)), by = .(cluster)]
precip_stats_CR_el <- clusters_el[, .(cluster = "CR",
                                    mean = round(mean(elevation), 2),
                                   sd = round(sd(elevation), 2),
                                   min = round(min(elevation), 2),
                                   max = round(max(elevation), 2),
                                   CV = round(sd(elevation)/mean(elevation),2))]
precip_stats_el <- rbind(precip_stats_el,precip_stats_CR_el)
png(paste0(path,"elevation_stats.png"))
p<-tableGrob(precip_stats_el)
grid.arrange(p)
dev.off()
  
el_cl <-  as.data.frame(clusters_el)
coordinates(el_cl) <- ~ lon + lat
proj4string(el_cl) <- proj4string(wa.map)

daily_rain_df <- precip_summary_pol[!is.na(over(precip_summary_pol, as(wa.map, "SpatialPolygons"))), ]
daily_rain_df <- as.data.frame(daily_rain_df)

el <- raster("results/DEM/DEM_czech.tif")

precip <- precip[,pch:=0]
precip$pch[precip$cluster==1] <- 0
precip$pch[precip$cluster==2] <- 1
precip$pch[precip$cluster==3] <- 2
precip$pch[precip$cluster==4] <- 3
precip$pch[precip$cluster==5] <- 4
precip$pch[precip$cluster==6] <- 5

precip <- precip[,color:=""]
precip$color[precip$cluster==1] <- "violetred4"
precip$color[precip$cluster==2] <- "maroon"
precip$color[precip$cluster==3] <- "tomato3"
precip$color[precip$cluster==4] <- "palevioletred3"
precip$color[precip$cluster==5] <- "slateblue3"
precip$color[precip$cluster==6] <- "royalblue3"


fun <- function() {
  points(precip$lon, precip$lat, pch=as.integer(precip$pch), col=precip$color,lwd = 1)
  legend( x="topright", 
          title="Cluster",
          legend=c("1","2","3","4","5","6"), 
          col=c("violetred4","maroon","tomato3","palevioletred3","slateblue3","royalblue3"),
          lwd=2,
          lty=c(NA,NA,NA,NA,NA,NA),
          pch=c(0,1,2,3,4,5),bty = "n", cex = 0.8)
}
fun2 <- function(){
  plot(wa.map,add=TRUE)
  text(wa.map, wa.map@data$NAZEV_NUTS, cex=0.7, pos=1, col="black") 
}
my_window <- extent(12.05,19.05,48.55,51.05)
plot(my_window, col=NA,axes=FALSE,ylab='',xlab='')
raster::plot(el,col=terrain.colors(255),addfun= fun2,axes=FALSE,
             box=FALSE,
             legend.args = list(text = 'Elevation (m)', side = 3,
                                font = 2, cex = 0.8))
#addRasterLegend(el, location=c(14.5,17.5,48,47.9), nTicks = 6,side = 1, ramp = terrain.colors(255))
axis(side = 2)
axis(side = 1)



plot(precip$lon, precip$lat,
     pch = 19,
     col = factor(precip$cluster), inherit.axes = TRUE, add=TRUE)

plot()
plot()
ggplot(el)
  geom_raster(el)+
  coord_fixed(ratio = 1) +
  theme_bw() 

dev.off()

tiff(filename=paste0(path,"elevation_box.tiff"), width=2300, height=2000, res=300)
ggplot(clusters_el, aes(x = factor(cluster), y = elevation)) +
  geom_boxplot(fill = colset_4[4]) +
  theme_bw()
dev.off()

ggplot(precip_monthly[precip_monthly$month==1,], aes(x = year, y = precipitation)) +
  geom_line(col =  colset_4[1]) +
  geom_point(col = colset_4[1]) + 
  geom_smooth(inherit.aes = TRUE,method="lm",colour = "grey",show.legend = TRUE,se = F) +
  geom_text(label = round(lm(precip_monthly$precipitation ~ precip_monthly$year)$coefficients[2],2))+
  facet_wrap(~cluster, scales = 'fixed',ncol = 2) +
  xlab("Year") + ylab("Precipitation (mm)") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+
  theme_bw()

ggplot(precip_monthly[precip_monthly$month==1,], aes(x = year, y = precipitation,color = precip_monthly$cluster)) +
  geom_line(col =  colset_4[1]) +
  geom_point(col = colset_4[1]) + 
  geom_smooth(inherit.aes = TRUE,method="lm",colour = "grey",show.legend = TRUE,se = F) +
  facet_wrap(~cluster, scales = 'fixed',ncol = 2) +
  xlab("Year") + ylab("Precipitation (mm)") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+
  theme_bw()

ggplot(precip_monthly[precip_monthly$month==1,],aes(x = year, y = precipitation)) + 
  geom_point(col = colset_4[1]) + 
  geom_line(col =  colset_4[1]) +
  geom_smooth(method = "lm", se=FALSE,col = "darkgrey") +
  stat_regline_equation(label.y = 200, aes(label = ..eq.label..)) +
  facet_wrap(~cluster)+
  xlab("Year") + ylab("Precipitation (mm)") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+
  theme_bw()

corr_matrix <- cbind(precip_stats[,1:2],precip_stats_el[,2])
corr_matrix <- corr_matrix[-7,]
colnames(corr_matrix) <- c("cluster", "avg_ann_precip","avg_altitude")
plot(corr_matrix$avg_ann_precip,corr_matrix$avg_altitude)
text(corr_matrix,labels = corr_matrix$cluster)

plot(corr_matrix$avg_ann_precip ~corr_matrix$avg_altitude, col="lightblue", pch=19, cex=2,data=corr_matrix)

abline(mod, col="red", lwd=3)

### this add the labels to the points using their rownames
### font = 2 is bold

text(corr_matrix$avg_ann_precip ~corr_matrix$avg_altitude, labels=corr_matrix$cluster,data=corr_matrix, cex=0.9, font=2)
