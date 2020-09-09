source("code/source.R")





data_path="data/raw/sample/"
all_nc4 <- list.files(data_path)
precip_summary_final <- data.table()


for (i in 1:length(all_nc4)){

precipitation_nc <- nc_open(paste0(data_path,all_nc4[i]))
precipitation_data <- ncvar_get(precipitation_nc)
date <- as.Date("1970-01-01 00:00")+ precipitation_nc$dim$time$vals/60/60/24



dimnames(precipitation_data)[[1]] <- precipitation_nc$dim$lat$vals
dimnames(precipitation_data)[[2]] <- precipitation_nc$dim$lon$vals



kk <- nc_close(precipitation_nc)


precip_summary <- data.table(melt(precipitation_data,
                        varnames = c("lat", "lon"),
                        value.name = "Precipitation"))

precip_summary <- precip_summary[complete.cases(precip_summary), ] 



date <- array(format(date,"%d/%m/%Y"),length(precip_summary$lat))
precip_summary <- cbind(precip_summary,date)
precip_summary$Precipitation[precip_summary$Precipitation<1]<-0 #Set all values with precipitation < 1mm to 0


precip_summary_final <- rbind(precip_summary,precip_summary_final,use.names=FALSE)


}


saveRDS(precip_summary_final, "data/precipitation.rds")


