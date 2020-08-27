owda.nc <- nc_open("./data/raw/owda.nc")
owda.data <- ncvar_get(owda.nc)

dimnames(owda.data)[[1]] <- owda.nc$dim$time$vals
dimnames(owda.data)[[2]] <- owda.nc$dim$lat$vals 
dimnames(owda.data)[[3]] <- owda.nc$dim$lon$vals

kk <- nc_close(owda.nc)

owda <- data.table(melt(owda.data, 
             varnames = c("time", "lat", "lon"), 
             value.name = "scPDSI"))

owda <- owda[complete.cases(owda), ] #remove rows with NAs to make the data.table smaller
owda$lon <- owda$lon - 0.25 #lon correction

#Subseting in space and time
owda <- owda[owda$lat > 35.25 & owda$lat < 62.75 & 
               owda$lon > -4.25 & owda$lon < 36.25, ]
owda_1000 <- owda[time >= 992] 
setkey(owda_1000, time, lon, lat)

saveRDS(owda, "./data/OWDA.rds")
saveRDS(owda_1000, "./data/OWDA_1000.rds")


