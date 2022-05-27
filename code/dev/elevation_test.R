source("code/source.R")

merge_dem <- function(i){
  
  el <- raster(paste0(instaldata_path,all_dem[i]))
  
  return(el)
  
}

instaldata_path="../data2/all/"
all_dem <- list.files(instaldata_path)

DEM <- data.table()


#----parallel execution settings----

cl <- makeCluster(6,outfile="") # no.cores
registerDoSNOW(cl)

#----progress bar----
pb <- txtProgressBar(min = 1, max = length(all_dem), style = 3)
progress<-function(n){
  setTxtProgressBar(pb,n)
}
opts<-list(progress=progress)

#----merging dataset----
#call function to merge all nc4 files in one data table with parallel execution 

system.time(DEM <- foreach(i=1:length(all_dem),.combine = merge,.packages=c("raster"),  .options.snow=opts, 
                           .errorhandling = 'remove') %dopar% {
                             errors<- tryCatch({
                               merge_dem(i)},
                               warning = function(war) {
                                 write.table(list("WARNING",all_nc4[i]),"errors.txt",append = TRUE,col.names = c("error type","file"))}, 
                               error = function(err) {
                                 write.table(list("ERROR",all_nc4[i]),"errors.txt",append = TRUE,col.names = c("error type","file")) },
                               finally = {
                                 
                               })
                             
                           })




close(pb)


stopCluster(cl)

saveRDS(DEM,"DEM.RDS")
library(raster)
dsn <- "code/shapes/SPH_KRAJ.shp"
el <- stack("DEM_czech.tif")
el1<- as.data.frame(el)
el1[1:10,]

wa.map <- readOGR(dsn)

plot(DEM)
plot(wa.map,add=TRUE)

el <- raster("DEM_czech.tif")
extend(dsn,wa.map)

el2 <- raster(el2)

r2 <- crop(el, wa.map)
r3 <- mask(r2,wa.map)

plot(el)

plot(r3)
plot(wa.map,add=TRUE)
writeRaster(DEM,"DEM_czech.tif")



