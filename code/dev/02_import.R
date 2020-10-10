source("code/dev/functions.R")

instaldata_path="data/raw/sample/"
all_nc4 <- list.files(instaldata_path)
precip_summary_final <- data.table()

#----parallel execution settings----

cl <- makeCluster(detectCores() - 1) # no.cores
registerDoParallel(cl)

#call function to merge all nc4 files in one data table with parallel execution 
system.time(precip_summary_final <- foreach(i=1:length(all_nc4),.combine = rbind) %dopar%  merge_nc4(i))

stopCluster(cl)

saveRDS(precip_summary_final, "data/precipitation.rds")




