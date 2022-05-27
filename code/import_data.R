source("code/dev/functions.R")

instaldata_path="data/raw/data_new/"
all_nc4 <- list.files(instaldata_path)

precip_summary_final <- data.table()

#----parallel execution settings----

cl <- makeCluster(4,outfile="/dev/null") # no.cores
registerDoSNOW(cl)

#----progress bar----
pb <- txtProgressBar(min = 1, max = length(all_nc4), style = 3)
progress<-function(n){
  setTxtProgressBar(pb,n)
}
opts<-list(progress=progress)

#----merging dataset----
#call function to merge all nc4 files in one data table with parallel execution 
system.time(precip_summary_final <- foreach(i=1:length(all_nc4),.combine = rbind,  .options.snow=opts, 
                                                      .errorhandling = 'remove') %dopar% {
                                                        errors<- tryCatch({
                                                          merge_nc4(i)},
                                                          warning = function(war) {
                                                            write.table(list("WARNING",all_nc4[i]),"errors.txt",append = TRUE,col.names = c("error type","file"))}, 
                                                          error = function(err) {
                                                            write.table(list("ERROR",all_nc4[i]),"errors.txt",append = TRUE,col.names = c("error type","file")) },
                                                          finally = {
                                                            
                                                          })
                                                        
                                                      })




close(pb)


stopCluster(cl)



saveRDS(precip_summary_final, "data/precipitation_0122_with_zeros.rds")





