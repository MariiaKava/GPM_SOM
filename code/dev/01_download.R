setwd("C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_download")

fil <- "C:/Users/rkpra/OneDrive/Documents/R_projects/GPM_download/subset_GPM_3IMERGDF_06_20200826_193405.txt"
url <- scan(fil, what = list(""), flush = TRUE)
urls <- unlist(url)
#t = basename(urls)
names = substr(urls, 228, 280) #subset the character for naming the files
library(httr)
for (i in 1:length(urls)){
  GET(as.character(urls[i]),
      authenticate("", ""),
      write_disk(path = names[i], overwrite = T))
}
