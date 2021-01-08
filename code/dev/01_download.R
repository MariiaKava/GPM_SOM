source("../source.R")


links_file <- "subset_GPM_3IMERGHH_06_20200829_061141.txt"

file <- paste0(dload_path, links_file)

url <- scan(file, what = list(""), flush = TRUE)
urls <- unlist(url)
#t = basename(urls)
names = substr(urls, 228, 280) #subset the character for naming the files
library(httr)
for (i in 1:length(urls)){
  GET(as.character(urls[i]),
      authenticate("MariiaKava", "JtCZFvDhRrLy7C9"),
      write_disk(path = paste0("../../data/raw/",names[i]), overwrite = T))
}
