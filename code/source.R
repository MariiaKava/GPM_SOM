library(data.table)
library(ggplot2)
library(ncdf4) 
library(doParallel)
library(reshape2)
library(doSNOW)
library(utils)
library(mapview)
library(sf)
library(ggmap)
library(metR)
library(lubridate)
library(oce)
library(spData)


dload_path <- "data/raw/"

LAT <- c(48, 51)
