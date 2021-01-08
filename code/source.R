library(data.table)
library(ggplot2)
library(ncdf4) 
library(doParallel)
library(reshape2)
library(doSNOW)
library(utils)
library(sf)
library(spData)
library(rgdal)
library(viridis)
library(manipulate)



dload_path <- "data/raw/"

LAT <- c(48, 51)
