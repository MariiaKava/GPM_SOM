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
library(kohonen)
library(dplyr)
library(xtable)
library(raster)
library(gridExtra)
library(dplyr)
library(colorspace)



dload_path <- "data/raw/"

LAT <- c(48, 51)
