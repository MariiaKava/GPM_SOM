#source("code/dev/03_precipitation.R")
# references: https://www.r-bloggers.com/2014/02/self-organising-maps-for-customer-segmentation-using-r/
source("code/source.R")
precip <- readRDS("results/filled_missing_datetime/threshold_precipInt_01/precipitation_summary.rds")

#----colors palletes----
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- inferno(10)
#----formatting the data----
precip <- precip[,id := paste0(lon,",",lat)]
precip <- precip[,long := lon]
# shuffle observations
set.seed(42)
rows <- sample(nrow(precip))
precip <- precip[rows, ]
#----creating training data----
#precip_train <- precip[,c(3,8,9,10,11,28,29,30,31,32,37)]# training data for annual variables
precip_train <- precip[,-c(1,2,3,8,9,10,11,28,29,30,31,32,37,58,59)]# training data for seasonal variables 
#precip_train <- precip[,-c(1,2,58,59)]# training data for all variables
precip_train_matrix <- as.matrix(scale(as.data.table(precip_train),center=TRUE,scale=TRUE)) # standardized vectors
path <- "results/filled_missing_datetime/threshold_precipInt_01/SOM/seasonal_variables/"
#----creating som grid and model and justifying parameters----
som_grid <- somgrid(xdim = 5, ydim=5, topo="hexagonal")# choose grid for SOMs
# som_model <- som(precip_train_matrix, 
#                  grid=som_grid, 
#                  rlen=1000000, 
#                  alpha=c(0.05,0.01), 
#                  keep.data = TRUE) # rlen - choose number of interactions
som_model <- supersom(precip_train_matrix, 
                      grid=som_grid,
                      rlen=100000, 
                      mode = "pbatch", 
                      cores = 6)
# distance from each node’s weights to the samples represented by that node is reduced
# If the curve is continually decreasing, more iterations are required
v <- "100000_5-5_"
tiff(filename=paste0(path,v,"changes.tiff"), width=2300, height=2000, res=300)
plot(som_model, type="changes") 
dev.off()
# visualisation plot of the count of how many samples are mapped to each node on the map
# Empty nodes indicate that som grid size is too big for the number of samples. 
# Aim for at least 5-10 samples per node when choosing map size.
tiff(filename=paste0(path,v,"count.tiff"), width=2300, height=2000, res=300)
plot(som_model, type="count")
dev.off()
# distance between nodes how to estimate good distance
tiff(filename=paste0(path,v,"dist_neighbours.tiff"), width=2300, height=2000, res=300)
plot(som_model, type="dist.neighbours", palette.name= grey.colors)
dev.off()
# plot of weight vectors.
# tiff(filename=paste0(path,v,"codes.tiff"), width=2300, height=2000, res=300)
# plot(som_model, type="codes")
# dev.off()
#----analysis of received nodes with heatmaps----
# heatmap
# visualisation of the distribution of a single variable across the map
# normalised variables
# heatmaps real variables 
tiff(filename=paste0(path,v,"heatmapReal.tiff"), width=2300, height=2000, res=300)
par(mfrow=c(4,5),cex.main=0.6)
for (i in 1:18) {
  col <- names(precip_train)[i]
  var_unscaled <- aggregate(precip_train[,get(col)], by=list(som_model$unit.classif),
                            FUN=mean, simplify=TRUE)[,2]  
  plot(som_model, type = "property", property=var_unscaled, main=col, 
       palette.name=coolBlueHotRed)
}
dev.off()
dev.off()
tiff(filename=paste0(path,v,"heatmapReal2.tiff"), width=2300, height=2000, res=300)
par(mfrow=c(4,5),cex.main=0.6)
for (i in 19:37) {
  col <- names(precip_train)[i]
  var_unscaled <- aggregate(precip_train[,get(col)], by=list(som_model$unit.classif),
                            FUN=mean, simplify=TRUE)[,2]  
  plot(som_model, type = "property", property=var_unscaled, main=col, 
       palette.name=coolBlueHotRed)
}
dev.off()
dev.off()
tiff(filename=paste0(path,v,"heatmapReal3.tiff"), width=2300, height=2000, res=300)
par(mfrow=c(4,5),cex.main=0.6)
for (i in 37:55) {
  col <- names(precip_train)[i]
  var_unscaled <- aggregate(precip_train[,get(col)], by=list(som_model$unit.classif),
                            FUN=mean, simplify=TRUE)[,2]  
  plot(som_model, type = "property", property=var_unscaled, main=col, 
       palette.name=coolBlueHotRed)
}
dev.off()
dev.off()
#---- creating matrix with k-means to justify the number of clusters---- 
mydata <- as.matrix(som_model$codes[[1]])


wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 1:(nrow(mydata)-1)) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}

tiff(filename=paste0(path,v,"noClusters.tiff"), width=2300, height=2000, res=300)
plot(wss) # number of clusters can be chosen
dev.off()
#----creating clusters----
## use hierarchical clustering to cluster the codebook vectors
ncluster <- 5
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), ncluster) # set number of clusters
# plot these results:
# tiff(filename=paste0(path,v,ncluster,"_cluster_result.tiff"), width=2300, height=2000, res=300)
# plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters") 
# add.cluster.boundaries(som_model, som_cluster)
# dev.off()
cluster_details <- data.frame(id=precip$id, 
                              cluster=som_cluster[som_model$unit.classif])
#----plot clusters on the map----
# dsn <- "code/shapes/SPH_KRAJ.shp"
# wa.map <- readOGR(dsn)
precip_to_plot <- merge(precip, cluster_details, by="id")
precip_to_plot$cluster <- as.factor(precip_to_plot$cluster)
saveRDS(precip_to_plot,paste0(path,"5-5_5_cluster_precip_summary.rds"))
# map
tiff(filename=paste0(path,v,ncluster,"_cluster_map.tiff"), width=2300, height=2000, res=300)
ggplot(precip_to_plot)+
  geom_tile(aes(lon,lat))+
  aes(long,lat,fill = factor(cluster))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = rainbow_hcl(10))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))
dev.off()


