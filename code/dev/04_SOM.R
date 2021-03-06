#source("code/dev/03_precipitation.R")
# references: https://www.r-bloggers.com/2014/02/self-organising-maps-for-customer-segmentation-using-r/
source("code/source.R")
precip <- readRDS("data/precipitation_summary_new.rds")

#----colors palletes----
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- inferno(10) # find better color pallet

#----formatting the data----
precip <- precip[,id := paste0(lon,",",lat)]
precip <- precip[,long := lon]

#----creating training data----
precip_train <- precip[,c(3,8,9,10,11,44,45,46,47,48)]# training data
precip_train_matrix <- as.matrix(scale(as.data.table(precip_train)))
#----creating som grid and model and justifying parameters----
som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")# choose grid for SOMs
som_model <- som(precip_train_matrix, 
                 grid=som_grid, 
                 rlen=10000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE) # rlen - choose number of interactions

# distance from each node’s weights to the samples represented by that node is reduced
# If the curve is continually decreasing, more iterations are required
plot(som_model, type="changes") 

# visualisation plot of the count of how many samples are mapped to each node on the map
# Empty nodes indicate that som grid size is too big for the number of samples. 
# Aim for at least 5-10 samples per node when choosing map size.
plot(som_model, type="count")

# distance between nodes how to estimate good distance
plot(som_model, type="dist.neighbours")

# plot of weight vectors. 
plot(som_model, type="codes")

#----analysis of received nodes with heatmaps----
# heatmap
# visualisation of the distribution of a single variable across the map
# normalised variables

plot(som_model, type = "property", property = som_model$codes[[1]][,4], 
     main=names(som_model$data)[[1]][,4], palette.name=coolBlueHotRed)

# heatmaps real variables 

tiff(filename="SOMs pictures/heatmap.tiff", width=2300, height=2000, res=300)
par(mfrow=c(4,3))
var_unscaled <- aggregate(precip_train[,1], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[1], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,2], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[2], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,3], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[3], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,4], by=list(som_model$unit.classif),
                                                           FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[4], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,5], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[5], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,6], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[6], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,7], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[7], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,8], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[8], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,9], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[9], 
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(precip_train[,10], by=list(som_model$unit.classif),
                          FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(precip_train)[10], 
     palette.name=coolBlueHotRed)
dev.off()
#---- creating matrix with k-means to justify the number of clusters---- 
mydata <- as.matrix(som_model$codes[[1]])


wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}


plot(wss) # number of clusters can be chosen

#----creating clusters----
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 10) # set number of clusters
# plot these results:
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

cluster_details <- data.frame(id=precip$id, 
                              cluster=som_cluster[som_model$unit.classif])

#----plot clusters on the map----
dsn <- "code/shapes/SPH_KRAJ.shp"
wa.map <- readOGR(dsn)

precip_to_plot <- merge(precip, cluster_details, by="id")
precip_to_plot$cluster <- as.factor(precip_to_plot$cluster)



# first experiment with clusters on the map, find better visualisation and colors

ggplot()+
  geom_tile(data=mappoints,aes(x=long,y=lat))+
  aes(long,lat,fill = factor(cluster))+
  coord_equal()+
  scale_fill_manual(values = pretty_palette)


ggplot(precip_to_plot)+
  geom_tile(aes(lon,lat))+
  aes(long,lat,fill = factor(cluster))+
  borders(wa.map,colour = "black")+
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = pretty_palette)+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))
dev.off()
                                                      