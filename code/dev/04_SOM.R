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
set.seed(1108)
rows <- sample(nrow(precip))
precip <- precip[rows, ]
#----creating training data----
#precip_train <- precip[,c(3,8,9,10,11,28,29,30,31,32,37)]# training data for annual variables

precip_train <- precip[,-c(1,2,3,8,9,10,11,28,29,30,31,32,37,58,59)]# training data for seasonal variables 
#precip_train <- precip[,-c(1,2,58,59)]# training data for all variables
precip_train_matrix <- as.matrix(scale(as.data.table(precip_train),center=TRUE,scale=TRUE)) # standardized vectors
path <- "results/SOM/final/"
#----creating som grid and model and justifying parameters----
#som_grid <- somgrid(xdim = 3, ydim=3, topo="hexagonal")# choose grid for SOMs
# som_model <- som(precip_train_matrix, 
#                  grid=som_grid, 
#                  rlen=1000000, 
#                  alpha=c(0.05,0.01), 
#                  keep.data = TRUE) # rlen - choose number of interactions
som_model3 <- som(precip_train_matrix, 
                      grid=somgrid(xdim = 3, ydim=3, topo="hexagonal"),
                      rlen=100000, 
                      mode = "pbatch", 
                      cores = 6)
som_model4 <- som(precip_train_matrix, 
                  grid=somgrid(xdim = 4, ydim=4, topo="hexagonal"),
                  rlen=100000, 
                  mode = "pbatch", 
                  cores = 6)
som_model5 <- som(precip_train_matrix, 
                  grid=somgrid(xdim = 5, ydim=5, topo="hexagonal"),
                  rlen=100000, 
                  mode = "pbatch", 
                  cores = 6)
# different plots -----
v <- "100000_3-3"
cluster_details <- data.frame(id=precip$id,
                              cluster=som_model3$unit.classif)
#----plot clusters on the map----
dsn <- "code/shapes/SPH_KRAJ.shp"
wa.map <- readOGR(dsn)
precip_to_plot <- merge(precip, cluster_details, by="id")
precip_to_plot$cluster <- as.factor(precip_to_plot$cluster)
# map
png(filename=paste0(path,v,"_cluster_map.png"), width=2300, height=1650, res=300)
ggplot(precip_to_plot)+
  geom_tile(aes(lon,lat))+
  aes(long,lat,fill = factor(cluster))+
  borders(wa.map,colour = "black")+
  coord_quickmap()+
  scale_fill_manual(values = rainbow_hcl(9))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(2, 1, 2, 1, "cm"),
        plot.background = element_rect(
            fill = "white",
            colour = "black",
            size = 1
          ),
        legend.position = "none"
        )
dev.off()

v <- "100000_4-4"
cluster_details <- data.frame(id=precip$id,
                              cluster=som_model4$unit.classif)
precip_to_plot <- merge(precip, cluster_details, by="id")
precip_to_plot$cluster <- as.factor(precip_to_plot$cluster)
# map
png(filename=paste0(path,v,"_cluster_map.png"), width=2300, height=1650, res=300)
ggplot(precip_to_plot)+
  geom_tile(aes(lon,lat))+
  aes(long,lat,fill = factor(cluster))+
  borders(wa.map,colour = "black")+
  coord_quickmap()+
  scale_fill_manual(values = rainbow_hcl(16))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(2, 1, 2, 1, "cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "black",
          size = 1
        ),
        legend.position = "none"
  )
dev.off()

v <- "100000_5-5"
cluster_details <- data.frame(id=precip$id,
                              cluster=som_model5$unit.classif)
precip_to_plot <- merge(precip, cluster_details, by="id")
precip_to_plot$cluster <- as.factor(precip_to_plot$cluster)
# map
png(filename=paste0(path,v,"_cluster_map.png"), width=2300, height=1650, res=300)
ggplot(precip_to_plot)+
  geom_tile(aes(lon,lat))+
  aes(long,lat,fill = factor(cluster))+
  borders(wa.map,colour = "black")+
  coord_quickmap()+
  scale_fill_manual(values = rainbow_hcl(25))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(2, 1, 2, 1, "cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "black",
          size = 1
        ),
        legend.position = "none"
  )
dev.off()

# distance from each node’s weights to the samples represented by that node is reduced
# If the curve is continually decreasing, more iterations are required
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
# heatmaps real variables -----
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
mydata3 <- as.matrix(som_model3$codes[[1]])
mydata4 <- as.matrix(som_model4$codes[[1]])
mydata5 <- as.matrix(som_model5$codes[[1]])


wss3 <- (nrow(mydata3)-1)*sum(apply(mydata3,2,var))
wss4 <- (nrow(mydata4)-1)*sum(apply(mydata4,2,var)) 
wss5 <- (nrow(mydata5)-1)*sum(apply(mydata5,2,var)) 
for (i in 1:(nrow(mydata3)-1)) {
  wss3[i] <- sum(kmeans(mydata3, centers=i)$withinss)
}
for (i in 1:(nrow(mydata4)-1)) {
  wss4[i] <- sum(kmeans(mydata4, centers=i)$withinss)
}
for (i in 1:(nrow(mydata5)-1)) {
  wss5[i] <- sum(kmeans(mydata5, centers=i)$withinss)
}
wss3 <- as.data.table(wss3)
wss3 <- wss3[,clusters := c(1:8)]
wss3 <- wss3[,grid := "3*3"]

wss4 <- as.data.table(wss4)
wss4 <- wss4[,clusters := c(1:15)]
wss4 <- wss4[,grid := "4*4"]

wss5 <- as.data.table(wss5)
wss5 <- wss5[,clusters := c(1:24)]
wss5 <- wss5[,grid := "5*5"]

wss <- rbind(wss3,wss4,wss5,use.names = FALSE)

tiff(filename=paste0(path,v,"noClusters.tiff"), width=2300, height=2000, res=300)
ggplot(wss)+
      geom_point(aes(clusters,wss3,colour = grid)) +
      theme_minimal() +
      ylab("Within cluster sum of squares") +
      xlab("Number of cluster") +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      plot.background = element_rect(
            colour = "black",
            size = 1
          ))
dev.off()
#----creating clusters----
## use hierarchical clustering to cluster the codebook vectors
ncluster <- 6
som_cluster3 <- cutree(hclust(dist(som_model3$codes[[1]])), ncluster)
som_cluster4 <- cutree(hclust(dist(som_model4$codes[[1]])), ncluster)
som_cluster5 <- cutree(hclust(dist(som_model5$codes[[1]])), ncluster)# set number of clusters

dist(som_model3$codes[[1]])
# plot these results:
# tiff(filename=paste0(path,v,ncluster,"_cluster_result.tiff"), width=2300, height=2000, res=300)
# plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters") 
# add.cluster.boundaries(som_model, som_cluster)
# dev.off()
cluster_details3 <- data.frame(id=precip$id, 
                              cluster=som_cluster3[som_model3$unit.classif])
cluster_details4 <- data.frame(id=precip$id, 
                               cluster=som_cluster4[som_model4$unit.classif])
cluster_details5 <- data.frame(id=precip$id, 
                               cluster=som_cluster5[som_model5$unit.classif])
#----plot clusters on the map----
dsn <- "code/shapes/SPH_KRAJ.shp"
wa.map <- readOGR(dsn)
precip_to_plot3 <- merge(precip, cluster_details3, by="id")
precip_to_plot4 <- merge(precip, cluster_details4, by="id")
precip_to_plot5 <- merge(precip, cluster_details5, by="id")

precip_to_plot3$cluster <- as.factor(precip_to_plot3$cluster)
precip_to_plot4$cluster <- as.factor(precip_to_plot4$cluster)
precip_to_plot5$cluster <- as.factor(precip_to_plot5$cluster)

saveRDS(precip_to_plot3,paste0(path,ncluster,"_3-3_cluster_precip_summary.rds"))
saveRDS(precip_to_plot4,paste0(path,ncluster,"_4-4_cluster_precip_summary.rds"))
saveRDS(precip_to_plot5,paste0(path,ncluster,"_5-5_cluster_precip_summary.rds"))


# map
png(filename=paste0(path,ncluster,"_3-3_cluster_map.png"), width=2300, height=1675, res=300)
ggplot() + 
  geom_tile(data = precip_to_plot3, aes(x=lon, y = lat, fill=cluster)) + 
  borders(wa.map,colour = "black")+
  coord_quickmap()+
  scale_fill_manual(values = rainbow_hcl(ncluster))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(2, 1, 2, 1, "cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "black",
          size = 1
        ),
        legend.position = "none"
  )
dev.off()

png(filename=paste0(path,ncluster,"_4-4_cluster_map.png"), width=2300, height=1675, res=300)
ggplot() + 
  geom_tile(data = precip_to_plot4, aes(x=lon, y = lat, fill=cluster)) + 
  borders(wa.map,colour = "black")+
  coord_quickmap()+
  scale_fill_manual(values = rainbow_hcl(ncluster))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(2, 1, 2, 1, "cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "black",
          size = 1
        ),
        legend.position = "none"
  )
dev.off()

png(filename=paste0(path,ncluster,"_5-5_cluster_map.png"), width=2300, height=1675, res=300)
ggplot() + 
  geom_tile(data = precip_to_plot5, aes(x=lon, y = lat, fill=cluster)) + 
  borders(wa.map,colour = "black")+
  coord_quickmap()+
  scale_fill_manual(values = rainbow_hcl(ncluster))+
  scale_shape_manual(values=c(1,2,3,4,5,6))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(2, 1, 2, 1, "cm"),
  )
dev.off()




