tiff(filename=paste0(path,"heatmapReal.tiff"), width=2300, height=2000, res=300)
par(mfrow=c(5,4),cex.main=1)
for(i in 3:7){
  color_by_var <- names(data)[i]
  color_variable <- data.table(data[, get(color_by_var)])
  color_by_var <- names(data)[i]
  interactive <- FALSE
  unit_colors <- aggregate(color_by, by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)
  color_by <- color_variable
  plot(som_model, type = "property", property=unit_colors[,2], main=color_by_var, palette.name=coolBlueHotRed) 
}
dev.off()

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


precip <- readRDS(paste0(path,"5-5_5_cluster_precip_summary.rds"))
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())
precip <- precip[,-1]
precip_l <- melt(precip, id.vars=c("lat", "lon","cluster"))
precip_l <- as.data.table(precip_l)
precip_stats <- precip_l[, .(mean = round(mean(value), 2),
                             sd = round(sd(value), 2),
                             min = round(min(value), 2),
                             max = round(max(value), 2),
                             CV = round(sd(value)/mean(value),2)), by = .(cluster,variable)]
saveRDS(precip_stats,paste0(path,"5-5_5_cluster_stats.rds"))
v <- "5-5_5_"
tiff(filename=paste0(path,v,"hist_ann_days.tiff"), width=2300, height=2000, res=300)
ggplot(precip, aes(ann_wetdays)) +
  geom_histogram(fill = "#97B8C2",bins = 100) +
  facet_wrap(~cluster, scales = 'free') +
  theme_bw()
dev.off()
count_obs_cluster <- precip[,(count=.N),by = .(cluster)]
tiff(filename=paste0(path,v,"hist_ann_days.tiff"), width=2300, height=2000, res=300)
ggplot(precip, aes(ann_wetdays)) +
  geom_histogram(fill = "#97B8C2",bins = 100) +
  facet_wrap(~cluster, scales = 'free') +
  theme_bw()
dev.off()

tiff(filename=paste0(path,v,"box_ann_days.tiff"), width=2300, height=2000, res=300)
ggplot(precip, aes(x=precip$cluster,y = precip$ann_wetdays))+
  geom_boxplot()
dev.off()
for (i in 3:57) {
  name <- names(precip)[i]
  p <- ggplot(precip, aes(x=precip$cluster,y = get(name)))+
    geom_boxplot()
  file_name = paste0(path,v,name,"_box.tiff")
  tiff(file_name)
  print(p)
  dev.off()
}
cluster=5
precip_cor_1 <- cor(precip[precip$cluster==cluster, c(3,8,9,10,11,28,29,30,31,32,37)], use = "pairwise.complete.obs")
to_plot <- melt(precip_cor_1)
tiff(filename=paste0(path,v,cluster,"cor_matrix.tiff"), width=2300, height=2000, res=300)
ggplot(data = to_plot, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(col = 'black') +
  scale_fill_gradient2(low = colset_4[4], 
                       high = colset_4[1], 
                       mid = colset_4[3],
                       midpoint = 0.5,
                       limits = c(-0.1, 1)) +
  geom_text(aes(label = round(value, 1))) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab(label = "") +
  ylab(label = "")
dev.off()