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