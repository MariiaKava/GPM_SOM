path <- "results/filled_missing_datetime/threshold_precipInt_01/SOM/annual_variables/EDA_5-5_5(s)/"
path2 <- "results/filled_missing_datetime/threshold_precipInt_01/SOM/seasonal_variables/"
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())
precip <- readRDS(paste0(path2,"5-5_5_cluster_precip_summary.rds"))
precip_05h <- readRDS("results/filled_missing_datetime/threshold_precipInt_01/daily_precip.rds")
precip <- precip[,-1]
precip_05_cluster <- merge(precip_05h,precip[,c(1,2,59)],all.x=TRUE)

precip_stats <- precip_05_cluster[, .(mean = round(mean(precipitation), 2),
                             sd = round(sd(precipitation), 2),
                             min = round(min(precipitation), 2),
                             max = round(max(precipitation), 2),
                             CV = round(sd(precipitation)/mean(precipitation),2)), by = .(cluster)]
saveRDS(precip_stats,paste0(path,"5-5_5_cluster_stats.rds"))

tiff(filename=paste0(path,"daily_precip_hist.tiff"), width=2300, height=2000, res=300)
ggplot(precip_05_cluster, aes(precipitation)) +
  geom_histogram(fill = "#97B8C2",bins=30) +
  facet_wrap(~cluster, scales = 'free') +
  theme_bw()
dev.off()
precip_monthly <- precip_05_cluster[,.(precipitation=sum(precipitation)),by=.(cluster,month,year)]

tiff(filename=paste0(path,"monthly_precip_box.tiff"), width=2300, height=2000, res=300)
ggplot(precip_monthly, aes(x = factor(month), y = precipitation)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~cluster, scales = 'free') + 
  theme_bw()
dev.off()

precip_ann <- precip_05_cluster[, .(precipitation = mean(precipitation)), by = .(year, cluster)]

tiff(filename=paste0(path,"ann_precip.tiff"), width=2300, height=2000, res=300)
ggplot(precip_ann, aes(x = year, y = precipitation)) +
  geom_line(col =  colset_4[1]) +
  geom_point(col = colset_4[1]) + 
  facet_wrap(~cluster, scales = 'free') +
  theme_minimal()
dev.off()

precip_ann[, precip_norm := scale(precipitation), by = cluster]
n_stations <- nrow(precip_stats)

tiff(filename=paste0(path,"comp_clusters.tiff"), width=2300, height=2000, res=300)
ggplot(precip_ann, aes(x = year, y = precip_norm, col = cluster)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  theme_bw()
dev.off()

precip_month_mat <- dcast(precip_monthly, year+month ~ cluster)
precip_month_cor <- cor(precip_month_mat[,-c(1,2)], use = "pairwise.complete.obs")
to_plot <- melt(precip_month_cor)

tiff(filename=paste0(path,"cor_matrix.tiff"), width=2300, height=2000, res=300)
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
