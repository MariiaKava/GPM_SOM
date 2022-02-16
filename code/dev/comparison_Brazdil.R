source("code/source.R")
path <- "results/filled_missing_datetime/threshold_precipInt_02/"
precip_summary <- readRDS(paste0(path,"precipitation_summary.rds"))

precip_totals <- matrix(c(mean(precip_summary$annual_precip), 
                (sd(precip_summary$annual_precip)/mean(precip_summary$annual_precip))*100,
                mean(precip_summary$avg_DJF_precip),
                (sd(precip_summary$avg_DJF_precip)/mean(precip_summary$avg_DJF_precip)*100),
                mean(precip_summary$avg_MAM_precip),
                (sd(precip_summary$avg_MAM_precip)/mean(precip_summary$avg_MAM_precip)*100),
                mean(precip_summary$avg_JJA_precip),
                (sd(precip_summary$avg_JJA_precip)/mean(precip_summary$avg_JJA_precip)*100),
                mean(precip_summary$avg_SON_precip),
                (sd(precip_summary$avg_SON_precip)/mean(precip_summary$avg_SON_precip)*100)),ncol=1, byrow=TRUE)
rownames(precip_totals) <- c('ann_mean(mm)','ann_sv','DJF_mean(mm)','DJF_sv(%)','MAM_mean(mm)',
                   'MAM_cv(%)','JJA_mean(mm)','JJA_cv(%)','SON_mean(mm)','SON_cv(%)')
colnames(precip_totals)<- c('value')
precip_totals <- as.data.frame(precip_totals)
precip_totals$value <- round(precip_totals$value,digit=2)

png(paste0(path,"precip_totals_comp_table1_p6.png"))
p<-tableGrob(precip_totals)
grid.arrange(p)
dev.off()

precip_days <- matrix(c(mean(precip_summary$ann_wetdays),
                        sd(precip_summary$ann_wetdays),
                        mean(precip_summary$wet_days_winter),
                        sd(precip_summary$wet_days_winter),
                        mean(precip_summary$wet_days_autumn),
                        sd(precip_summary$wet_days_autumn),
                        mean(precip_summary$wet_days_summer),
                        sd(precip_summary$wet_days_summer),
                        mean(precip_summary$wet_days_spring),
                        sd(precip_summary$wet_days_spring)
                        ),ncol = 1, byrow = TRUE)
rownames(precip_days) <- c('ann_mean(days)','ann_sd','DJF_mean(days)','DJF_sd','MAM_mean(days)','MAM_sd','JJA_mean(days)','JJA_sd',
                           'SON_mean(days)','SON_sd')
colnames(precip_days)<- c('value')
precip_days<- as.data.frame(precip_days)
precip_days$value <- round(precip_days$value, digits=2)

png(paste0(path,"precip_days_comp_table4_p14.png"))
p<-tableGrob(precip_days)
grid.arrange(p)
dev.off()


precip_daily_amount <- readRDS(paste0(path,"daily_precip.rds"))



annual_precip_amount <- precip_daily_amount[,.(annual_precip_amount=sum(precipitation)),by=.(lat,lon,month,year)]
ann_precipVar <- annual_precip_amount[,.(precipitation=mean(annual_precip_amount)),by = .(month,year)]

# tiff(filename=paste0(path,"Ann_monthly_precipVar.tiff"), width=2300, height=2000, res=300)
# ggplot(data=annual_precip_amount,aes(x=year,y=precipitation))+
#   xlab("year") + ylab("precipitation(mm)")+
#   geom_point(size=2, shape=23) +
#   geom_text(
#     label=annual_precip_amount$year, 
#     nudge_x = 0.5, nudge_y = 0.5, 
#     check_overlap = T
#   )
# dev.off()

precip_2000_2010 <- annual_precip_amount[year>2000 & year<2011,]
precip_2011_2019 <- annual_precip_amount[year>2010 & year<2020,]

precip_2000_2010 <- precip_2000_2010[,.(annual_precip_amount=mean(annual_precip_amount)),by=.(month)]

precip_2011_2019 <- precip_2011_2019[,.(annual_precip_amount=mean(annual_precip_amount)),by=.(month)]

# precip_2000_2010$month[precip_2000_2010$annual_precip_amount==min(precip_2000_2010$annual_precip_amount)]
# min(precip_2000_2010$annual_precip_amount)
# precip_2000_2010$month[precip_2000_2010$annual_precip_amount==max(precip_2000_2010$annual_precip_amount)]
# 
# precip_2011_2019$month[precip_2011_2019$annual_precip_amount==min(precip_2011_2019$annual_precip_amount)]
# precip_2011_2019$month[precip_2011_2019$annual_precip_amount==max(precip_2011_2019$annual_precip_amount)]

precip_all <- annual_precip_amount[,.(annual_precip_amount=mean(annual_precip_amount)),by=.(month)]
# precip_all$month <- as.character(precip_all$month)

tiff(filename=paste0(path,"Ann_monthly_precipVar.tiff"), width=2300, height=2000, res=300)
ggplot(data=precip_all,aes(x=precip_all$month,y=precip_all$annual_precip_amount))+
  xlab("month") + ylab("precipitation(mm)")+
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))
dev.off()

tiff(filename=paste0(path,"Ann_monthly_precipVar2011-2019.tiff"), width=2300, height=2000, res=300)
ggplot(data=precip_2011_2019,aes(x=precip_2011_2019$month,y=precip_2011_2019$annual_precip_amount))+
  xlab("month") + ylab("precipitation(mm)") +
  ggtitle("Pecipitation 2011-2019") +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))
dev.off()

tiff(filename=paste0(path,"Ann_monthly_precipVar2000-2019.tiff"), width=2300, height=2000, res=300)
ggplot(data=precip_2000_2010,aes(x=precip_2000_2010$month,y=precip_2000_2010$annual_precip_amount))+
  xlab("month") + ylab("precipitation(mm)") +
  ggtitle("Pecipitation 2001-2010") +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))
dev.off()

# min_max_months <- matrix(c('Apr','Aug','Nov','Dec'),ncol = 2,byrow = TRUE)
# rownames(min_max_months)<- c('2001-2010','2011-2019')
# colnames(min_max_months) <- c('Min','Max')
# min_max_months

# png("results/zero_threshold/min_max_months_comp_table2_p13.png")
# p<-tableGrob(min_max_months)
# grid.arrange(p)
# dev.off()
