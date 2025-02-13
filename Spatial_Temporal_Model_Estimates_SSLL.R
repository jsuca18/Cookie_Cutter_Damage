#####Predicting#
library(fmsb)
library(plyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
setwd("~/Documents/Cookie_Cutter/Code_for_Model")
source("BRT_Eval_Function_JJS.R")
df<-readRDS("~/Documents/Cookie_Cutter/Cookie_Cutter_Full_DF_GLORYS_thru_2022.rds")

df$HKS_FLT<-df$HKS/df$FLTS
df$LITE_HK<-df$NUM_LITE_DEVICES/df$HKS
df<-df[df$FISHERY=="SSLL",]
#df<-df[df$HKS_FLT<=8,]
df<-df[df$HKS_FLT!="Inf",]
df<-df[!is.na(df$HKS_FLT),]

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
df[is.nan(df)] <- NA
df$Random<-rnorm(nrow(df))
df$bait_code<-as.factor(df$bait_code)
df$vessel_name<-as.factor(df$vessel_name)
df$year_end<-as.numeric(df$year_end)
df$permit_num<-as.factor(df$permit_num)
df$year<-as.numeric(df$year)
df$month<-as.numeric(df$month)
df$Full_Soak_Time_Hrs<-as.numeric(df$haul_end_datetime-df$set_begin_datetime)

CC_bite_DF<-df


CC_bite_DF$Fish_Count<-rowSums(CC_bite_DF[,20:183])
CC_bite_DF$Fish_per_hk<-CC_bite_DF$Fish_Count/CC_bite_DF$HKS
CC_bite_DF<-CC_bite_DF[CC_bite_DF$Fish_per_hk<1,]
CC_bite_DF$FISHERY<-as.factor(CC_bite_DF$FISHERY)

CC_bite_DF<-CC_bite_DF[,-which(names(CC_bite_DF) %in% c("mld"))]

Used_preds<-c("Fish_Count","bait_code", "SST","log_chla","julian_end", "year_end","lunar_rad","SST_SD","SSH","SSH_SD","SSS","SSS_SD", "Random","PDO","MEI", "Current_Speed","MLD","NPGO", "FLTLN_LEN","BRNCHLN_LEN", "HKS_FLT", "LITE_HK","day_hrs","night_hrs")


Preds<-which(colnames(CC_bite_DF) %in% Used_preds)
SSLL_CC_BRT_Models<-list()
CC_bite_DF$CC_Bite_PA[CC_bite_DF$CC_bites>0]<-1
CC_bite_DF$CC_Bite_PA[CC_bite_DF$CC_bites==0]<-0
CC_bite_DF$year_end<-as.numeric(as.character(CC_bite_DF$year_end))
#CC_bite_DF<-CC_bite_DF[CC_bite_DF$year_end<=2015,]
CC_bite_DF<-CC_bite_DF[CC_bite_DF$HKS_FLT<50,]

Cookie_Cutter_SSLL_BRT_Models<-readRDS( "PA_SSLL_Cookie_Cutter_Bite_BRT_Models_2025.rds")
Cookie_Cutter_SSLL_Estimates<-matrix(,nrow=nrow(CC_bite_DF), ncol=50)
#df$vessel_name<-NA
#df$permit_num<-NA
for (i in 1:1){
  for (k in 1:50){
    Cookie_Cutter_SSLL_Estimates[,k]<-predict.gbm(Cookie_Cutter_SSLL_BRT_Models[[1]][[k]], CC_bite_DF,
                                         n.trees=Cookie_Cutter_SSLL_BRT_Models[[1]][[k]]$gbm.call$best.trees, type="response")
    print(paste("completed", k, "of 50"))}
}

saveRDS(Cookie_Cutter_SSLL_Estimates, "CC_SSLL_Model_Estimate_2025.rds")

Cookie_Cutter_SSLL_Est<-readRDS("CC_SSLL_Model_Estimate_2025.rds")

Mean_Cookie_Cutter_SSLL<-rowMeans(Cookie_Cutter_SSLL_Est, na.rm=T)

df_predictions<-cbind(CC_bite_DF, Mean_Cookie_Cutter_SSLL)
df_predictions$Count<-1

Monthy_Estimates<-ddply(df_predictions, .(year_end, month), summarize, Real=mean(CC_Bite_PA, na.rm=T), Modeled=mean(Mean_Cookie_Cutter_SSLL, na.rm=T),Total_Sets=sum(Count), Mean_Night_Hrs=mean(night_hrs, na.rm=T), Mean_Fish=mean(Fish_Count, na.rm=T), Hooks=mean(HKS, na.rm=T), Mean_Day_Hrs=mean(day_hrs, na.rm=T))
Monthy_Estimates$Day<-15
Monthy_Estimates$Date<-paste(Monthy_Estimates$year_end,Monthy_Estimates$month, Monthy_Estimates$Day, sep="-")
Monthy_Estimates$Time<-as.Date(paste(Monthy_Estimates$year_end,Monthy_Estimates$month, Monthy_Estimates$Day, sep="-"), format="%Y-%m-%d")
Monthy_Estimates$Real[Monthy_Estimates$Time>="2016-1-15"]<-NA

Dates_Cont<-as.data.frame(seq(from=as.Date("2005-01-15"), to=as.Date("2022-12-15"), by="month"))

names(Dates_Cont)<-"Date"
Dates_Cont_SSLL<-merge(Dates_Cont, Monthy_Estimates, by.x="Date", by.y=c("Time"), all.x=TRUE)

png("~/Documents/Cookie_Cutter/Updated_Figures/CC_Bites_through_Time_SSLL_2025.png", height=4, width=8, units="in", res=300)
plot(Dates_Cont_SSLL$Date, Dates_Cont_SSLL$Real, type="l", lwd=3, ylim=c(0,1), ylab=c("Prob/Prop Sets with Bite"), xlab="Time", cex.lab=1.25)
par(new=T)
plot(Dates_Cont_SSLL$Date, Dates_Cont_SSLL$Modeled, type="l",col="red", lty=3, lwd=3, ylim=c(0,1), ylab=c("Prob/Prop Sets with Bite"), xlab="Time", cex.lab=1.25)
dev.off()




png("~/Documents/Cookie_Cutter/Updated_Figures/Monthly_Night_Hours_SSLL.png", height=5, width=10, units="in", res=300)
plot(Dates_Cont_SSLL$Date, Dates_Cont_SSLL$Mean_Night_Hrs, type="l", lwd=3, xlab="Time", ylab="Mean Night Hours per Set", cex.lab=1.25)
dev.off()

png("~/Documents/Cookie_Cutter/Updated_Figures/Monthly_Fish_SSLL.png", height=5, width=10, units="in", res=300)
plot(Dates_Cont_SSLL$Date, Dates_Cont_SSLL$Mean_Fish, type="l", lwd=3, xlab="Time", ylab="Mean Fish Count per Set", cex.lab=1.25)
dev.off()

png("~/Documents/Cookie_Cutter/Updated_Figures/Monthly_Hooks_SSLL.png", height=5, width=8, units="in", res=300)
plot(Dates_Cont_SSLL$Date, Dates_Cont_SSLL$Hooks, type="l", lwd=3, xlab="Time", ylab="Mean Hooks per Set", cex.lab=1.25)
dev.off()

png("~/Documents/Cookie_Cutter/Updated_Figures/Monthly_Day_Hours_SSLL.png", height=5, width=10, units="in", res=300)
plot(Dates_Cont_SSLL$Date, Dates_Cont_SSLL$Mean_Day_Hrs, type="l", lwd=3, xlab="Time", ylab="Mean Day Hours per Set", cex.lab=1.25)
dev.off()

cor.test(Monthy_Estimates$Real, Monthy_Estimates$Modeled, na.rm=T)

df_predictions<-df_predictions[df_predictions$year_end<=2015,]
df_predictions$Lat<-round(df_predictions$cent.lat)
df_predictions$Lon<-round(df_predictions$cent.lon)
df_predictions$Count<-1
df_predictions$Quarter[df_predictions$month>=1 & df_predictions$month<=3]<-1
df_predictions$Quarter[df_predictions$month>=4 & df_predictions$month<=6]<-2
df_predictions$Quarter[df_predictions$month>=7 & df_predictions$month<=9]<-3
df_predictions$Quarter[df_predictions$month>=10 & df_predictions$month<=12]<-4

Binned_Est<-ddply(df_predictions, .(Lon, Lat), summarise, Real=mean(CC_Bite_PA, na.rm=T), Modeled=mean(Mean_Cookie_Cutter_SSLL, na.rm=T),Total_Sets=sum(Count), Mean_Night_Hrs=mean(night_hrs, na.rm=T), Unq_Captains=length(unique(permit_num)))

#Binned_Estimates<-Year_Month_Est[Year_Month_Est$Quarter==2,]
#Binned_Estimates<-Binned_Estimates[Binned_Estimates$Lat>13,]
Binned_Estimates<-Binned_Est[Binned_Est$Unq_Captains>=3,]

Binned_Estimates<-Binned_Est[Binned_Est$Total_Sets>10,]



world <- ne_countries(scale="large",returnclass = "sf")#generate high res coastlines 

png("~/Documents/Cookie_Cutter/Updated_Figures/Modeled_SSLL_CC_Bite_Model_estimates_2025.png", res=300, height=6, width=8, units="in")
ggplot()+coord_fixed(ratio = 1)+geom_raster(data= Binned_Estimates,aes(x=Lon, y=Lat, fill=Modeled))+scale_fill_viridis_c( guide = guide_colourbar(title="Pr(Bite)"), limit=c(0,1))+
  geom_sf(data=world)+theme_bw()+geom_sf()+coord_sf(xlim=c(-178,-125), ylim=c(0, 40))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("")
dev.off()

png("~/Documents/Cookie_Cutter/Updated_Figures/Modeled_SSLL_CC_Bite_Actual_Prop_2025.png", res=300, height=6, width=8, units="in")
ggplot()+coord_fixed(ratio = 1)+geom_raster(data= Binned_Estimates,aes(x=Lon, y=Lat, fill=Real))+scale_fill_viridis_c( guide = guide_colourbar(title="Prop. Bite"), limit=c(0,1))+
  geom_sf(data=world)+theme_bw()+geom_sf()+coord_sf(xlim=c(-178,-125), ylim=c(0, 40))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("")
dev.off()


cor.test(Binned_Estimates$Modeled, Binned_Estimates$Real)

         