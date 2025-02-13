#looking at catch ratios for the figures in CC MS
library(matrixStats)
library(fmsb)
setwd("C:/JJS_Old_Lptp/Longline_Projects/Cookie_Cutter")
CC_bite_DF1<-readRDS("Cookie_Cutter_Full_DF.rds")
CC_bite_DF1$CC_Bite_PA[CC_bite_DF1$CC_bites>0]<-1
CC_bite_DF1$CC_Bite_PA[CC_bite_DF1$CC_bites==0]<-0
CC_bite_DF1$Full_Moon<-0
CC_bite_DF1$Full_Moon[CC_bite_DF1$lunar_rad>=pi*0.5 & CC_bite_DF1$lunar_rad<=pi*1.5]<-1
CC_bite_DF1$Set_Count<-1


CC_bite_DSLL<-CC_bite_DF1[CC_bite_DF1$FISHERY=="DSLL",]
CC_bite_SSLL<-CC_bite_DF1[CC_bite_DF1$FISHERY=="SSLL",]
Effort_by_Moon_SSLL<-ddply(CC_bite_SSLL, .(year_end), plyr::summarize, Number_of_Sets=sum(Set_Count), Full_Moon_Ratio=sum(Full_Moon)/sum(Set_Count))
Effort_by_Moon_DSLL<-ddply(CC_bite_DSLL, .(year_end), plyr::summarize, Number_of_Sets=sum(Set_Count), Full_Moon_Ratio=sum(Full_Moon)/sum(Set_Count))

png("Propotion_of_Sets_On_Full_Moon_SSLL.png", height=5, width=6, units="in", res=300)
plot(Effort_by_Moon_SSLL$year_end, Effort_by_Moon_SSLL$Full_Moon_Ratio, xlab="Year",ylab="Proportion of Sets near Full Moon", cex.lab=1.4, pch=19, cex=1.5, ylim=c(0.4,0.75))
par(new=T)
plot(Effort_by_Moon_SSLL$year_end, Effort_by_Moon_SSLL$Full_Moon_Ratio, xlab="Year",ylab="Proportion of Sets near Full Moon", cex.lab=1.4, type="l",lwd=2, cex=1.5, ylim=c(0.4,0.75))
dev.off()



png("Propotion_of_Sets_On_Full_Moon_DSLL.png", height=5, width=6, units="in", res=300)
plot(Effort_by_Moon_DSLL$year_end, Effort_by_Moon_DSLL$Full_Moon_Ratio, xlab="Year",ylab="Proportion of Sets near Full Moon", cex.lab=1.4, pch=19, cex=1.5, ylim=c(0.4,0.75))
par(new=T)
plot(Effort_by_Moon_DSLL$year_end, Effort_by_Moon_DSLL$Full_Moon_Ratio, xlab="Year",ylab="Proportion of Sets near Full Moon", cex.lab=1.4, type="l",lwd=2, cex=1.5, ylim=c(0.4,0.75))
dev.off()


print(sum(CC_bite_SSLL$CC_Bite_PA)/nrow(CC_bite_SSLL))
print(sum(CC_bite_DSLL$CC_Bite_PA)/nrow(CC_bite_DSLL))


mround <- function(x,base){
  base*round(x/base)
}
CC_bite_DSLL$Set_Count<-1
CC_bite_DSLL$Lat_Deg<-round(CC_bite_DSLL$cent.lat)
CC_bite_DSLL$Lon_Deg<-round(CC_bite_DSLL$cent.lon)
CC_bite_DSLL$Sword_PA[CC_bite_DSLL$`Xiphias gladius`>0]<-1
CC_bite_DSLL$Sword_PA[CC_bite_DSLL$`Xiphias gladius`==0]<-0

Effort_by_Bin<-ddply(CC_bite_DSLL, .(Lat_Deg, Lon_Deg), plyr::summarize, Number_of_Sets=sum(Set_Count), Bigeye_Total_Ratio=sum(`Thunnus obesus`)/(sum(`Thunnus obesus`)+sum(`Thunnus albacares`)), Swordfish_Proportion=sum(Sword_PA)/sum(Set_Count))

Effort_by_Bin_Showable<-Effort_by_Bin[Effort_by_Bin$Number_of_Sets>25,]
colnames(Effort_by_Bin_Showable)<-c("Lat","Lon","Sets", "Bigeye Ratio", "Prop. with Swordfish")
world <- ne_countries(scale=50,returnclass = "sf")#generate high res coastlines
# setwd("/home/jsuca/Longline_Projects/SSLL_Code/May_2024_Code_Output/Monthly_Model_Output/Maps")
png("Fishing_Effort_DSLL_Bigeye_Ratio_2005-2022_for_CC.png", res=300, height=6, width=8, units="in")
p<-ggplot()+coord_fixed(ratio = 1)+geom_raster(data= Effort_by_Bin_Showable,aes(x=Lon, y=Lat, fill=`Bigeye Ratio`))+scale_fill_viridis(option="cviridis")+
  geom_sf(data=world)+theme_bw()+geom_sf()+coord_sf(xlim=c(-178,-120), ylim=c(0, 45))+ggtitle("Deep-Set")
print(p)
dev.off()


