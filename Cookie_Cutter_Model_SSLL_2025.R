#example running of BRTs using Lancetfish distribution from SSLL data
library(matrixStats)
library(fmsb)
setwd("~/Documents/Cookie_Cutter/Code_for_Model")
source("BRT_Eval_Function_JJS.R")
df<-readRDS("~/Documents/Cookie_Cutter/Cookie_Cutter_Full_DF_GLORYS_v2.rds")

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
CC_bite_DF<-CC_bite_DF[CC_bite_DF$year_end<=2015,]
CC_bite_DF<-CC_bite_DF[CC_bite_DF$HKS_FLT<50,]
BRT_SSLL_Est_1<-gbm.step(CC_bite_DF, gbm.x = Preds, gbm.y = ncol(CC_bite_DF),tree.complexity=5,learning.rate = 0.005 , bag.fraction = 0.75,family="bernoulli", n.folds=5)
Ntrees<-ifelse(length(BRT_SSLL_Est_1$trees)<1000, 1000,length(BRT_SSLL_Est_1$trees))
#Ntrees<-8550 #comes from previous run that failed after this point

#Used_preds<-c("Fish_Count","permit_num","bait_code", "sst","log_chla","julian_end", "year_end","lunar_rad","Eddy_Classification","sst_anom","SLA","Temp_300m","Random","PDO","MEI",  "Current_Speed", "model_sss","chla.anom","mld","NPGO", "FLTLN_LEN","BRNCHLN_LEN", "HKS_FLT", "LITE_HK","Dist_to_200m", "O2_300m","day_hrs","night_hrs")

SSLL_CC_BRT_Models1<-fit.brt.n_eval_Balanced_Fixed(CC_bite_DF, gbm.x = Preds, gbm.y=ncol(CC_bite_DF) ,lr=0.005, tc=5,family="bernoulli",nt=Ntrees, bag.fraction = 0.75,50)

var_tested<-names(CC_bite_DF[,Preds])

PA_Model<-SSLL_CC_BRT_Models1[[1]]
iters=length(PA_Model)
percent_contrib<-NULL#list()
for(q in 1:iters){                               
  sum1<-summary(PA_Model[q][[1]]  , plot=F )
  sum2<-sum1[order(sum1[,1], levels = var_tested),]
  percent_contrib<-cbind(percent_contrib, sum2[,2])
  rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}


Mean_PA_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))

Predictors_to_Keep_Index<-which(Mean_PA_Contributions>Mean_PA_Contributions$Random)

Predictors_to_Keep<-Mean_PA_Contributions[,Predictors_to_Keep_Index]
Reduced_Predictors<-which(colnames(CC_bite_DF) %in% colnames(Predictors_to_Keep))

SSLL_CC_BRT_Models<-fit.brt.n_eval_Balanced_Fixed(CC_bite_DF, gbm.x = Reduced_Predictors, gbm.y=ncol(CC_bite_DF) ,lr=0.005, tc=5,family="bernoulli",nt=Ntrees, bag.fraction = 0.75,50)

saveRDS(SSLL_CC_BRT_Models, "PA_SSLL_Cookie_Cutter_Bite_BRT_Models_2025.rds")



SSLL_CC_BRT_Models<-readRDS("PA_SSLL_Cookie_Cutter_Bite_BRT_Models_2025.rds")



Reef_Fish_BRT_Models<-SSLL_CC_BRT_Models
Model_Skill_PA<-matrix(,1,4)

k=1
Model_PA_Eval<-matrix(,50,2)

if (is.null(Reef_Fish_BRT_Models[[k]])==FALSE){
  Model_Evals_PA<-Reef_Fish_BRT_Models[[2]]
  
  for (i in 1:50){
    Model_PA_Eval[i,1]<-Model_Evals_PA[[i]]@auc
    Model_PA_Eval[i,2]<-max(Model_Evals_PA[[i]]@TPR+Model_Evals_PA[[i]]@TNR-1)
  }
  Model_Skill_PA[k,1]<-mean( Model_PA_Eval[,1])
  Model_Skill_PA[k,2]<-min( Model_PA_Eval[,1])
  Model_Skill_PA[k,3]<-mean( Model_PA_Eval[,2])
  Model_Skill_PA[k,4]<-min( Model_PA_Eval[,2])}

Species_Exact_Names<-c("Cookie Cutter")
rownames(Model_Skill_PA)<-Species_Exact_Names
colnames(Model_Skill_PA)<-c("Mean_AUC","Min_AUC","Mean_TSS","Min_TSS")
write.csv(Model_Skill_PA,"PA_Metrics_Cookie_Cutter_SSLL_Response_2025.csv")

# #####Full model plot#########
`%nin%` = Negate(`%in%`)
var_tested<-c("Fish_Count","bait_code", "SST","log_chla","julian_end", "year_end","lunar_rad","SST_SD","SSH","SSH_SD","SSS","SSS_SD", "Random","PDO","MEI", "Current_Speed","MLD","NPGO", "FLTLN_LEN","BRNCHLN_LEN", "HKS_FLT", "LITE_HK","day_hrs","night_hrs")

#Species_Exact_Names<-c("Moli","Kaupu")
for (j in 1:1){#length(Q)
  Model<-Reef_Fish_BRT_Models[[j]]
  if (is.null(Model)==FALSE){
    Fish_Models<-Reef_Fish_BRT_Models[[1]]
    Species_Name<-"Cookie_Cutter"
    var_tested<-Fish_Models[[1]]$var.names
    #var_tested<-colnames(PA_Data_Reef_Fishes_MHI[Predictors])
    Fish_Models_Good<-Fish_Models#[Q]
    percent_contrib<-NULL#list()
    iters=length(Fish_Models_Good)
    part_plot<-list()
    part_plot<-list()
    percent_contrib<-NULL#list()
    Continuous_Preds<-which(var_tested %nin% c("permit_num","bait_code", "Eddy_Classification","FISHERY"))
    for(q in 1:iters){                                #this was 50
      mod<-Fish_Models_Good[q][[1]]
      ###
      part_plot1<-data.frame(row.names=1:100)
      for(x in c(Continuous_Preds)){ ###
        
        pp<-plot(mod ,var_tested[x],return.grid=T) ###
        part_plot1<-cbind(part_plot1, pp) ###
      }
      
      #   ###
      part_plot[[q]]<-part_plot1 ###
      
      sum1<-summary(Fish_Models_Good[q][[1]]  , plot=F )
      sum2<-sum1[order(sum1[,1], levels = var_tested),]
      percent_contrib<-cbind(percent_contrib, sum2[,2])
      rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
    }
    All_percent_contribution<-cbind(rownames(percent_contrib), paste(round(rowMeans(percent_contrib),2), round(rowSds(percent_contrib),2), sep=" ± "))
    SSLL_All_percent_contribution<-All_percent_contribution
    #
    #
    Mean_PA_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))
    write.csv( Mean_PA_Contributions,paste0("Var_Contributions_",Species_Name,"_PA_SSLL_2025.csv"))
    
    PA_Predictors_Plot<- rbind(rep(max(Mean_PA_Contributions),length(var_tested)) , rep(0,length(var_tested)) , Mean_PA_Contributions)
    PA_Predictors_Plot[]<-sapply(PA_Predictors_Plot, as.numeric)
    par(mfrow=c(1,1))
    
    png(paste0("Radar_Chart_",Species_Name,"_PA_SSLL_2025.png"), height=6, width=6, units="in",res=300)
    radarchart(PA_Predictors_Plot,  pfcol=rgb(0.0,0.3,0.5,0.5), pcol=rgb(0.0,0.3,0.5,0.5), title=paste0(Species_Name,"_PA"))
    dev.off()
    #
    All_percent_contribution<-cbind(rownames(percent_contrib), paste(round(rowMeans(percent_contrib),2), round(rowSds(percent_contrib),2), sep=" ± "))
    #
    png(paste0("Partial_plots_",Species_Name,"_PA_SSLL_2025.png"), height=18,width=14, res=300, units="in")
    par(mfrow=c(5,5))
    mn_part_plot<-list()
    for(y in c(Continuous_Preds)){
      id<-which(colnames(part_plot[[1]])==var_tested[y])
      all1<-NULL
      all2<-NULL
      for(z in 1:iters){											 #this was 50
        all1<-rbind(all1, cbind(c(part_plot[[z]][,id])))
        all2<-rbind(all2, cbind(c(part_plot[[z]][,id+1])))
      }
      all3<-cbind(all1, all2)
      all1<-all3[order(all3[,1]),]
      #
      plot(all1, xlab=var_tested[y], col="white", ylab=paste("f(",var_tested[y], ")", sep=""),cex.axis=1.2, cex.lab=1.2) #, ylim=c(-8,2))
      plx<-predict(loess(all1[,2] ~ all1[,1], span = 0.3), se=T)
      mn_part_plot[[y]]<- cbind(all1[,1], plx$fit)
      lines(all1[,1],plx$fit)
      lines(all1[,1],plx$fit - qt(0.975,plx$df)*plx$se, lty=2)#0.975
      lines(all1[,1],plx$fit + qt(0.975,plx$df)*plx$se, lty=2)
      rug(na.omit(unlist(CC_bite_DF[var_tested[y]])))
      legend("bottomright", paste(All_percent_contribution[which(All_percent_contribution[,1]==var_tested[y]),2],"%", sep=" "), bty="n", cex=1.4)
    }
    dev.off()
    rm(Fish_Models)
    rm(Model)
    gc()
  }
  else if (is.null(Model)==TRUE){
    rm(Model)
    
  }}

