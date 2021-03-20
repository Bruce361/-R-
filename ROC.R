#软件的安装
install.packages("survivalROC")

library(survivalROC)
setwd("F:\\科研交流 大数据\\seer\\4_ROC")
seer<-read.table("risk_score.txt",header=T,sep="\t")
#3 year
predict_time<-12*3 #如果是天数365*3 
myroc<-survivalROC(Stime=seer$survival_time, status=seer$status, marker=seer$risk_score, predict.time=predict_time,method="KM")
pdf("ROC_3year.pdf")
plot(myroc$FP,myroc$TP,type="l",xlim=c(0,1),ylim=c(0,1),col="blue", 
xlab="FP",ylab="TP",main=paste("3-year Survival","AUC=",round(myroc$AUC,3)))
abline(0,1)
dev.off()
#5_year
predict_time<-12*5 #如果是天数365*5
myroc<-survivalROC(Stime=seer$survival_time, status=seer$status, marker=seer$risk_score, predict.time=predict_time,method="KM")
pdf("ROC_5year.pdf")
plot(myroc$FP,myroc$TP,type="l",xlim=c(0,1),ylim=c(0,1),col="blue", 
xlab="FP",ylab="TP",main=paste("3-year Survival","AUC=",round(myroc$AUC,3)))
abline(0,1)
dev.off()