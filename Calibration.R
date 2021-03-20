#设定自己的工作目录
setwd("F:\\科研交流 大数据\\seer\\5_Calibration")
#加载安装包
library(rms)
library(foreign)
library(survival)

#读取数据
seer<-read.table("seer.txt",header=T,sep="\t")
 
seer$age<-factor(seer$age,labels=c("<50","50-59","60-69","70-79",">=80"))
seer$sex<-factor(seer$sex,labels=c("Female","Male"))
seer$race<-factor(seer$race,labels=c("Black","White","Other"))
seer$stage_T<-factor(seer$stage_T,labels=c("T0","T1","T2","T3a","T3b","T3NOS","T4","TX"))
seer$stage_N<-factor(seer$stage_N,labels=c("N0","N1","NX"))
seer$stage_M<-factor(seer$stage_M,labels=c("M0","M1"))
#构建多因素Cox回归模型
#3-year
cox1 <- cph(Surv(survival_time,status) ~ age + sex + race +  stage_T + stage_N + stage_M,surv=T,x=T, y=T,time.inc = 1*12*3,data=seer) 

cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*12*3, m= 12000, B=100)

#画校准图
pdf("calibrate3.pdf",12,8)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 3-Year Survival",ylab="Actual 3-Year Survival",col="blue")
lines(cal,c('mean.predicted',‘KM'),type = ‘a',lwd = 3,col ="black" ,pch = 16)
box(lwd = 1)
abline(0,1,lty = 3,lwd = 3,col = "black")
dev.off()
#5-year
cox1 <- cph(Surv(survival_time,status) ~ age + sex + race +  stage_T + stage_N + stage_M,surv=T,x=T, y=T,time.inc = 1*12*5,data=seer) 

cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*12*5, m= 12000, B=100)

#画校准图
pdf("calibrate5.pdf",12,8)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 5-Year Survival",ylab="Actual 5-Year Survival",col="blue")
lines(cal,c('mean.predicted',‘KM'),type = ‘a',lwd = 3,col ="black" ,pch = 16)
mtext(“ ”)
box(lwd = 1)
abline(0,1,lty = 3,lwd = 3,col = "black")
dev.off()