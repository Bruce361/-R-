#install.packages("caret")

library(foreign)
library(survival)
library(caret)

setwd("F:\\科研交流 大数据\\seer\\6_verification")
seer<-read.table("seer.txt",header=T,sep="\t")
set.seed(300)
seerd<-createDataPartition(y=seer$id,p=0.70,list=F)
seerdev<-seer[seerd, ]
seerv<-seer[-seerd,] 
write.csv(seerdev, "seerdev.csv")
write.csv(seerv, "seerv.csv")














#验证
tcga<-read.table("ver.txt",header=T,sep="\t")

#将数据转换成因子格式 
tcga$age<-factor(tcga$age,labels=c("<50","50-59","60-69",">=70"))
tcga$sex<-factor(tcga$sex,labels=c("FEMALE","MALE"))
tcga$race<-factor(tcga$race,labels=c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN"))
tcga$smoking<-factor(tcga$smoking,labels=c("1","2","3","4","5"))
tcga$radiation<-factor(tcga$radiation,labels=c("YES","NO"))
tcga$pharmaceutical<-factor(tcga$pharmaceutical,labels=c("YES","NO"))
tcga$stage_T<-factor(tcga$stage_T,labels=c("T1","T1a","T1b","T1c","T2","T2a","T2b","T3","T4","TX"))
tcga$stage_M<-factor(tcga$stage_M,labels=c("M0","M1","M1b","MX"))
tcga$stage_N<-factor(tcga$stage_N,labels=c("N0","N1","N2","NX"))
tcga$surgery<-factor(tcga$surgery,labels=c("0","1"))

#将数据打包好
ddist <- datadist(tcga)
options(datadist='ddist')

#构建多因素的Cox回归模型
fmla1 <- as.formula(Surv(survival_time,status) ~ age + sex + race + smoking + radiation + pharmaceutical + stage_T + stage_N + stage_M)
cox2 <- coxph(fmla1,data=tcga)
summary(cox2)

#c-index 0.714

#画校准图
cox1 <- cph(Surv(survival_time,status) ~ age + sex + race + smoking + radiation + pharmaceutical + stage_T + stage_N + stage_M,surv=T,x=T, y=T,time.inc = 1*365*5,data=tcga) 
cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*365*5, m=50, B=1000)
pdf("calibrate_ver.pdf",12,8)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram Predicted Survival",ylab="Actual Survival",col="blue")
lines(cal,c('mean.predicted',‘KM'),type = ‘a',lwd = 3,col ="black" ,pch = 16)
mtext(“ ”)
box(lwd = 1)
abline(0,1,lty = 3,lwd = 3,col = "black")
dev.off()