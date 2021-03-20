#没有安装过这些包的要安装
#install.packages("rms") 
#install.packages("foreign") 
#install.packages("survival") 

#设定自己的工作目录
setwd("F:\\科研交流 大数据\\seer\\1_multcox")
#加载安装包
library(rms)
library(foreign)
library(survival)

#读取数据
seer<-read.table("seer.txt",header=T,sep="\t")

#将数据转换成因子格式 
seer$age<-factor(seer$age,labels=c("<50","50-59","60-69","70-79",">=80"))
seer$sex<-factor(seer$sex,labels=c("Female","Male"))
seer$race<-factor(seer$race,labels=c("Black","White","Other"))
seer$stage_T<-factor(seer$stage_T,labels=c("T0","T1","T2","T3a","T3b","T3NOS","T4","TX"))
seer$stage_N<-factor(seer$stage_N,labels=c("N0","N1","NX"))
seer$stage_M<-factor(seer$stage_M,labels=c("M0","M1"))

#将数据打包好
ddist <- datadist(seer)
options(datadist='ddist')

#构建多因素的Cox回归模型
fmla1 <- as.formula(Surv(survival_time,status) ~ age + sex + race + stage_T + stage_N + stage_M)
cox2 <- cph(fmla1,data=seer)
#计算C-index
coxpe <- predict(cox2)#模型预测
c_index=1-rcorr.cens(coxpe,Surv(seer$survival_time,seer$status))
c_index

