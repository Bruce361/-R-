#设定自己的工作目录
setwd("F:\\科研交流 大数据\\seer\\7_KM_survival")

library(survival)

inputdata<- read.table("risk_score.txt",header=T,sep="\t")



#by risk
kms<-survfit(Surv(survival_time,status)~risk_level,data=inputdata)
kmdffexp=survdiff(Surv(survival_time,status)~risk_level,data=inputdata)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)

pdf("survival_risk.pdf")
plot(kms,lty="solid",col=c("red","green"),
xlab="Survival time in months",ylab="Survival probabilities",
main=paste("Surival curve of risk score(P=", pValue ,")",sep=""))
legend("topright",c("High risk","Low risk"),lty="solid",col=c("red","green"))
dev.off()

inputdata1<- read.table("seer.txt",header=T,sep="\t")

#by age
kms<-survfit(Surv(survival_time,status)~age,data=inputdata1)
kmdffexp=survdiff(Surv(survival_time,status)~age,data=inputdata1)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)

pdf("survival-age.pdf")
plot(kms,lty="solid",col=c("red","green","yellow","blue","black"),
xlab="Survival time in months",ylab="Survival probabilities",
main=paste("Surival curve of age (P=", pValue ,")",sep=""))
legend("topright",c("<50","50-59","60-69","70-79",">=80"),lty="solid",col=c("red","green","yellow","blue","black"))
dev.off()

#by gender
kms<-survfit(Surv(survival_time,status)~sex,data=inputdata1)
kmdffexp=survdiff(Surv(survival_time,status)~sex,data=inputdata1)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)

pdf("survival-sex.pdf")
plot(kms,lty="solid",col=c("red","green"),
xlab="Survival time in months",ylab="Survival probabilities",
main=paste("Surival curve of gender (P=", pValue ,")",sep=""))
legend("topright",c("Female","Male"),lty="solid",col=c("red","green"))
dev.off()

#by race
kms<-survfit(Surv(survival_time,status)~race,data=inputdata1)
kmdffexp=survdiff(Surv(survival_time,status)~race,data=inputdata1)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)
pdf("survival-race.pdf")
plot(kms,lty="solid",col=c("red","green","blue"),
xlab="Survival time in months",ylab="Survival probabilities",
main=paste("Surival curve of race (P=", pValue ,")",sep=""))
legend("topright",c("Black","White","Other"),lty="solid",col=c("red","green","blue"))
dev.off()

