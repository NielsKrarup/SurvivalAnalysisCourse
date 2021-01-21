library(survival)
data(rhDNase)
head(rhDNase)
?rhDNase

first <- subset(rhDNase, !duplicated(id)) #first row for each subject
attach(first)
fu.tmp=as.numeric(end.dt-entry.dt)
n=length(fu.tmp)
fu.tmp[!is.na(ivstart)]=ivstart[!is.na(ivstart)]
set.seed(232)
fup=fu.tmp+runif(n)/1000 # adding random noise to break ties
status=0*fup
status[!is.na(ivstart)]=1
cbind(fup,status,first)[1:20,]
data.cf=data.frame(fup,status,trt,fev)[fup>0,] #DATA TO BE USED FOR REST OF EXERCISE
head(data.cf)

# fu: follow-up time to first exacerbations or end of follow-up whatever comes first
# status: 1 if an exacerbation; 0 otherwise

# trt: treatment arm: 0=placebo, 1= rhDNase
# fev: forced expriatory volume at enrollment, a measure of lung capacity


#GOF
library(timereg)
model = coxph(Surv(fup,status)~trt+fev,data=data.cf)
summary(model)
time.test= cox.zph(model,transform="log") #Could the theta value be 0?
print(time.test)

library(MASS)
library(mice)
plot(subset(data.cf$fup,data.cf$trt==0),nelsonaalen(subset(data.cf,data.cf$trt==0),fup,status),cex=0.5)
lines(subset(data.cf$fup,data.cf$trt==1),nelsonaalen(subset(data.cf,data.cf$trt==1),fup,status),type="p",cex=0.5,col="darkblue")

fit.cox = cox.aalen(Surv(fup,status)~prop(trt)+prop(fev),weighted.test = 0,data.cf)
#plot(fit.cox)
plot(fit.cox,score=T)
summary(fit.cox)

fit.cox.w = cox.aalen(Surv(fup,status)~prop(trt)+prop(fev),weighted.test = 1,data.cf)
plot(fit.cox.w,score=T)
summary(fit.cox.w)

resfit = cox.aalen(Surv(fup,status)~prop(trt)+prop(fev),weighted.test = 0,data.cf,residuals=1,n.sim=0)
resids = cum.residuals(resfit,data.cf,cum.resid=1)
summary(resids)
plot(resids,score=2)

resfit2 = cox.aalen(Surv(fup,status)~prop(trt)+prop(log(fev)),weighted.test = 0,data.cf,residuals=1,n.sim=0)
resids2 = cum.residuals(resfit2,data.cf,cum.resid=1)
summary(resids2)
plot(resids2,score=2)

#Only treatment covariate
model2 = coxph(Surv(fup,status)~trt,data=data.cf) 
summary(model2)
time.test2 = cox.zph(model2,transform="log")
print(time.test2)

fit.cox.simp = cox.aalen(Surv(fup,status)~prop(trt),weighted.test = 0,data.cf)
summary(fit.cox.simp)
#plot(fit.cox.simp)
plot(fit.cox.simp,score=T)
summary(fit.cox.simp)

fit.cox.w.simp = cox.aalen(Surv(fup,status)~prop(trt),weighted.test = 1,data.cf)
plot(fit.cox.w.simp,score=T)
summary(fit.cox.w.simp)



