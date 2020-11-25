
library(mets)
data(TRACE)

## data structure
head(TRACE)
help(TRACE)

## nelson-aalen
out <- aalen(Surv(time,status!=0)~-1+factor(vf),data=TRACE,robust=0)
par(mfrow=c(1,2))
plot(out)

## nelson-aalen
par(mfrow=c(1,1))
out <- phreg(Surv(time,status!=0)~strata(vf),data=TRACE)
bplot(out,se=TRUE)

## kaplan-meier
ss <- survfit(Surv(time,status!=0)~vf,TRACE)
plot(ss)

### logrank test 
survdiff(Surv(time,status!=0)~vf,TRACE)

################Cox Regression modelling ################

out <- phreg(Surv(time,status!=0)~vf+chf+wmi,data=TRACE)
bplot(out,se=TRUE)

dd <- data.frame(vf=c(1,0),chf=0,wmi=1.4)
po <- predict(out,dd,se=0)
plot(po)

### gof 
go <- gof(out)
summary(go)
par(mfrow=c(1,3))
plot(go)


### proportionality 
### interactions 
### linearity of wmi 

out <- phreg(Surv(time,status!=0)~strata(vf)+chf+wmi,data=TRACE)
gp <- gof(out)
summary(gp)
par(mfrow=c(1,2))
plot(gp)

par(mfrow=c(1,1))
plot(po,se=0)
pos <- predict(out,dd,se=0)
plot(pos,se=0,add=TRUE)

## graphical test, but prefer cumulative MG residuals
bplot(out,log="y")
gofG.phreg(out)

## linearity 
gofZ <- gofZ.phreg(Surv(time,status!=0)~strata(vf)+chf+wmi,data=TRACE,vars="wmi")
summary(gofZ)
plot(gofZ,type="z")


################################################################
################################################################

