
## simulations 
library(mets)

data(TRACE)
navf0 <- phreg(Surv(time,status!=0)~+1,subset(TRACE,vf==0))
navf1 <- phreg(Surv(time,status!=0)~+1,subset(TRACE,vf==1))
bplot(navf1,se=TRUE,col=2)
bplot(navf0,se=TRUE,add=TRUE)

strace0 <- rchaz(navf0$cumhaz,n=10000)
strace1 <- rchaz(navf1$cumhaz,n=10000)

snavf0 <- phreg(Surv(time,status!=0)~+1,strace0)
snavf1 <- phreg(Surv(time,status!=0)~+1,strace1)

bplot(snavf1,col=3,add=TRUE)
bplot(snavf0,col=4,add=TRUE)


## cox models 
tt <- phreg(Surv(time,status!=0)~vf,TRACE)
summary(tt)
bplot(tt)

stt <- sim.cox(tt,n=100000)
ttt <- phreg(Surv(time,status!=0)~vf,stt)
bplot(ttt,add=TRUE,col=2)


