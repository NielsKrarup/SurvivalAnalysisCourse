# Competing risk 
library("cmprsk")
library(timereg)
data(melanoma)

# Melanoma example, MS ----------------------------------------------------

#1 is other causes, 2 is from melonma
melanoma$status2 <- ifelse(melanoma$status == 2, 0, ifelse(melanoma$status == 1, 2, 1))
melanoma

fit <- cmprsk::cuminc(ftime = melanoma$days/365.24, fstatus = melanoma$status2)
str(fit)

plot(fit$`1 2`$time, fit$`1 2`$est,type = "s", ylim = c(0,0.5))
se <- fit$`1 2`$var^0.5
up <- fit$`1 2`$est + 1.96*se
low <- fit$`1 2`$est - 1.96*se

lines(fit$`1 2`$time, low, type = "s", lty = 2)
lines(fit$`1 2`$time, up, type = "s", lty = 2)

abline(v = 8)
abline(h = fit$`1 2`$est[max(which(fit$`1 2`$time<=8))], lty = 3)
#Add Kaplan Meier, with all other causes than Melanoma as censored 
melanoma$statusKM <- ifelse(melanoma$status == 1, 1, 0)
km <- survfit(Surv(melanoma$days/365.24, melanoma$statusKM)~1)
lines(km$time, 1-km$surv, type = "s", col = 2)
tmp <- summary(km, times = 8)
1-tmp$surv
abline(h = 1-tmp$surv, col = 2, lty = 3)

melanoma$days[melanoma$statusKM == 1]


# Slides example treatment effect from no-dep. hazard ---------------------

F1 <- Vectorize(function(t, x){
  integrate(f = function(s) exp(-s-2^(1-x)*s^(3/2)/3)*1, lower = 0, upper = t)$value
})

F2 <- Vectorize(function(t, x){
  integrate(f = function(s) exp(-s-2^(1-x)*s^(3/2)/3) * s^(1/2)/(2^x), lower = 0, upper = t)$value
})

S <- function(s,x) exp(-s -2^(1-x)*s^(3/2)/3)

F1(2,x =0)

F2(10, 1)
plot(function(t)F1(t, x = 0), xlim = c(0,3), ylim = c(0,1.1))
plot(function(t)F1(t, x = 1), xlim = c(0,3), add = T, col = 2)

plot(function(t)F2(t, x = 0), xlim = c(0,3), ylim = c(0,1.1), lty = 2, add = T)
plot(function(t)F2(t, x = 1), xlim = c(0,3), add = T, col = 2, lty = 2)

#Survival
plot(function(t)S(s = t, x = 0), xlim = c(0,3), add = T, lty = 3)
plot(function(t)S(s = t, x = 1), xlim = c(0,3), add = T, lty = 3, col = 2)

#add to one?
plot(function(t)S(s = t, x = 0) + F1(t, x = 0) + F2(t, x = 0), xlim = c(0,3), add = T, lty = 4, lwd = 3)
plot(function(t)S(s = t, x = 1) + F1(t, x = 1) + F2(t, x = 1), xlim = c(0,3), add = T, lty = 4, col = 2)

