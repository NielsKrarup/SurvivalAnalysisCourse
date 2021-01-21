library(ggplot2)
library(survival)
foo <- function(n = 50){
  
rate <- 2
n_sim <- 1e4

ts <- rexp(n = n, rate = rate)
c <- runif(n = n, min = 0, max = max(ts))
d <- (ts <= c)
t <- pmin(ts, c)

df <- data.frame(t = t, d = d, ts = ts, c = c)

#ggplot(df, aes(x = t, col = d)) + geom_jitter(aes(y = d), height = 0, width = 0)

#plot(x = t, y = d, col = d+1, pch = d+1, data = df)
survfit_obj <- survfit(Surv(time = t, event = d)~ 1, data = df)
#str(survfit_obj)
# plot(survfit_obj)     
# plot(function(x) 1- pexp(q = x, rate = rate), xlim = range(survfit_obj$time), add = T , col = 2)

#Martingale

#M = N - Lambda

N <- Vectorize(function(t = 1, obj = survfit_obj) as.numeric(sum( obj$n.event[(obj$time <= t)] )))
N(100)
#plot(N, type = "s", main = "N")
L <- Vectorize(function(t, obj = survfit_obj) {
  as.numeric(integrate(f = function(u) sum(obj$time >= u)*(dexp(x = u, rate = rate)/(1-pexp(q = u, rate = rate))), lower = 0, upper = t)$value)
})
#plot(L, main = "L")
#abline(a = 0, b = n*rate)

M <- Vectorize(function(t, obj = survfit_obj) {
  as.numeric(sum(obj$n.event[(obj$time <= t)])) -
    as.numeric(integrate(
      f = function(u)
        sum(obj$time >= u) * (dexp(x = u, rate = rate) / (1 - pexp(
          q = u, rate = rate
        ))),
      lower = 0,
      upper = t
    )$value)
})
M <- Vectorize(M)
plot(M, add = T, col = alpha("grey", 0.4))
return(M(0.2))
}
plot(M, ylim = c(-20, 20))
foo(n = 30)
a <- c()
for(i in 1:1000)a[i] <- foo()
var <- integrate(f = function(u) rate/((max(ts)-u)*1/max(ts)*(1-pexp(q = u, rate = rate))), lower = 0, upper = 0.2)$value
sd <- sqrt(var)
hist(a, prob = T)
plot(function(x)dnorm(x = x, sd = 5), add = T, xlim= c(-10, 10))
