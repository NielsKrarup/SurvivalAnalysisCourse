library(ggplot2)

n <- 1e7

pix <- 0.6
piz <- 0.4

beta <- -pix/(1-pix)
gamma <- -piz/(1-piz)
rho <- 0.3

basehaz <- 1

x <- rbinom(n = n, size = 1, prob = pix)
#x <- factor(x, levels = c("1","0"))

z <- rbinom(n = n, size = 1, prob = piz)
#z <- factor(z, levels = sort(unique(z), decreasing = T))
base_tb <- table(factor(x, levels = 1:0), factor(z, levels = 1:0))/n

time <- rexp(n = n, rate = basehaz*exp(beta*x + gamma*z))
time2 <- rexp(n = n, rate = basehaz + beta*x + gamma*x*z)

df <- data.frame(time, time2, x, z)

ggplot(data = df, aes(x = time, col = factor(x):factor(z))) + geom_density(alpha = 0.3) + xlim(0,7)
#ggplot(data = subset(df, time2 < 2), aes(x = time2, col = factor(x):factor(z))) + geom_density(alpha = 0.3)

#dist at time t
foo_tb <- function(t = 1, counts = F){
  df_sub <- subset(df, time > t)
  out <-
  if(counts){
    table(factor(df_sub$x, levels = 1:0), factor(df_sub$z, levels = 1:0))
  }else{
    table(factor(df_sub$x, levels = 1:0), factor(df_sub$z, levels = 1:0))/nrow(df_sub)*100
  }
  #marginals
  pix_mar <-     table(factor(df_sub$x, levels = 1:0))/nrow(df_sub)
  piz_mar <-     table(factor(df_sub$z, levels = 1:0))/nrow(df_sub)
  
  #Indep Tabel
  
  out_indep <- outer(X = pix_mar, Y = piz_mar, FUN = "*")*100
  base = outer(X = c(pix, 1-pix), Y = c(piz, 1-piz), FUN = "*")*100
  marg_obs <- matrix(c(pix_mar, piz_mar),byrow = T, 2)*100
  rownames(marg_obs) <- c("X", "Z");colnames(marg_obs) <- c("%1", "%0")
  
  return(list(Baseline_true = base, Observed= out, MarginalsObserved = marg_obs, Indep = out_indep))

}

base_tb*100;outer(X = c(pix, 1-pix), Y = c(piz, 1-piz), FUN = "*")*100

foo_tb(0)
foo_tb(1)
foo_tb(2.5)
foo_tb(5)
foo_tb(10)

#independence broken!
chisq.test(base_tb*n)
chisq.test(foo_tb(t = 0.1, counts = T)$Observed)

#check dist √:
foo_dist <- function(i = 1, j = 1, t = 2.5, A_0 = function(s) s){
  ifelse(i,pix,1-pix)*ifelse(j, piz, 1-piz)*exp(-A_0(t)*exp(i*beta + j*gamma)) /
    (
        exp(-A_0(t)*exp(0*beta + 0*gamma))*(1-pix)*(1-piz) +
        exp(-A_0(t)*exp(1*beta + 0*gamma))*(pix)*(1-piz) +
        exp(-A_0(t)*exp(0*beta + 1*gamma))*(1-pix)*(piz) +
        exp(-A_0(t)*exp(1*beta + 1*gamma))*(pix)*(piz) 
        
    )
}
#another parameterization 
foo_dist <- function(i = 1, j = 1, t = 2.5, A_0 = function(s) s){
  ifelse(i,pix,1-pix)*ifelse(j, piz, 1-piz) /
    (
      exp(A_0(t)*(exp(i*beta + j*gamma) - exp(0*beta + 0*gamma)))*(1-pix)*(1-piz) +
        exp(A_0(t)*(exp(i*beta + j*gamma) - exp(1*beta + 0*gamma)))*(pix)*(1-piz) +
        exp(A_0(t)*(exp(i*beta + j*gamma) - exp(0*beta + 1*gamma)))*(1-pix)*(piz) +
        exp(A_0(t)*(exp(i*beta + j*gamma) - exp(1*beta + 1*gamma)))*(pix)*(piz) 
      
    )
}
foo_tb(1)
matrix(c(foo_dist(t = 1), foo_dist(j = 1, t = 1),
         foo_dist(i = 0, t = 1), foo_dist(i = 0, j = 0, t = 1)), byrow = T, 2)

foo_tb(2.5)
foo_dist(t = 2.5)


foo_tb(70)
foo_dist(t = 70)

#marginal dist.
foo_marg_x <- function(i = 1, t = 2.5, A_0 = function(s) s){
  (exp(-A_0(t)*exp(i*beta + 0))*(1-piz)*ifelse(i,pix,1-pix) + exp(-A_0(t)*exp(i*beta + gamma))*(piz)*ifelse(i,pix,1-pix))  /
    (
      exp(-A_0(t)*exp(0*beta + 0*gamma))*(1-pix)*(1-piz) +
        exp(-A_0(t)*exp(1*beta + 0*gamma))*(pix)*(1-piz) +
        exp(-A_0(t)*exp(0*beta + 1*gamma))*(1-pix)*(piz) +
        exp(-A_0(t)*exp(1*beta + 1*gamma))*(pix)*(piz) 
      
    )
}

foo_tb(t = 2)$MarginalsObserved
foo_marg_x(i = 1, t = 2) 
