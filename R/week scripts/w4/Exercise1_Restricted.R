##
shape = 2
scale <- 3

#restriction 
tau <- 4

S <- function(u) 1-pgamma(q = u, shape = shape, scale = scale)
f <- function(u) dgamma(x = u, shape = shape, scale = scale)

#sim data 
n <- 1e6
xt <- rgamma(n = n, shape = shape, scale = scale)
x <- pmin(xt, tau)

plot(function(x) dgamma(x = x, shape = shape, scale = scale), xlim = c(0, 10), ylim = c(0, tau))
hist(x, prob = T, add = T)
abline(v = tau)

s <- 3
abline(v = s, col = 2)
sum(x > s)

mer <- Vectorize( function(s) ifelse(sum(x > s) == 0, tau - s, mean(x[x > s] - s))  ) 
mer(0);mean(x)


plot(mer, xlim = c(0, 5), add = T)
abline(h = mer(3), col = 2, lty = 2)
text(2,mer(3), paste(round(mer(3), 2)))
# calculated 

foo <- function(s) 1/S(s)*integrate(f = function(u) S(u), lower = s, upper = tau)$value
mer(3)
foo(3)
