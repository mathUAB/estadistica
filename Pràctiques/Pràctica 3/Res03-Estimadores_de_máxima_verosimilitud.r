##########################
###### 3rd PRACTICE ######
##########################

### Section 1 ###

# Example 1
library(MASS)
## exponential distribution
x = rexp(100, rate = 2)
fitdistr(x, "exponential")
1 / mean(x) # MLE
## normal distribution
n = 100
y = rnorm(n, 3, 6)
fitdistr(y, "normal")
mean(y) # MLE for the mean
s = var(y) * (n - 1) / n
sqrt(s) # MLE for the standard deviation

# Example 2
x = rexp(100, 2)
mean(x)
1 / mean(x)
mlogL = function(x, lam = 1) {
  return (-(length(x) * log(lam) - lam * sum(x))) # the minus sign is because we want to maximize, not minimize
}
mlogl = function(x, lam = 1) {
  return (-sum(dexp(x, rate = lam, log = TRUE)))
}
npoint = 101
lams = seq(min(x), max(x) + 2, length = npoint)
logls = numeric(npoint)
for (i in 1:npoint)
  logls[i] = mlogl(x, lams[i])
plot(
  lams,
  -logls,
  type = "l",
  xlab = expression(lams),
  ylab = expression(l(lams))
)
nlm(mlogl, 1, x = x)
#optim(par=1,mlogl,x=x) # this produces a warning
optimize(mlogl, interval = c(0, 10), x = x) # for one-variable optimization
1 / mean(x)
#################

### Problems

# Problem 1
n=100
x = rnorm(n, mean = 3, sd = 2)
mlogl = function(x, theta = c(0, 1)) {
  mu = theta[1]
  sigma = theta[2]
  return (-sum(dnorm(
    x, mean = mu, sd = sigma, log = TRUE
  )))
}
fitdistr(x,"normal")
nlm(mlogl, c(mean(x),1), x = x)
optim(c(mean(x), 1), mlogl, x = x)
mean(x);sqrt(var(x)* (n - 1) / n)

# Problem 2
# data("michelson")
x = michelson$Speed
hist(x)
fitdistr(x, "normal") # standard error too high ---> suspicious
mean(x);sqrt((length(x)-1)/length(x)*var(x))
optim(c(mean(x), 1), mlogl, x = x)
hist(x)
s = sort(which(x < 700 | x > 1000), decreasing = TRUE)
s
z = x
for (i in s)  z = z[-i]
z
hist(z)
fitdistr(z, "normal") # standard error still too high ---> suspicious
optim(c(mean(z), 1), mlogl, x = z)

# Problem 3
a = c(18, 18, 12, 7, 5)
b = c(1, 2, 3, 4, 5)
x = sum(a * b)
n = sum(a)
A = prod(factorial(b) ^ a)
lileli = function(x, theta = 1) {
  return (theta ^ x * exp(-n * theta) / (A * (1 - exp(-theta)) ^ n))
}
mlogl = function(x, theta=1) {
  return (-log(lileli(x, theta)))
}
optimise(interval = c(0, 10), mlogl, x = x)
nlm(mlogl, 2, x = x)

# Problem 4
library(evir)
data("danish")
summary(danish)
# We estimate xm with min(danish) (in fact it can be seen that it is the MLE of xm)
xm_aprox=min(danish)
log_likeli = function(x, alph=1) {
    return (-sum(log(alph * xm_aprox ^ alph / x ^ (alph + 1))))
}
optimise(interval=c(0,10), log_likeli, x = danish)
nlm(log_likeli,2,x=danish)
# Obs: It can be seen that the MLE of alpha is: n/(sum(log(x_i))-nlog(xm_aprox))
# In our case:
(length(danish))/(sum(log(danish))-length(danish)*log(xm_aprox))


# Problem 5
n=100
x = rgamma(n, shape = 10, rate = 3)
mu1 = mean(x) # empirical 1st moment
mu2 = 1 / n * sum(x ^ 2) # empirical 2nd moment
# solving analytically we get:
alpha = -mu1 ^ 2 / (mu1 ^ 2 - mu2)
alpha
beta = -mu1 / (mu1 ^ 2 - mu2)
beta
mlogl = function(x, theta = c(1, 1)) {
  alph = theta[1]
  bet = theta[2]
  return (-sum(dgamma(
    x,
    shape = alph,
    rate = bet,
    log = TRUE
  )))
}
optim(c(alpha, beta), mlogl, x = x)
fitdistr(x,"gamma")
