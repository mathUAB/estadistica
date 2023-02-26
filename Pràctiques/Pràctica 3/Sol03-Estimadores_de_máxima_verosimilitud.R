# ------------------------------------------------------------------ # 
# Seminari 3: estimadores de máxima verosimilitud 
# ------------------------------------------------------------------ # 
# Exercici 1: log-verosimilitud para normales
# ------------------------------------------------------------------ # 
mlogLnorm=function(theta=c(0,1),x){
  # el param es sigma (y no sigma^2)
  mu=theta[1];s=theta[2]
  logL=-length(x)*log(s^2)/2-sum((x-mu)^2)/(2*s^2)+length(x)*log(2*pi)
  return(-logL)
}
mlogLnorm2=function(theta=c(0,1),x){
  mu=theta[1];s2=theta[2]
  return(-sum(dnorm(x,mean=mu,sd=s2, log = TRUE)))
  return(-logL)
}
# mlogLnorm2 y mlogLnorm son equivalentes 
x=rnorm(100,mean=3,sd=4)
nlm(mlogLnorm2,p=c(0,1),x=x)
nlm(mlogLnorm,p=c(0,1),x=x)
optim(par=c(0,1),mlogLnorm,x=x)
optim(par=c(0,1),mlogLnorm2,x=x)

# ------------------------------------------------------------------ # 
# Exercici 2: datos de Michelson
# ------------------------------------------------------------------ # 
library(MASS)
data("michelson")
head(michelson)
hist(michelson$Speed)
mich=michelson$Speed[-which.min(michelson$Speed)]
mich=mich[-which.max(mich)]
hist(mich)

x=michelson$Speed
n=length(x)
nlm(mlogLnorm2,p=c(800,78),x=x)
mu=mean(x)

# ------------------------------------------------------------------ # 
# Exercici 3: Poisson truncada
# ------------------------------------------------------------------ # 
sum=18*1+2*18+3*12+4*7+5*5;
n=18+18+12+7+5;
loglikminus=function(t) {
  return(-sum*log(t)+n*t+n*log((1-exp(-t))))
}
score=function(t) {
  return(sum*1/t - n - n*exp(-t)/(1-exp(-t)))
}
info=function(t) {
  return(-sum/t^2 + n*exp(-t)/(1-exp(-t))+n*exp(-2*t)/(1-exp(-t))^2)
} 
optimize(loglikminus,interval=c(0,5))
theta=optimize(loglikminus,interval=c(0,20))$minimum
nlm(loglikminus,p=2)

# ------------------------------------------------------------------ # 
# Exercici 4: Distribución de Pareto
# ------------------------------------------------------------------ # 
mloglpareto=function(x,a=1) {
  n=length(x)
  xm=min(x)
  ll=-n*log(a)-n*a*log(xm)+(a+1)*(sum(log(x)))
  return(ll)
}
npoint <- 101
alphas<- seq(5, 8,length = npoint)
logls <- numeric(npoint)

for (i in 1:npoint) logls[i] <- mloglpareto(x,alphas[i])
plot(alphas, -logls, type = "l")

# install.packages("evir")
require(evir)
library(evir)
data("danish")
head(danish)
x=danish
theta.start=mean(x)/(mean(x)-min(x))
nlm(mloglpareto, theta.start, x=x, hessian=TRUE, fscale=length(x))
