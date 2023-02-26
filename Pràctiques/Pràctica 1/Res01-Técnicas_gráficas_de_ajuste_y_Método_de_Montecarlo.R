##########################
###### 1st PRACTICE ######
##########################

### Section 1 ###

### 1
f1=function(n){
  x=sort(rexp(n,rate = 5))
  y=c(1:n)/(n+1)
  Y=-log(1-y)
  plot(x,Y)
  abline(lm(Y~x))
  return(lm(Y~x))
}
f1(15)
f1(50)  
f1(100)

### 2
install.packages("evir")
library ("evir")
data(danish)
Y=c(1:length(danish))/(length(danish)+1)
plot(log(sort(danish)),log(1-Y),asp=1)
abline(lm(log(1-Y)~log(sort(danish))))
# We deduce, thus, that it is plausible to think that the data come from a Pareto distribution.

### 3
library(MASS)
data(michelson);names(michelson)
qqnorm(michelson$Speed)
qqline(michelson$Speed)

#################

### Section 2 ###

# Example
nn =20
muestra.1 = rnorm(nn)
muestra.2 = rnorm(nn)
cor(muestra.1,muestra.2)
plot(muestra.1,muestra.2, pch=16)
abline(h=0, lty=2)
abline(v=0, lty=2)
title(paste("r =", round(cor(muestra.1, muestra.2), 3)))

correlacion=function(n){
  x=rnorm(n);y=rnorm(n)
  cor(x,y)
}
correla=replicate(1000,correlacion(20))
quantile(correla,c(0.05,0.95))
hist(correla,probability=T,col="cornsilk")
abline(v=quantile(correla,c(0.1,0.9)),col="darkblue")
points(mean(correla),0,pch=8,col="maroon4")

# Taxis
# 1
library(purrr)
f=function(N,n){
  X=rdunif(n,N,1)
  Y=mean(X)
  c(max(X),2*Y)
}
# take N=100 y n=20
N=100;n=20
f(N,n) 
taxis=replicate(1000,f(N,n))
# 1st row ~ max(Y_i); 2nd row ~ 2*mean(Y)
hist(taxis[1,],probability=T,col=rgb(1,0,1,1/4),xlim=c(60,140),ylim=c(0,0.19))
hist(taxis[2,],probability=T,col=rgb(0,0,1,1/4),add=T)
exp1=mean(taxis[1,]);bias1=exp1-N
exp2=mean(taxis[2,]);bias2=exp2-N
bias1;bias2 # bias
err1=var(taxis[1,])+bias1^2
err2=var(taxis[2,])+bias2^2
err1;err2 # MSE
#################