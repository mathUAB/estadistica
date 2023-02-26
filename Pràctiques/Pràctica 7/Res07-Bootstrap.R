####################
### 7th PRACTISE ###
####################

### Exercise 1
asp=rep(c(1,0),times=c(104,10933))
n1=length(asp)
pla=rep(c(1,0),times=c(189,10845))
n2=length(pla)
N=1000
boot=numeric(N)
for(i in 1:N){
  a=sample(asp,n1,replace = T)
  b=sample(pla,n2,replace = T)
  Ra=sum(a)/n1
  Rb=sum(b)/n2
  boot[i]=Ra/Rb  
}
mean(boot)
boot_sd=sd(boot)
boot_sd


### Exercise 2

## a) 
# Already done before

## b)
# P(X_(n) < t) = P(X1 < t, ... , Xn < t)
#              = P(X1 < t)^n = 
# = {
#    0 if t < 0; 
#    (t/theta)^ if 0 < t < theta;
#    1 if t> theta
#   }
## c)

# Integrating for getting the expectation we get E(X_(n)) = n/(n+1)*theta
# Integrating for getting the second moment we get E(X_(n)^2) = n/(n+2)*theta^2
# So V(X_(n))=n/(n+2)*theta^2-n^2/(n+1)^2*theta^2= n/((n+1)^2*(n+2))*theta^2

## d)
f=function(n,B,theta){
  x=runif(n,0,theta)
  hat_theta=max(x)
  boot=numeric(B)
  for(i in 1:B){
    a=sample(x,n,replace = T)
    boot[i]=max(a)
  }
  return(c(hat_theta,boot))
}
g=function(n,hat_theta,boot){
  var_boot=var(boot)
  var_est=n/((n+1)^2*(n+2))*hat_theta^2
  return(var_est/var_boot)
}
X=f(25,5000,3)
g(25,X[1],X[-1])
X=f(100,1000,3)
g(100,X[1],X[-1])
X=f(1000,2000,3)
g(1000,X[1],X[-1])
# Other apporach:
library(boot)
boot.fun=function(x,i){max(x[i])}
f=function(n,B,theta){
  x=runif(n,0,theta)
  boot(x,boot.fun,R=B)
}
# The values do not approximate to 1 (the function max() isn't Hadamard 
# differentiable and we can't guarantee that the bootstrap will work)

## e)
# P(n(^theta_n-^theta*_n) <= 0)= P(^theta_n <= ^theta*_n) = 
# P(^theta_n <= sum(^theta_n_b)/B) = P(B*^theta_n <= sum(^theta_n_b)) =
# = P(^theta_n_b=^theta_n for all b)
# = 1- P("algun ^theta_n_b no és ^theta_n")
# = 1 - ((n-1)/n)^n ja que la probabilitat d'obtenir el maxim és 1/n
# = 1-(1-1/n)^n

## f)
# sup_t|P(Tn <= t) - P(T*_n <= t)| >= |P(Tn <= 0) - P(T*_n <= 0)| >=
# >= |0 - P(T*_n <= 0)| = 1-(1-1/n)^n ---> 1-e^-1
X=f(1000,2000,3)
boot=X$t
hat_theta=X$t0
hist(boot)
mean(boot==hat_theta)
1-exp(-1)

## g)
# We have that:
# P(T_n <= t)    = P(^theta_n >= theta - t/n)
# (suppose n>>t) = 1-P(^theta_n < theta - t/n)
# (suppose n>>t) = 1 - [(theta-t/n)/theta]^n
#                = 1 - [1-t/(n*theta)]^n ---> 1 - exp(-t/theta)
# Similarly we have:
# P(T*_n <= t)    = P(^theta*_n >= ^theta_n - t/n)
# (suppose n>>t) = 1-P(^theta_n < theta - t/n)
# (suppose n>>t) = 1 - [(theta-t/n)/theta]^n
#                = 1 - [1-t/(n*theta)]^n ---> 1 - exp(-t/theta)
# which is the distribution of a Exp(1/theta).
n=100;B=5000
x=runif(n,0,3)
hat_theta=max(x)
boot=numeric(B)
for(i in 1:B){
  a=runif(n,0,hat_theta)
  boot[i]=max(a)
}
mean(boot)
hat_theta
