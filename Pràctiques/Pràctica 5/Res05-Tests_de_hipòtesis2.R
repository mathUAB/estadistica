##################
### PRÀCTICA 5 ###
##################

### Section 1 ###
### Exercise 1
## a)
# o = theta
# L(x,o)=o^sum(x_i)exp(-n*o)/(prod(x_i!)*(1-exp(-o))^n)
# l(x,o)=sum(x_i)*log(o)-n*o + const -n*log(1-exp(-o))
# S(x,o)=sum(x_i)/o-n-n*exp(-o)/(1-exp(-o))=
#       =sum(x_i)/o-n/(1-exp(-o))
# J(x,o)=sum(x_i)/o^2 - n*exp(-o)/(1-exp(-o))^2
## b)
x=c(1:6)
y=c(1486,694,195,37,10,1) 
S=sum(x*y)
n=sum(y)
# The maximum of l(x,o) is equivalent to (sum(x_i) --> S, n --> sum(y))

f=function(o){
  # the minus sign is to maximise the function
  -(S*log(o)-n*o-n*log(1-exp(-o)))
}
# to find an initial approximation we recall that for a poisson X lamba=Exp(X). 
# So we can start with an interval centered at the mean of the data (S/n).
S/n
o_mle=optimise(f,c(0,3))$minimum
o_mle
# observed information
J=function(o){S/o^2-n*exp(-o)/(1-exp(-o))^2}
I=c(-qnorm(0.975),qnorm(0.975))
# instead of putting I(_mle)^-1/2, we use J(o_mle)^-1/2
IConf=o_mle+I/sqrt(J(o_mle))
IConf

### Exercise 2
## a)
# I follow Alejnadra's notes about the likelihood test
x=c(1:6)
y=c(71,28,5,2,2,1)
# We estimate p:
S=sum(x*y)
n=sum(y)
df=length(x)
# The MLE of a geometric distr. is 1/\overline{X}--->1/(S/n)=n/S
LRTmulti=function(S,y){ # S: sum of the values (usually x*y), y: vector of frequency of values
  n=sum(y)
  p=n/S # MLE of a Geo(p)
  pi=(1-p)^(c(1:(length(y)-1))-1)*p # aproximate probabilities for each box
  pi=c(pi,1-sum(pi)) # we append the last box
  expectation = n*pi
  return (expectation)
}
LRTmulti(S,y) # too small (expectations < 5)
# we group cells
x=c(1:3)
y=c(71,28,10)
LRTmulti(S,y) # now its good
# test statistic
Lambda=2*sum(y*log(y/(n*pi))) 
df=df-2
# p-value
pchisq(Lambda,df,lower.tail = F) 
# since it is greater than 0.05 we accept H0

## b)
# H0: p=4/5
# L(x,p)=(1-p)^(sum(x_i)-n)*p^n
# l(x,p)=(sum(x_i)-n)*log(1-p)+n*log(p)
# S = -(sum(x_i)-n)/(1-p)+n/p
# J = (sum(x_i)-n)/(1-p)^2+n/p^2
# I = E(J) = ... = n/(p^2*(1-p))
I=function(p){
  n/(p^2*(1-p))
}
p_mle=n/S
p0=4/5
wald=(p_mle-p0)^2*I(p_mle)
pchisq(wald,df=1)
IConf=c(-qnorm(0.975),qnorm(0.975))
IConf=p_mle+IConf*sqrt(1/I(p_mle))
IConf
IConf[1]<p0 & IConf[2]>p0 # thus, we reject the hypothesis


### Exercise 3
## a)
goles=read.csv2("http://mat.uab.cat/~acabana/data/goles.csv")
spa=goles$Espanya
gol=goles$goles
SPA_matches=rep(gol,times=spa)
# H0: mean(SPA_matches) = 3
test=t.test(SPA_matches,mu=3)
test
test$p.value<0.05
# we reject the H0

## b)
# H0: lamb1 = lamb2 = lamb3 = lamb4 = lamb5
# MLE of a Pois(lamb) is \overline{X}=sum(x_i)/n
log_like=function(x,freq,l0,l1){ 
  # x = number of goals, freq = frequency
  # l0 = lambda under H0, l1 = lambda under H1
  L0=l0^x*exp(-l0)/factorial(x)
  L1=l1^x*exp(-l1)/factorial(x)
  -2*sum(log((L0/L1)^freq))
}
log_like2=function(x,l0,l1){ 
  # x = number of goals, freq = frequency
  # l0 = lambda under H0, l1 = lambda under H1
  L0=l0^x*exp(-l0)/factorial(x)
  L1=l1^x*exp(-l1)/factorial(x)
  -2*sum(log(L0/L1))
}
# data
ENG_matches=rep(gol,times=goles$Inglaterra)
FRA_matches=rep(gol,times=goles$Francia)
ALE_matches=rep(gol,times=goles$Alemania)
ITA_matches=rep(gol,times=goles$Italia)
x=c(ENG_matches,FRA_matches,ALE_matches,ITA_matches,SPA_matches)

matches=colSums(goles)[-1] # we omit the first column (goals)
matches
goals=colSums(goles*gol)[-1]
goals
# Under H0:
lamb0_mle=sum(goals)/sum(matches)
# Under H1
lamb1_mle=goals/matches

m=ncol(goles)-1
Lamb_log_like=numeric(m)
for (i in 1:m){
  # the i + 1 is to avoid the "goles" column
  Lamb_log_like[i]=log_like(gol,goles[,i+1],lamb0_mle,lamb1_mle[i])
}
log_like2(FRA_matches,lamb0_mle,lamb1_mle[2])
LAMB = sum(Lamb_log_like)
nu=(ncol(goles)-1)-1
# we reject if Lamb = -2*log(L0/L1) > const.
pchisq(LAMB,df=nu,lower.tail = F)
# we definitely reject H0.

### Section 2 ###
### Exercise 4
# a)
wind=c(5.86,6.64,8.8,7.81,7.78,7.63,7.51,6.95,5.13,5.2,4.79,5.3)
f=function(x,theta=c(alph,beta)){ # -log-likelihood
  -((theta[1]-1)*sum(log(x))+length(x)*log(theta[1])-length(x)*theta[1]*log(theta[2])-sum(x^theta[1])/theta[2]^theta[1])
}
gamma=0.5772
# theta = 1 / alph
# xi = - log(beta)
# E(X) = xi + theta * gamma
# Var(X) = pi^2 * theta^2 / 6
# => theta = sqrt(6 * Var(X) / pi^2)
# => xi = E(X) - theta * gamma = E(X) - sqrt(6 * Var(X) / pi^2) * gamma
# ==> alph = 1 / sqrt(6 * Var(X) / pi^2)
# ==> beta = exp(sqrt(6 * Var(X) / pi^2) * gamma - E(X))
alph0=1 / sqrt(6 * var(-log(wind)) / pi^2)
beta0=exp(sqrt(6 * var(-log(wind)) / pi^2) * gamma - mean(-log(wind)))
sol=nlm(f,p=c(alph0,beta0),x=wind)
sol
alph_mle=sol$estimate[1]
beta_mle=sol$estimate[2]
# b)
n=length(wind)
x=rweibull(n,shape=alph_mle, scale = beta_mle)
qqplot(x,wind)
# not a straight line

# other approach:
# a = alpha, b = beta
# we have to solve:
# 1 - exp(-(y_k)^a/b^a) = k/(n+1) <==>
# -log(1 - k/(n+1)) = (y_k)^a/b^a <==>
#  1/a*log(-log(1 - k/(n+1))) + log(b) = log(y_k)
wind_sort=sort(wind)
teo_quant = c(1:n)/(n+1)
x=log(-log(1-teo_quant))
y=log(wind_sort)
plot(x,y)
lm.fit(x,y)
lm(y~x) # line y = m*x +n
n=lm(y~x)$coefficients[1]
m=lm(y~x)$coefficients[2]
hat_beta=exp(n)
hat_alph=1/m

# d)
install.packages("fitdistrplus");library(fitdistrplus)
fitdist(wind, "weibull")
# the results are correct.