##########################
###### 4th PRACTICE ######
##########################

### Section 1 ###

# Exercise 1
skull = read.table("http://mat.uab.cat/~acabana/data/skull.txt", header =
                     TRUE)
names(skull)
etruskan = skull$Etruscan
etruskan
italian = na.omit(skull$Italian)
italian
# H_0: mean(etruscan) - mean(italian) = 0
# H_1: mean(etruscan) - mean(italian) =! 0
qqnorm(italian)
qqplot(etruskan, italian)
# looking at the graphics, we see that we can suppose that the data comes from a normal distribution
var.test(etruskan, italian) # we can suppose equal variances
t.test(etruskan, italian, var.equal = TRUE)
# Since the confidence interval does not contain the 0, we reject H_0.

# Exercise 2
darwin = read.table("http://mat.uab.cat/~acabana/data/darwin.txt", header =
                      TRUE)
names(darwin)
cruz = darwin$f.cruzada
cruz
auto = darwin$f.auto
auto
# H_0: mean(cruz) - mean(auto) = 0
# H_1: mean(cruz) - mean(auto) =! 0
qqnorm(cruz)
qqplot(cruz, auto)
# we may suppose normality on the data
var.test(cruz, auto) # ==> different variances
t.test(cruz, auto)
# Since the confidence interval does not contain the 0, we reject H_0.

### Section 2 ###

# a)
# H_0: mu=mu_0=0
# H_1: mu=mu_1>0
# T = (X(bar)-mu0)/(sigma/sqrt(n)). (test statistic)
# By N&P, we reject when T > const.
# 1-beta = P(T>const.|mu=m1)
# alpha = P(T>const.|mu=0)
alpha = 0.05
n = 20
C1 = qnorm(1 - alpha, mean = 0, sd = 1)
C1
# const.=C and therefore the power is: 
# 1 - beta = P(T > C | mu = m1) <==>
# 1 - beta = P((X(bar) - mu1 + mu1) / (sigma / sqrt(n)) > C | mu = m1) <==>
# 1 - beta = P(Z + mu1 / (sigma / sqrt(n)) > C | mu = m1) (Z ~ N(0,1)) <==>
# 1 - beta = P(Z > C - mu1 / (sigma / sqrt(n))) <==>
# 1 - beta = 1 - P(Z <= C - mu1 / (sigma / sqrt(n)))
# Let delta = mu1 / (sigma / sqrt(n)) 
# Resolution:
delta = seq(from = -4, to = 4, by = 0.1)
pow1 = 1 - pnorm(C1 - delta)
plot(delta, pow1, type = 'l')

# b)
# H_0: mu=mu_0 = 0
# H_1: mu=mu_1 != 0
# T = (X(bar)-mu0)/(sigma/sqrt(n)). (test statistic)
# By N&P, we reject when |T| > const.
# 1-beta = P(|T|>const.|mu=m1)
# alpha = P(|T|>const.|mu=0)
C2 = qnorm(1 - alpha / 2, mean = 0, sd = 1)
C2
# 1 - beta = P( |T| > C | mu = m1) <==>
# 1 - beta = P(T > C | mu = m1) + P(T < -C | mu = m1) <==>
# 1 - beta = P((X(bar) - mu + mu) / (sigma / sqrt(n)) > C | mu = m1) + 
#          + P((X(bar) - mu + mu) / (sigma / sqrt(n)) < -C | mu = m1) <==>
# 1 - beta = P(Z > C - mu / (sigma / sqrt(n))) +  P(Z < -C - mu / (sigma / sqrt(n)))
# 1 - beta = 1 - P(Z <= C - mu / (sigma / sqrt(n))) +  P(Z < -C - mu / (sigma / sqrt(n)))
# fem delta = mu / sqrt(n)
delta = seq(from = -4, to = 4, by = 0.1)
pow2 = 1 - pnorm(C2 - delta) + pnorm(-C2 - delta)
plot(delta, pow2, type = 'l')

### Section 3 ###

# Exercise 4
lipids = read.table("http://mat.uab.cat/~acabana/data/lipids.txt", header = TRUE)
names(lipids)
L=lipids$TRG
mu0=mean(L)
qqnorm(L) # clearly the data is not normal
N = 1000
cover=function(n){
  covered = numeric(N) # vector of 0's and length N.
  for (i in 1:N) {
    sam = sample(L, n)
    if(sum(sam-sam[1])==0){ # to solve the case when 'sam' has all its values equal, i.e. c(40,40)
      covered[i]=TRUE
    } 
    else{ # normal case
      covered[i]=t.test(sam,mu=mu0)$p.value>0.05
    }
  }
  return (covered)
}
n=c(2, 3, 4, 5, 10, 20, 40, 80, 160, 320, 640)
y=c()
for (j in n){
  y=c(y,cover(j))
}
Y=matrix(y,nrow=N,ncol=11)
coverage=apply(Y,2,mean) # '2' indicates that the mean is taken over the columns
coverage
# plot with linear axis
plot(n,coverage,type='l')
# plot with linear axis
plot(log2(n),coverage,type='l',ylim=c(0.85,1),xlab='n',xaxt='n')
x=seq(1,9,2) # from 2^1 to 2^9 with steps of '2', in the following command
axis(1,at=x,labels=2^x)
abline(h=0.95,col='red')

# Exercise 5
sigma0=var(L)
cover_var=function(n){
  covered = numeric(N) # vector of 0's and length N.
  for (i in 1:N) {
    sam = sample(L, n)
    R=(n-1)*var(sam)/sigma0 # test statistic
    # Recall that the p-values are (denoting T as the test statistic, and t the observed value):
    # p = P(T >= t | H0) if the test is a one-sided right-tail test
    # p = P(T <= t | H0) if the test is a one-sided left-tail test
    # p = 2*min{P(T >= t | H0), P(T <= t | H0)} if the test is a two-sided test
    covered[i]=2*min(pchisq(R,df=n-1,lower.tail = FALSE),pchisq(R,df=n-1))>0.05 # p-value.
  }
  return (covered)
}
y=c()
for (j in n){
  y=c(y,cover(j))
}
Y=matrix(y,nrow=N,ncol=11)
coverage=apply(Y,2,mean) # '2' indicates that the mean is taken over the columns
coverage
# plot with linear axis
plot(n,coverage,type='l')
# plot with linear axis
plot(log2(n),coverage,type='l',ylim=c(0.85,1),xlab='n',xaxt='n')
x=seq(1,9,2) # from 2^1 to 2^9 with steps of '2', in the following command
axis(1,at=x,labels=2^x)
abline(h=0.95,col='red')

### Section 4 ###

# Exercise 6
cruz # X_i
auto # Y_i
# H_0: mean(cruz) - mean(auto) = 0
# H_1: mean(cruz) - mean(auto) =! 0
wilcox.test(cruz,auto,exact=F,conf.int = T) # an exact p-value isn't computed.
# Since the p-value < 0.05 we reject H_0.

# Exercise 7
# H_0: median = 0.618
# H_1: median != 0.618
ratio = c(0.693, 0.670, 0.654, 0.749, 0.606, 0.553, 0.601, 0.609, 0.672, 0.663, 
          0.606, 0.615, 0.844, 0.570, 0.933, 0.576, 0.668, 0.628, 0.690, 0.611)
wilcox.test(ratio,mu=0.618,exact=F,conf.int = T)
# Since the p-value > 0.05 we accept H_0.

### Section 5 ###
# H_0: X_i and Y_j come from the same distribution.
# H_1:X_i and Y_j do not come from the same distribution.
x = c(1.15, 0.88, 0.9, 0.74, 1.21)
y = c(0.8, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
ansari.test(x,y)
# Since the p-value > 0.05 we accept H_0.