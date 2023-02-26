##########################
###### 6th PRACTICE ######
##########################

### Section 1 ###

# Exercise 1
skull=read.table("http://mat.uab.cat/~acabana/data/skull.txt",header=T)
etru=skull$Etruscan
ita=na.omit(skull$Italian) # we omit the NA values
# var-test
var.test(etru,ita) # we must assume the variances equal
# t-test
t.test(etru,ita,var.equal = T) # we reject H0: mean_etr = mean_ita
# wilcox-test
wilcox.test(etru,ita,conf.int = T) # we reject H0

# Exercise 2
# H_0: median = 0.618
# H_1: median != 0.618
mu0=0.618
ratio = c(0.693, 0.670, 0.654, 0.749, 0.606, 0.553, 0.601, 0.609, 0.672, 0.663, 
          0.606, 0.615, 0.844, 0.570, 0.933, 0.576, 0.668, 0.628, 0.690, 0.611)
wilcox.test(ratio,mu=mu0,exact=F,conf.int = T)
# Since the p-value > 0.05 we accept H_0.

# Exercise 3
# binom-test
ratio_bin=ratio-mu0
ratio_success=length(ratio_bin[ratio_bin>0]) # number of positive values
n=length(ratio)
binom.test(ratio_success,n,p=0.5)
# Since the p-value > 0.05 we accept H_0.

### Section 2 ###
# H_0: X_i and Y_j come from the same distribution.
# H_1:X_i and Y_j do not come from the same distribution.
x = c(1.15, 0.88, 0.9, 0.74, 1.21)
y = c(0.8, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
ansari.test(x,y)
# Since the p-value > 0.05 we accept H_0.

### Section 3 ###
lluvia=read.table("http://mat.uab.cat/~acabana/data/lluvia.txt")
x=lluvia$nobomb ; y=lluvia$bomb
x
y
# Exercise 1
qqplot(x,y)
boxplot(x)
qqnorm(y)
hist(x)
hist(y)
# Exercise 2
t.test(x,y)
# Since the p-value > 0.05 we accept H_0.
# Exercise 3
xx=log(x)
yy=log(y)
hist(xx) # More symmetric than the previous data
hist(yy) # More symmetric than the previous data
t.test(xx,yy)
# Since the p-value < 0.05 we reject H_0.
# Exercise 4
wilcox.test(x,y)
# Since the p-value < 0.05 we reject H_0. 
# This test is more suitable for the kind of problem because we 
# know that the sample is not normal (from the first histograms)

## Permutation test
# Exercise 1
# H0: mu_x = mu_y
# H1: mu_x < mu_y
# Exercise 2
# We take the statistic mu_x-mu_y < const.
# Exercise 3, 4, 5
muestra=c(x,y)
original=mean(x)-mean(y)
rep=1000
distrib=numeric(rep)
l=length(x)+length(y)
for(i in 1:rep){
  sam = sample(muestra,l)
  new_x=sam[1:length(x)]
  new_y=sam[(length(x)+1):l]
  distrib[i]=mean(new_x)-mean(new_y)
}
# Exercise 6
hist(distrib,xlim=c(-2,4))
abline(v=quantile(distrib,0.05),col='red')
abline(v=quantile(distrib,0.95),col='red')
points(original,0)
# we reject the hypothesis
