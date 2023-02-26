##########################
###### 2nd PRACTICE ######
##########################

### Section 1 ###

# Example 1
N = 1000
n = 10
mu = 4
sigma = 2
x = rnorm(N * n, mu, sigma)
dim(x) = c(N, n)
x
M = apply(x, 1, mean) # 1 indicates that the function 'mean' is applied to the rows
err = qnorm(0.975) * sigma / sqrt(n)
A = M - err
B = M + err
u = (A < mu) & (B > mu)
sum(u) / length(u)

# Exercise 1
N = 300
n = 20
p = 0.4
x = rbinom(N, n, p)
sigma = sqrt(n * p * (1 - p))
alpha = 1 - 0.99
err = qbinom(1 - alpha / 2, n, p) * sigma / sqrt(N)
mu = mean(x)
A = mu - err
B = mu + err
A;B
n*p
n * p > A & n * p < B # The interval does contain the theoretical mean


# Exercise 2
N = 1000
n = 20
m = 3
sigma = 1
x = rnorm(N * n, m, sigma)
dim(x) = c(N, n)
M = apply(x, 1, mean)
alpha = 1 - 0.90
err = qnorm(1 - alpha / 2) * sigma / sqrt(n)
A = M - err
B = M + err
u = (A < m) & (B > m)
sum(u) / length(u)

# Exercise 3
x = c(5.2, 4.8, 5.3, 5.7, 5.0, 4.7, 4.3, 5.5, 5.4, 5.1, 4.9, 5.8)
t.test(x, conf.level = 0.95)$conf.int
t.test(x, conf.level = 0.99)$conf.int # The interval is wider than the previous one.

#################

### Section 2 ###

# Exercise 4
n = 200
p = 0.08
prop.test(n * p, n, conf.level = 0.92)$conf.int

#################

### Section 3 ###

# Example 2
n = 10
gm = 0.95
alp = 1 - gm
a = qchisq(alp / 2, n - 1)
b = qchisq(1 - alp / 2, n - 1)
z = seq(0, 30, by = 0.1)
y = dchisq(z, n - 1)
plot(
  z,
  y,
  type = 'l',
  xlab = "",
  ylab = "",
  ylim = c(0.0041, 0.11)
)
lines(c(a, a), c(0, dchisq(a, n - 1)))
lines(c(b, b), c(0, dchisq(b, n - 1)))

# Exercise 5
x = c(7.9, 8.3, 4.8, 8.4, 7.9, 5.2, 5.6, 3.2, 9.1, 7.7, 6.5, 4.4)
n=length(x)
v = var(x)
gm = 0.93
alp = 1 - gm
a = qchisq(1 - alp / 2, n - 1)
b = qchisq(alp / 2, n - 1)
A = (n - 1) * v / a
B = (n - 1) * v / b
A;B

#################

### Section 4 ###

# Exercise 6
x = c(93, 106, 87, 92, 102, 95, 88, 110)
y = c(92, 102, 89, 92, 101, 96, 88, 105)
t.test(x, y, paired = TRUE, level.conf = 0.97)$conf.int 
# Since the interval contain both positive and negative values, we can't affirm 
# nothing about about the decrase of arterial pressure.
mean(x) - mean(y)

# Exercise 7
L = c(6.7, 1.9, 6.4, 4.8, 2.6, 4.9, 6.7, 3.6, 1.5, 1.2, 2.4, 2.4, 4.6, 4.9, 4.8)
S = c(6.2, 3.7, 4.5, 6.2, 6.0, 5.3, 3.5, 3.6, 3.1, 0.3, 5.3, 4.5, 4.5, 3.6, 4.5)
n = length(L)
varL = 3.5
varS = 2.2
gm = 0.95
alp = 1 - gm
err = qnorm(1 - alpha / 2) * sqrt(varL / n + varS / n)
mudiff = mean(L) - mean(S)
A = mudiff - err
B = mudiff + err
A;B

# Exercise 8
# equal variances
t.test(L, S, var.equal = TRUE, level.conf = 0.95)$conf.int
# different variances
t.test(L, S, var.equal = FALSE, level.conf = 0.95)$conf.int

#################

### Problems

# Problem 1
setwd("/home/victor/Downloads/")
load("malaria.RData")
names(malaria)
edat = malaria[, 1]
sexe = malaria[, 2]
n = length(edat)
# a)
t.test(edat, conf.level = 0.95)$conf.int
# b)
numH = sum(sexe == 'H')
prop.test(numH, n, conf.level = 0.92)$conf.int
numD = sum(sexe == 'D')
prop.test(numD, n, conf.level = 0.92)$conf.int
# c)
edatHomes = malaria$Edat[malaria$Sexe == 'H']
edatDones = malaria$Edat[malaria$Sexe == 'D']
t.test(edatHomes,
       edatDones,
       var.equal = TRUE,
       level.conf = 0.93)$conf.int
# d)
v = var(edat)
gm = 0.90
alp = 1 - gm
a = qchisq(1 - alp / 2, n - 1)
b = qchisq(alp / 2, n - 1)
A = (n - 1) * v / a
B = (n - 1) * v / b
A;B

# Problem 2
setwd("/home/victor/Downloads/")
load("Nadons.RData")
ph = Nadons[, 1]
n = length(ph)
# a)
t.test(ph, conf.level = 0.90)$conf.int
# b)
Fum = Nadons$pH[Nadons$Madre == 'F']
t.test(Fum, conf.level = 0.90)$conf.int
NFum = Nadons$pH[Nadons$Madre == 'NF']
t.test(NFum, conf.level = 0.90)$conf.int
# c)
t.test(Fum, NFum, var.equal = TRUE, level.conf = 0.95)$conf.int # suppose they have the same variance
# We can't deduce anything because the interval contains both positive and negative values.

# Problem 3
A = matrix(
  c(7.9, 5.4, 8.3, 6.2, 8.2, 8.3, 7.8, 4.9, 6.2, 8.9, 7.8, 9.7, 7.2,
    8.2, 5.7, 6.0, 4.2, 7.5, 4.6, 6.2, 5.2, 5.3, 9.2, 6.5, 8.1, 4.5),
  nrow = 2
)
A
P1 = A[1,]
P2 = A[2,]
n = length(P1)
# a)
t.test(P1, conf.level = 0.90)$conf.int
t.test(P2, conf.level = 0.90)$conf.int
# from here we can't deduce which average mark was the highest.
t.test(P1, conf.level = 0.80)$conf.int
t.test(P2, conf.level = 0.80)$conf.int
# again, we can't deduce which average mark was the highest.
# b)
t.test(P1, P2, var.equal = TRUE, level.conf = 0.92)$conf.int # suppose they have the same variance
# We cannot deduce anything because the interval contains both positive and negative values.
# c)
F = P1 * 0.4 + P2 * 0.6
t.test(F, conf.level = 0.95)$conf.int
# d)
gm = 0.93
alp = 1 - gm
a = qchisq(1 - alp / 2, n - 1)
b = qchisq(alp / 2, n - 1)

v1 = var(P1)
A1 = (n - 1) * v1 / a
B1 = (n - 1) * v1 / b
A1;B1

v2 = var(P2)
A2 = (n - 1) * v2 / a
B2 = (n - 1) * v2 / b
A2;B2
# We can't deduce anything.