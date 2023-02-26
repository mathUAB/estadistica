# ------------------------------------------------------------------ #
# Seminari 2: intervals de confiança
# ------------------------------------------------------------------ #
# Exercici 1: distribució binomial
# ------------------------------------------------------------------ #
N <- 300
n <- 20
p <- 0.4
sigma <- n*p*(1-p)
alpha=0.1
x <- rbinom(N, n, p)
m <- mean(x)
err <- qnorm(1-alpha/2)*sigma/sqrt(n)
A <- m - err
B <- m + err

# ------------------------------------------------------------------ #
# Exercici 2: distribució normal
# ------------------------------------------------------------------ #
N <- 1000
n <- 20
m <- 3
sigma <- 1
alpha <- .1

x <- rnorm(N*n, m, sigma)
dim(x) <- c(N,n)

M <- apply(x, 1, mean)
err <- qnorm(1-alpha/2) * sigma / sqrt(n)
A <- M - err
B <- M + err

u <- (A<m) & (B>m)
sum(u)/N

# ------------------------------------------------------------------ #
# Exercici 3: variancia desconeguda
# ------------------------------------------------------------------ #
x <- c(5.2,4.8,5.3,5.7,5.0,4.7,4.3,5.5,5.4,5.1,4.9,5.8)
t.test(x)$conf.int
t.test(x, conf.level = 0.90)$conf.int

# ------------------------------------------------------------------ #
# Exercici 4: proporcions
# ------------------------------------------------------------------ #
p <- 0.08
n <- 200
x <- p*n
prop.test (x , n , conf.level = 0.92) $ conf.int

# ------------------------------------------------------------------ #
# Exercici 5: variancia 
# ------------------------------------------------------------------ #
notes <- c(7.9, 8.3, 4.8, 8.4, 7.9, 5.2, 5.6, 3.2, 9.1, 7.7, 6.5, 4.4)
n <- length(notes)
gm <- 0.93
alph <- 1 - gm
a <- qchisq(alph/2 , n-1)
b <- qchisq(1-alph/2, n-1)
linf <- (n-1)*var(notes) / b
lsup <- (n-1)*var(notes) / a

# ------------------------------------------------------------------ #
# Exercici 6: dades aparellades 
# ------------------------------------------------------------------ #
abans <- c(93, 106, 87, 92, 102, 95, 88, 110)
despres <- c(92, 102, 89, 92, 101, 96, 88, 105)
niv_conf <- 0.97
t.test(abans, despres, paired = TRUE, conf.level = niv_conf)$conf.int

# ------------------------------------------------------------------ #
# Exercici 7: mostres independents
# ------------------------------------------------------------------ #
L <- c(6.7, 1.9, 6.4, 4.8, 2.6, 4.9, 6.7, 3.6, 1.5, 1.2, 2.4, 2.4, 4.6, 4.9,
       4.8)
S <- c(6.2, 3.7, 4.5, 6.2, 6.0, 5.3, 3.5, 3.6, 3.1, 0.3, 5.3, 4.5, 4.5, 3.6,
       4.5)
n <- length(L)  # OBS: En aquest cas n=nx=ny
sigma2_L <- 3.5
sigma2_S <- 2.2
mean_L <- mean(L)
mean_S <- mean(S)
g <- .95
alpha <- 1-g
z <- qnorm(1-alpha/2)
err <- z * sqrt( (sigma2_L/n) + (sigma2_S/n) )
linf <- mean_L - mean_S - err
lsup <- mean_L - mean_S + err

# ------------------------------------------------------------------ #
# Exercici 8: variancies desconegudes 
# ------------------------------------------------------------------ #
L <- c(6.7, 1.9, 6.4, 4.8, 2.6, 4.9, 6.7, 3.6, 1.5, 1.2, 2.4, 2.4, 4.6, 4.9,
       4.8)
S <- c(6.2, 3.7, 4.5, 6.2, 6.0, 5.3, 3.5, 3.6, 3.1, 0.3, 5.3, 4.5, 4.5, 3.6,
       4.5)
# Variancies iguals
t.test(L, S, var.equal = TRUE, level.conf=.95)$conf.int
# Variancies diferents
t.test(L, S, var.equal = FALSE, level.conf=0.95)$conf.int
