# --------------------------------------------------------- #
# Practica 4
# --------------------------------------------------------- #
# Apartat 1: tests amb dades reals
# --------------------------------------------------------- #
# Exercici 1.1: fitxer skulls
# --------------------------------------------------------- #
# Llegim les dades 
skull=read.table("http://mat.uab.cat/~acabana/data/skull.txt",header=T)
names(skull)
summary(skull$Etruscan)
summary(skull$Italian)
length(skull$Italian) == length(skull$Etruscan)
attach(skull)
Italian = na.omit(Italian)
# Anàlisi gràfic 
boxplot(skull)
qqnorm(Etruscan); qqline(Etruscan);
qqnorm(Italian); qqline(Italian)
## Hacemos el test para las variancias
var.test(Etruscan,Italian)
# Podem acceptar la hipotesis nula: les variancies son iguals

# Calculamos la t observada
tobs=(mean(Etruscan)-mean(Italian))/(sqrt(S2p)*sqrt((1/(length(Etruscan)))+
                                                      (1/length(Italian))))
# Quantil nivell de confiança .975
qt(.975,152)
t.test(Etruscan, Italian, var.equal=T)

# Suposem que no podem asumir que les variancies son iguals. Aleshores el test
# t ens dona graus de llibertat no enters que podem calcular fen servir l'aprox
# de Welch
t.test(Etruscan,Italian) 

# --------------------------------------------------------- #
# Exercici 1.2: fitxer darwin 
# --------------------------------------------------------- #
# Llegim dades
prob6<-read.table("http://mat.uab.cat/~acabana/data/darwin.txt")
names(prob6)
# Experimento pareado
diff6<-prob6$f.cruzada-prob6$f.auto
par(mfrow=c(2,2))
# Anàlisi gràfic
hist(diff6,breaks="Sturges",xlab="Altura en pulgadas",main="Histograma",col= "darkmagenta")
qqnorm(diff6,main="Grafico probabilistico Normal")
qqline(diff6)
boxplot(diff6,xlab=NULL,main=NULL,ylab="Altura en pulgadas",main="Boxplot")
# Estadistic T, R com a calculadora
D <- mean(diff6)
s <- sd(diff6)
n <- length(diff6)
cbind(D, s, n)
T <- D/(s/sqrt(n))
# Utilitzem potència del R
t.test(diff6, alternative="greater")

# --------------------------------------------------------- #
# Apartat 2: potencia del test
# --------------------------------------------------------- #
par(mfrow=(c(1,1)))
delta=seq(from=-4, to=4, by=0.1)
potuni=1-pnorm(1.64-delta)
pot=1-pnorm(1.96-delta)+pnorm(-1.96-delta)
plot(delta,pot,type="l",col="blue",ylim=c(0,1),ylab="Funciones de Potencia")
abline(h=0);abline(h=0.05,lty=2)
lines(delta,potuni,type="l",col="red")

# --------------------------------------------------------- #
# Apartat 3: test t amb dades no normals
# --------------------------------------------------------- #
# Llegim les dades 
lipids=read.table("http://myweb.uiowa.edu/pbreheny/data/lipids.txt",header=T)
dim(lipids)
hist(lipids$TRG)
mean(lipids$TRG)
pop=lipids$TRG
mu=mean(lipids$TRG);mu
N = 10000 ## Numero de simulaciones
n = 25 ## tamaño de la muestra
covered=numeric(N) ## aqui pondremos los resultados
for (i in 1:N){
  sam <- sample(pop, n)
  covered[i] <- t.test(sam, mu=mu)$p.value > 0.05
}
mean(covered)

# --------------------------------------------------------- #
# Exercici 4: repetir per diferents valors de n 
# --------------------------------------------------------- #
n = c(2, 3, 4, 5, 10, 20, 40, 80, 160, 320, 640)
# Declarem una matriu buida
covered = matrix(NA, N, length(n), dimnames=list(1:N, n))
for (i in 1:N){
  for (j in 1:length(n)){
    sam = sample(pop, n[j], replace=TRUE)
    if (sd(sam)==0){
      covered[i,j] = 0
      next
    }
    covered[i,j] = t.test(sam, mu=mean(pop))$p.value > 0.05
  }
}
coverage=apply(covered, 2, mean)
plot(n, coverage, type="l", ylim=c(0.85,1))
abline(h=0.95, col="red")

# Utilitzem eix no linial per apreciar millor
plot(log2(n), coverage, type="l", ylim=c(0.85,1), xaxt="n", xlab="n")
x=seq(1, 9, 2)
axis(1, at=x, labels=2^x)
abline(h=0.95, col="red")

# --------------------------------------------------------- #
# Exercici 5: repetir el test para la varianza
# --------------------------------------------------------- #
n=25 ## 
covered =numeric(N)
for (i in 1:N){
  sam <- sample(pop, n)
  ci <- var(sam)*(n-1)/qchisq(c(0.975,0.025), n-1)
  covered[i] <- (ci[1] < var(pop)) & (var(pop) < ci[2])
}
mean(covered)

# Repetim per diferents valors de n
n = c(2, 3, 4, 5, 10, 20, 40, 80, 160, 320, 640)
coveredmean = matrix(NA, N, length(n), dimnames=list(1:N, n))
coveredvar = matrix(NA, N, length(n), dimnames=list(1:N, n))
for (i in 1:N){
  for (j in 1:length(n)){
    sam = sample(pop, n[j], replace=TRUE)
    if (sd(sam)==0){
      #coveredmean[i,j] = 0
      coveredvar[i,j] = 0
      next
    }
    #coveredmean[i,j] = t.test(sam, mu=mean(pop))$p.value > 0.05
    ci <- var(sam)*(n[j]-1)/qchisq(c(0.975,0.025), (n[j]-1))
    coveredvar[i,j] = (ci[1] < var(pop)) & (var(pop) < ci[2])
  }
}
coveragemean=apply(coveredmean, 2, mean)
coveragevar=apply(coveredvar, 2, mean)
plot(n, coveragevar, type="l", ylim=c(0.80,1))
abline(h=0.95, col="red")
