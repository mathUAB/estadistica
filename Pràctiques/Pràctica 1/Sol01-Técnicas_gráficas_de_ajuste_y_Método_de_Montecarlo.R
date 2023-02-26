# ----------------------------------------------------------------- # 
# Seminari 1: tecnicas graficas de ajuste y Montecarlo
# ----------------------------------------------------------------- # 
# Exercici 1: dsitribució exponencial
# ----------------------------------------------------------------- # 
qqexpo=function(n,rate) {
  expo=rexp(n,rate)
  par(mfrow=c(1,2))
  hist(expo, probability = T, col='cornflowerblue')
  z=-log(1-(1:n)/(n+1))
  plot(sort(expo),z)         # sort ordena
  print(lm(z~sort(expo)))
  abline(lm(z~sort(expo)))   # lm ajusta models lineals
}
qqexpo(15,5)
qqexpo(100,5)

# ----------------------------------------------------------------- # 
# Exercici 2: distribució de Pareto
# ----------------------------------------------------------------- # 
install.packages("evir")
library(evir)
data(danish)
? danish
boxplot(danish)
head (danish)
hist(danish, probability = T , col='cornflowerblue')

qqpareto = function(x) {
  n=length(x)
  z=-log(1-(1:n)/(n+1))
  # y=numeric(n)
  # y=c()
  # for (k in 1:n) {
  #   y[k] = 1-(k/(n+1))
  # }
  plot(log(sort(x)), z)
  abline(lm(z~log(sort(x))))
}
qqpareto(danish)

# ----------------------------------------------------------------- # 
# Exercici 3: taxis
# ----------------------------------------------------------------- # 
# Apartat 1
taxi = function(N,n) {
  # Uniforme discreta de tamaño n en 1:N
  y=sample(N, size=n, replace=T) 
  N1=max(y)
  N2=2*mean(y)
  c(N1=N1,N2=N2)
}
taxi(100,5);

# Apartat 2: Experimento de simulacion con 1000 repeticiones
rep=1000
estimadores=replicate(rep,taxi(100,5)) #2 filas 100 columnas
estimadores

# Apartat 3: Histrograma de las distribuciones de ambos estimadores
par(mfrow=c(1,2))
hist(estimadores[1,])
hist(estimadores[2,])

# Apartat 4: Sesgos medios
sesgo=c(mean(estimadores["N1",])-100,mean(estimadores["N2",])-100)
#iconf para los sesgos
(mean(estimadores["N1",])-100)+1.96*c(-1,1)*sd(estimadores["N1",])/sqrt(rep)
(mean(estimadores["N2",])-100)+1.96*c(-1,1)*sd(estimadores["N2",])/sqrt(rep)
# N2 tiene menos sesgo, aunque en realidad deberia ser insesgado
# Pero no necesariamente es mejor: tiene error absoluto mas grande
error.abso=abs(estimadores-100)
boxplot(t(error.abso))
apply(t(error.abso),2,mean)
apply(t(error.abso),2,sd)/sqrt(rep)
# N1 tiene error absoluto menor

# Apartat 5: Calculamos el error cuadratico medio E(N_i-N)^2, i=1,2
var=c(var(estimadores["N1",]),var(estimadores["N2",]))
ECM=sesgo^2+var;
