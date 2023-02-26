# ------------------------------------------------------------------ #
# Seminario 6 - 2022
# M. Barcelona
# mié 04 may 2022 13:16:25 CEST
# ------------------------------------------------------------------ #
# Test de Wilcoxon
# ------------------------------------------------------------------ #
# Datos skulls
skull=read.table("http://mat.uab.cat/~acabana/data/skull.txt",header=T)
names(skull)
attach(skull)
Italian=na.omit(Italian)
n=length(Italian)
m=length(Etruscan)
wilcox.test(Etruscan,Italian)
t.test(Etruscan, Italian)

# Datos de Shoshone
ratio=c(0.693,0.670,0.654,0.749,0.606,0.553,0.601,0.609,0.672,0.663,0.606,0.615,
        0.844,0.570,0.933,0.576,0.668,0.628,0.690,0.611)

qqnorm(ratio); qqline(ratio)
t.test(ratio,mu=0.618)
wilcox.test(ratio,mu=0.618)

# Prueba del signo
sum((ratio-0.618)>0)
length(ratio)
binom.test(11,20)

# ------------------------------------------------------------------ #
# Test de Ansari Bradley
# ------------------------------------------------------------------ #
y=c(0.8 , 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
x=c(1.15, 0.88, 0.9 , 0.74, 1.21)
ansari.test(x,y)

# ------------------------------------------------------------------ #
# Pruebas exactas y test de permutaciones
# ------------------------------------------------------------------ #
# Leemos los datos
nobomb = c(58.8726, 39.117, 18.4721, 17.5297,
           14.8817, 12.9936, 8.9929, 6.0795, 4.984, 3.9476, 3.9196, 3.224,
           2.6193, 2.5119, 2.4502, 2.4495, 2.1614, 2.1512, 1.7357, 1.1013,
           1.0268, 0.8335, 0.5592, 0.3995, 0.1186, 0.0708)
bomb = c(132.0633,
         82.4972, 79.6845, 46.4593, 34.603, 22.6582, 19.5007, 15.2783,
         14.5006, 14.2957, 12.9937, 12.3675, 11.597, 10.1155, 8.9492,
         6.1029, 5.8555, 5.2969, 4.5607, 3.8821, 2.34, 2.032, 1.0623,
         0.7754, 0.5372, 0.1282)# Constuimos un vector para cada grupo

lluvia=read.table("http://mat.uab.cat/~acabana/data/lluvia.txt")
x=lluvia$nobomb ; y=lluvia$bomb
# 1. Analisis grafico sobre los datos, distribucion?
#Dibujitos
par(mfrow=c(2,1))
hist(nobomb,probability=TRUE,breaks=seq(0,140,by=20), ylim=c(0,0.1)); lines(density(nobomb))
hist(bomb,probability=TRUE,breaks=seq(0,140,by=20),ylim=c(0,0.1)); lines(density(bomb))

boxplot(x,y,names=c("No bombardeadas","Bombardeadas"))$stats
## x_(i) contra y_(i).  Las nubes bombardeadas producen + lluvia
qqplot(x,y)
abline(0,1)

# 2. Test t para comprobar medias de ambas poblaciones. Es adecuado?
t.test(x,y, paired=TRUE)

# 3. Transformar los datos para tener distribucion simetrica. Volver a test t.
# Transformaciones posibles, sqrt(x), 1/x i ln x
xnew <- sqrt(x); ynew<-sqrt(y)
hist(xnew,probability=TRUE); lines(density(xnew))
hist(ynew,probability=TRUE); lines(density(ynew))
t.test(xnew,ynew)

# 4. Test de Wilcoxon. Es mas adecuado? Por que
wilcox.test(x,y)
