# --------------------------------------------------------- #
# Fri 29 Apr 2022 12:22:13 PM CEST
# --------------------------------------------------------- #
# Exercici 1
# --------------------------------------------------------- #
sum=1486+2*694+3*195+4*37+5*10+6;
n=1486+694+195+37+10+1;
f=c(1486,694,195,37,10,1)
g=c(1,2,3,4,5,6)
sum=0
for (i in 1:length(f)) { sum=sum+f[i]*g[i] }
# Equival a sum=sum(f*g)
n=sum(f)

loglik = function (t) {
  return(-sum*log(t)+n*t+n*log(1-exp(-t)))
}
theta=optimize(loglik,interval=c(0,20))$minimum
Info=3663/theta^2- (2423*exp(-theta)/(exp(-theta)-1)^2)
sigma=sqrt(1/Info)
interval=theta+c(-1,1)*qnorm(0.975)*sigma

# --------------------------------------------------------- #
# Exercici 2
# --------------------------------------------------------- #
x=c(1,2,3,4,5,6);
num=c(71,28,5,2,2,1);
# EMV 
sum(num*x);sum(num)
media=sum(num*x)/sum(num);
p=1/media;
# Calculo probabilitats
xx=x[-length(x)]
pi=(1-p)^(xx-1)*p
pi=c(pi,1-sum(pi))
pi
# Calculo valors esperats
esp=sum(num)*pi
esp
# Agrupo caselles
x=c(1,2,3);
num=c(71,28,10);
xx=x[-length(x)]
pi=(1-p)^(xx-1)*p
pi=c(pi,1-sum(pi))
# Calculo valors esperats
esp=sum(num)*pi
esp
# LRT
lam=2*sum(num*log(num/(sum(num)*pi)));
pvalor=1-pchisq(lam,1);
pvalor
# Test de Wald para p=4/5
info=sum(num)/(p^2*(1-p))
p0=4/5
W=(p-p0)^2*info;
# Un int de confianza 95% para p estadistico dado por
sigma=sqrt(1/info)
p+c(-1,1)*1.96*sigma

