# ---------------------------------------------------------------------- # 
# Seminari 7 - 2021/2022 - BOOTSTRAP
# ---------------------------------------------------------------------- # 
# Exercici 1: error estándard del RR
# ---------------------------------------------------------------------- # 
library('boot')
# Generamos los datos
aspi=numeric(11037);
plac=numeric(11034);
aspi[1:104] = 1;
plac[1:189] = 1;
RR=((sum(aspi))/length(aspi))/((sum(plac))/length(plac));

# opcio no valida
boot.func = function (x, ind) {
  (sum(x[i,1])/length(x[i,1]))/(sum(x[i,2])/length(x[i,2]))
}

B=10000; BRR=c();
for (b in 1:B) { 
  baspi = sample(aspi, length(aspi), replace=T)
  bplac = sample(plac, length(plac), replace=T)
  BRR[b] = (sum(baspi)/length(baspi))/(sum(bplac)/length(bplac));
}
mean(BRR);
bias = mean(BRR) - RR
nosesg = 2*RR - mean(BRR);
error=sd(BRR)/sqrt(B)
# sort the ratio estimates to obtain bootstrap CI
rat.sorted <- sort(BRR)
# 0.025th and 0.975th quantile gives equal-tail bootstrap CI
CI.bs <- c(rat.sorted[round(0.025*B)], rat.sorted[round(0.975*B+1)])
CI.bs

# ---------------------------------------------------------------------- # 
# Exercici 2
# ---------------------------------------------------------------------- # 
theta=2;
n=25; B=10000;

compute_var = function (theta, n, B) {
  varteo = n*theta^2/((n+1)^2*(n+2))
  x = runif(n, 0, theta);
  theta.hat <- max(x); 
  
  # Non-parametric bootstrap
  mle.boot = function (x, ind) { max(x[ind]) }
  out = boot(x, mle.boot, B)
  varexp.nonpar = var(out$t)
  
  # Parametric bootstrap
  theta.par  = replicate(B, max(runif(n, 0, theta.hat)))
  varexp.par = var(theta.par)
  
  # Resultats
  print(paste0("Ratio entre varianzas non-parametric ", varexp.nonpar/varteo));
  print(paste0("Ratio entre varianzas parametric ", varexp.par/varteo));
  return (list("varexp.par" = varexp.par, "varexp.nonpar"=varexp.nonpar[,1],
               "varteo" = varteo, "theta.par" = theta.par, 
               "theta.nonpar" = out$t[,1], "x"=x, "theta.hat"=theta.hat))
}
out = compute_var(theta, n, B)

