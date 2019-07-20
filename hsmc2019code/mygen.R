library(randnet)
library(tidyverse)
library(moments)

mygen <- function(beta, rho)  {
  dt <- BlockModel.Gen(30, 300, beta, K =3, rho, simple = FALSE, power = TRUE)
  Adj = dt$A
  grp = dt$g
  mat = cbind(tot = rep(1, 300), deg1 = (grp ==1), deg2 = (grp ==2), deg3=(grp ==3))
  list(degree.mat =data.frame(grp,dt$theta, Adj%*%mat), prob.edge= unique(as.vector(dt$P)))
}

beta = .2
rho = .8
i = 1
data = matrix(rep(0,64*5), nrow = 64)

for (beta in (2:9)/10){
  for (rho in (2:9)/10){
    run = mygen(beta, rho)
    avg.deg.grp = data.frame(run$degree.mat)%>%group_by(grp)%>%
      summarise(cnt = n(), total= mean(tot), total.sk = skewness(tot) , deg.1 = mean(deg1), deg.2 = mean(deg2), deg.3 = mean(deg3))
    data[i,] = c(beta, rho, obs = mean(diag(as.matrix(avg.deg.grp[, 5:7]))/avg.deg.grp$cnt),true =run$prob.edge[1],avg.deg.grp$total.sk[1])
    i = i+1
  }
}

colnames(data)<- c("beta", "rho", "obs.edge.prob", "th.edge.prob", "degree.skew")
plot(obs.edge.prob~th.edge.prob, data = data)
abline(a = 0, b = 1)
plot(obs.edge.prob~beta, data = data)
abline(a = .1, b = 0)
plot(obs.edge.prob~rho, data = data)
abline(a = .1, b = 0)
plot(degree.skew~rho, data = data)
plot(degree.skew~beta, data = data)