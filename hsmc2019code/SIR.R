library(randnet)
library(igraph)
library(tidyverse)
NetworkSIR=function(CM,tau,gamma){
  #generate SIR epidemic on a CM-network
  #CM = contact matrix
  #tau = probability of infection across an edge
  #gamma = probability of removal per time step
  N=dim(CM)[1]
  I=matrix(rep(0,N),nrow=N,ncol=1) #First infecteds
  S=matrix(rep(1,N),nrow=N,ncol=1) #First susceptibles
  R=matrix(rep(0,N),nrow=N,ncol=1) #First removed
  I1=sample(1:N, size=1) #Pick first random infected
  I[I1,1]=1
  S[I1,1]=0
  t=1
  while(sum(I[,t-1])>0 | t==1){
    t=t+1
    infneigh=CM%*%I[,t-1]
    pinf=1-(1-tau)^infneigh
    newI=rbinom(N, S[,t-1], pinf)
    newR=rbinom(N, I[,t-1], gamma)
    nextS=S[,t-1]-newI
    nextI=I[,t-1]+newI-newR
    nextR=R[,t-1]+newR
    I=cbind(I, nextI)
    S=cbind(S, nextS)
    R=cbind(R, nextR)
  }
  res=list(I=I,S=S,R=R)
  class(res)="netSIR"
  return(res)
}

summary.netSIR=function(x){
  t=dim(x$S)[2]
  S=apply(x$S,2,sum)
  I=apply(x$I,2,sum)
  R=apply(x$R,2,sum)
  res=data.frame(S=S,I=I,R=R)
  return(res)
}

plot.netSIR=function(x){
  y=summary(x)
  plot(y$S, type="b", xlab="time", ylab="")
  lines(y$I, type="b", col="red")
  lines(y$R, type="b", col="blue")
  legend("right", legend=c("S", "I", "R"),
         lty=c(1,1,1), pch=c(1,1,1),
         col=c("black", "red", "blue"))
}

dt <- BlockModel.Gen(30, 300, .2, K =3, .6, simple = FALSE, power = TRUE)
dis = NetworkSIR(dt$A, .3, .1)
summary(dis)
plot(dis)