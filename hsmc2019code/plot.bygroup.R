dt <- BlockModel.Gen(5, 10, .2, K =4, .6, simple = FALSE, power = TRUE)
dis = vac.NetworkSIR(dt$A, .3, .1,.2)
summary(dis)
plot(dis)

plot.bygrade=function(CM=BlockModel.Gen(2, 10, .2, K =3, .6, simple = FALSE, power = TRUE),vSIR=vac.NetworkSIR(CM$A, .3, .1,.2)){
  y=summary(vSIR)
  plot(y$R, type="b", col="blue", xlab="time", ylab="number of nodes")
  lines(y$I, type="b", col="red")
  lines(y$S, type="b", col="black")
  legend("right", legend=c("S", "I", "R"),lty=c(1,1,1), pch=c(1,1,1),col=c("black", "red", "blue"))
  
}


n=0
while(n<=max(g)){
  n=n+1
}
{
  group<-which(vSIR$g==n)
  specg<-dis$R[group,]

  
  
  
 
   plot((dt$g)==1)
}

###################

library(randnet)
library(igraph)
library(tidyverse)
library(epimdr)
vac.NetworkSIR=function(CM=BarabasiAlbert(N=300,K=2),tau=0.3,gamma=0.3,vac=0.5){
  #generate SIR epidemic on a CM-network
  #CM = contact matrix
  #tau = probability of infection across an edge
  #gamma = probability of removal (recovery) per time step
  #vac = percent of population vaccinated
  N=dim(CM)[1]
  I=matrix(rep(0,N),nrow=N,ncol=1) #First infecteds
  S=matrix(rep(1,N),nrow=N,ncol=1) #First susceptibles
  R=matrix(rep(0,N),nrow=N,ncol=1) #First recovered
  g=sample(4,N,rep=TRUE) #vector of group membership of length N; how do we know we won't just end up with one group of 1s? hmm...
  vacdata<-data.frame(g=1:4, grade=c(1,1,2,2),vac=c(.2,.2,.8,.8))
  pb<-left_join(data.frame(g),vacdata,by="g")
  (runif(N)<pb$vac)->vacnode #returns TRUE if vaccinated
  S[vacnode,1]<-0
  R[vacnode,1]<-1
  numvac=sum(R[,1])
  numsus=sum(S[,1])
  I1<-sample((1:N)[!vacnode],1)
  I[I1,1]=1
  S[I1,1]=0
  
  t=1
  while(sum(I[,t-1])>0 | t==1){ #run for first case or while you have in infected node from the last timestep
    t=t+1
    infneigh=CM%*%I[,t-1] #CM*infected nodes = how many infected nodes each node is connected to
    pinf=1-(1-tau)^infneigh #prob that a node will be infected by its neighbors
    newI=rbinom(N, S[,t-1], pinf) #probability of new nodes becoming infected
    newR=rbinom(N, I[,t-1], gamma) #prabability of new nodes becoming recovered (remember max(recovered)=sum(infected))
    nextS=S[,t-1]-newI #new susceptible population
    nextI=I[,t-1]+newI-newR
    nextR=R[,t-1]+newR
    I=cbind(I, nextI)
    S=cbind(S, nextS)
    R=cbind(R, nextR)
  }
  res=list(I=I,S=S,R=R)
  class(res)="netSIR"
  res
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
  plot(y$R, type="b", col="blue", xlab="time", ylab="number of nodes")
  lines(y$I, type="b", col="red")
  lines(y$S, type="b", col="black")
  legend("right", legend=c("S", "I", "R"),
         lty=c(1,1,1), pch=c(1,1,1),
         col=c("black", "red", "blue"))
}

dt <- BlockModel.Gen(5, 10, .2, K =4, .6, simple = FALSE, power = TRUE)
dis = vac.NetworkSIR(dt$A, .3, .1,.2)
summary(dis)
plot(dis)