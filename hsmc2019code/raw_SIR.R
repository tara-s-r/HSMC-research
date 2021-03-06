library(poweRlaw)
library(tidyverse)
library(randnet)
load("/Users/tararoshan/Desktop/GitHub/HSMC-research/hsmc2019code/vacRates.RData")
####   Create contact network for district, function my.gen
##  w gives different weights to the within grade contacts.  With pre-school having the most contacts
w = 3 - 1/13* (0:13)

### These functions create the base matrix of within group, within school ratios of degrees for each type of school
el.mat = function(){
  one = matrix(1, ncol = 7, nrow =7)/10
  diag(one)<- w[1:7]
  row.names(one)<- c("Pre", "K", 1:5)
  return(one)
}
ms.mat<- function(){
  one = matrix(1, ncol = 3, nrow =3)/10
  diag(one)<- w[8:10]
  row.names(one)<- c(6:8)
  return(one)
}
hs.mat <- function(){
  one = matrix(1, ncol = 4, nrow =4)/10
  diag(one)<- w[11:14]
  row.names(one)<- c(9:12)
  return(one)
}
ets.mat <- function(){
  one = matrix(1, ncol = 14, nrow =14)/10
  diag(one)<- w
  row.names(one)<- c("Pre", "K", 1:12)
  return(one)
}
k8.mat<- function(){
  one = matrix(1, ncol = 10, nrow =10)/10
  diag(one)<- w[1:10]
  row.names(one)<- c("Pre", "K", 1:8)
  return(one)
}
### combines the school matrices into one matrix for the district based on the number of schools 
isd.mat<- function(n.el, n.ms, n.hs){
  num.tot <- 7*n.el + 3*n.ms + 4*n.hs
  one = matrix(1, ncol = num.tot, nrow =num.tot)/20
  for(i in (1:n.el)){
    from = (i-1)*7 + 1
    to = (i-1)*7 + 7
    one[from:to, from:to] = el.mat() 
  }
  for(i in (1:n.ms)){
    from = 7*n.el +(i-1)*3 + 1
    to = 7*n.el +(i-1)*3 + 3
    one[from:to, from:to] = ms.mat() 
  }
  for(i in (1:n.hs)){
    from = 7*n.el +3*n.ms  + (i-1)*4 + 1
    to = 7*n.el +3*n.ms  + (i-1)*4 + 4
    one[from:to, from:to] = hs.mat() 
  }
  row.names(one)<- c(rep(c("Pre" , "K", 1:5), n.el), rep(6:8, n.ms), rep(9:12, n.hs))
  colnames(one)<- c(rep(c("Pre" , "K", 1:5), n.el), rep(6:8, n.ms), rep(9:12, n.hs))
  return(one)
}
### for special case for district with only two campuses, here it assumes the campuses are Pre-8 and HS.
isd2.mat<- function(){
  num.tot <- 14
  one = matrix(1, ncol = num.tot, nrow =num.tot)/20
  one[1:10, 1:10] = k8.mat()
  one[11:14, 11:14]= hs.mat()

  row.names(one)<- c("Pre", "K", 1:12)
  colnames(one)<- c("Pre", "K", 1:12)
  return(one)
}
###  creates the key to know which grade corresponds to which group. 
grp.to.grd <- function(n.el, n.ms, n.hs){
  n.grp =   n.grp = 7*n.el + 3*n.ms + 4*n.hs
  data.frame(g = 1:n.grp, grade = c(rep(-1:5, n.el), rep(6:8, n.ms), rep(9:12, n.hs)))
}

### uses functions above to simulate a contact matrix based on the number of students in the district and
### number of campuses. 
##  alpha is the power for the power law, lambda is the average degree for each node. 
my.gen<- function(n, n.campus, lambda = 20, alpha = 5){
  
  if(n.campus>3){
    n.hs = round(n.campus/5.3)
    n.ms = round(n.campus*1.3/5.3)
    n.el = round(n.campus*3/5.3)
    n.grp = 7*n.el + 3*n.ms + 4*n.hs
    P0 = isd.mat(n.el, n.ms , n.hs)
    Pi = 1/(14*c(rep(n.el, 7*n.el), rep(n.ms, 3*n.ms), rep(n.hs, 4*n.hs)))
    g.mini = grp.to.grd(n.el, n.ms, n.hs)}
  else{
    g.mini = data.frame(g=1:14, grade = -1:12)
    n.grp = 14
    Pi = rep(1, 14)/14
    if(n.campus == 1){
      P0 = ets.mat() 
    } else if (n.campus == 2){
      P0 = isd2.mat()
    } else if(n.campus == 3){
      n.hs = 1
      n.ms=1
      n.el = 1
      P0=isd.mat(1,1,1)
    }
  }
  
  M <- matrix(0, n, n.grp)
  
  membership <- sample(x = n.grp, size = n, replace = TRUE, prob = Pi)
  
  M[cbind(1:n, membership)] <- 1
  # LL <- diag(sqrt(lambda))
  A.bar <- M %*% P0%*% t(M)
  
  
  degree.seed <- rplcon(300, 1, alpha)
  
  node.degree <- sample(degree.seed, size = n,
                        replace = TRUE)
  
  DD <- diag(node.degree)
  A.bar <- (DD %*% A.bar %*% DD)
  A.bar <- lambda*A.bar/ mean(colSums(A.bar))
  
  
  upper.index <- which(upper.tri(A.bar))
  upper.p <- A.bar[upper.index]
  upper.u <- runif(n = length(upper.p))
  upper.A <- rep(0, length(upper.p))
  upper.A[upper.u < upper.p] <- 1
  A <- matrix(0, n, n)
  A[upper.index] <- upper.A
  A <- A + t(A)
  return(list(A = A, g = membership, P = A.bar, theta = node.degree, grade.key = g.mini))
}


####  Given the network, simulate the outbreak by infecting a random node with vac.NetworkSIR
vac.NetworkSIR=function(CM,tau=0.15,gamma=0.01,vac=.5){
  N=dim(CM)[1]
  I=matrix(rep(0,N),nrow=N,ncol=1) #First infecteds
  S=matrix(rep(1,N),nrow=N,ncol=1) #First susceptibles
  R=matrix(rep(0,N),nrow=N,ncol=1) #First recovered
  (runif(N)<vac)->vacnode #returns TRUE if vaccinated
  if(sum(vacnode)==N){
    res = list(I = matrix(0, nrow= N, ncol =1), S = matrix(0, nrow= N, ncol =1), R = matrix(0, nrow= N, ncol =1))
    class(res)="netSIR"
    return(res)}else{
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
      res}
}

#######   Use the code to simulate outbreak for sample of schools
small = rates.by.grades[rates.by.grades$totStuD < 1000, ]
out=matrix(0,nrow=100,ncol=4)
out.many = out.many
isd.samp = sample(1:dim(small)[1], 100)
for(j in (1:5)){
  k = isd.samp[j]
  isd = small$Fac.Id[k]
  n.camp=small$DZCAMPUS[k]
  n=small$totStuD[k]
  res=my.gen(n,n.camp)
  CM=res$A
  df2<-left_join(res$grade.key,data.frame(grade=(-1:12),v=t(small[1,17:30])),by="grade")
  df3<-left_join(data.frame(g=res$g),df2, by="g")
  vac=df3$X1

  colnames(out)<- c("District", "n.inf", "time.max", "prop.inf")
  for(i in 1:100){
    I=vac.NetworkSIR(CM,tau=0.15,gamma=0.01,vac)$I
    max=max(colSums(I))
    timemax=min((1:dim(I)[2])[colSums(I)==max])
    out[i,]=c(isd,max,timemax,as.numeric(max/rates.by.grades$totStuD[j]))
  }
  if(j==1){
    out.many = out
  }else {
    out.many = rbind(out.many, out)
  }
}