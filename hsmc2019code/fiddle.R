#TESTING CHANGE IN BETA AND CHANGE IN RATIO
dt <- BlockModel.Gen(30,300,K=3,beta=0.2,rho=0.9,simple=FALSE,power=TRUE)
#ratio of intragroup/intergroup
sum(dt$A[dt$g==1,dt$g==1])/sum(dt$A[dt$g==1,dt$g==2|3])
#Generally close to 0.7
dt <- BlockModel.Gen(30,300,K=3,beta=0.9,rho=0.9,simple=FALSE,power=TRUE)
sum(dt$A[dt$g==1,dt$g==1])/sum(dt$A[dt$g==1,dt$g==2|3])
#Generally close to 0.35

degree<-dt$A%*%rep(1,300)
mean(degree)
hist(degree)

########

dt$A[dt$g==1, dt$g==2]%*%rep(1,sum(dt$g==2))
dt$A[dt$g==1,]%*%as.matrix(data.frame(tot=rep(1,300),deg1.1=(dt$g==1),deg1.2=(dt$g==2),deg1.3=(dt$g==3)))
dt$A[dt$g==1,]%*%as.matrix(data.frame(tot=rep(1,300),deg1.1=(dt$g==1),deg1.2=(dt$g==2),deg1.3=(dt$g==3)))->x
sum(x[,2])/sum(x[,c(3,4)])

sq<-function(x){
  x^2
}
##############
#figure out how to change group sizes to model the school sizes
#look at tot vs. theta: PLOT IT
#############
#what does this even do?----->
#for(i in 1:3) {
#  tmp<-sum(dt$A[dt$g==i,dt$g==i])/(2*sum(dt$A))+
#    sum(dt$A[dt$g==i,dt$g!=i])/(sum(dt$A))
#  print(tmp)
#}