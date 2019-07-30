out=matrix(0,nrow=100,ncol=3)
for(i in 1:1){
  I=vac.NetworkSIR(my.gen(rates.by.grades$totStuD[1], rates.by.grades$DZCAMPUS[1]),tau=0.3,gamma=0.3,vac=t(rates.by.grades[1,17:30]))$I
  max=max(colSums(I))
  timemax=min((1:diim(I)[2])[colSums(I)==max])
  out[i,]=c(district,max,timemax)
}

##RUN SIMULATIONS!!!
