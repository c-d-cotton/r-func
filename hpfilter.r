#!/usr/bin/env Rscript
hpfilter=function(y,lambda=1600){
  L=length(y)
  hp=diag(rep(6),L)
  for(i in 1:(L-1)){
    hp[i,i+1]=-4
    hp[i+1,i]=-4
  }
  for(i in 1:(L-2)){
    hp[i,i+2]=1
    hp[i+2,i]=1
  }
  hp[1,1]=1
  hp[1,2]=-2
  hp[2,1]=-2
  hp[2,2]=5
  hp[L,L]=1
  hp[L,L-1]=-2
  hp[L-1,L]=-2
  hp[L-1,L-1]=5
  trend=(solve((diag(1,L)+lambda*hp))%*%y)
  return(trend)
}
