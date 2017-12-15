rm(list=ls())
library('mlbench')

pxKDE <- function(xi,x,r)
{
  N<-nrow(x)
  n<-ncol(x)
  K<-c()
  for(i in 1:N)
  {
    d<-t(xi-x[i,])%*%(xi-x[i,])
    K[i]<-exp(-d/(2*r^2))
  }
  p<-(1/(N*(sqrt(2*pi)*r)^n))*sum(K)
  return(p)
}

N<-500
spirals<-mlbench.spirals(N,cycles=1,sd=0.1)

ic1<-which(spirals$classes=='1')
ic2<-which(spirals$classes=='2')

x<-unlist(spirals[[1]])
plot(x[ic1,1],x[ic1,2],col='magenta',xlim=c(-2,2),ylim=c(-2,2),
     xlab='x1',ylab='x2')
par(new=T)
plot(x[ic2,1],x[ic2,2],col='blue',xlim=c(-2,2),ylim=c(-2,2),
     xlab='',ylab='',main='Distribuição dos dados')

y<-as.matrix(x[,1])
y[ic1,]<-1
y[ic2,]<--1

r<-1
J<-c()
h<-c()
h[1]<-r
for(j in 1:30)
{
  h[j+1]<-h[j]/1.2#1.06*sd(x)*N^(-1/5)
  
  pxc1<-c()
  pxc2<-c()
  
  for(i in 1:N)
  {
    pxc1[i]<-pxKDE(x[i,],x[ic1,],h[j])
    pxc2[i]<-pxKDE(x[i,],x[ic2,],h[j])
  }
  J[j]<-sum(c(sd(pxc1),sd(pxc2)))/sum(rbind(pxc1,pxc2))
}

r<-(h[which.max(J)])#1.06*sd(x)*N^(-1/5)

pxc1<-c()
pxc2<-c()

for(i in 1:N)
{
  pxc1[i]<-pxKDE(x[i,],x[ic1,],r)
  pxc2[i]<-pxKDE(x[i,],x[ic2,],r)
}
plot(pxc1[ic1],pxc2[ic1],col='red',xlim=c(0,2),ylim=c(0,2))
par(new=T)
plot(pxc1[ic2],pxc2[ic2],col='blue',xlim=c(0,2),ylim=c(0,2))

