library(readxl)
moscas_brancas <- read_excel("moscas brancas.xlsx")
require(car)
require(dae)

y<- c(1.22,	1.58,	1.58,	1.87,	
      2.12,	0.71,	2.35,	2.12,
      1.87,	2.12,	1.85,  1.58,
     	2.12,	4.30,	2.92,	3.08,	
      3.81,	3.54,	4.42,	2.74,
      4.60,	5.04,	  6.36,	  4.18)

n<- length(y); n
t<-6
r<-4
parcelas<- factor(c(1:n)); parcelas
trat<- factor(rep(c("A","B","C","D","E", "F"),time=c(r,r,r,r,r,r))); trat
DIC<- data.frame(parcelas,trat,y)
DIC

Var<- tapply(y,trat,var);Var
Media<- tapply(y,trat,mean);Media
Desvio_padrao<- sqrt(Var);Desvio_padrao

V<-data.frame(Var,Media,Desvio_padrao); V

moscas.aov      <- aov(y~trat)                
summary(moscas.aov)

residuos<-resid(moscas.aov)
plot(trat,residuos)
par(mfrow=c(2,2))
plot(moscas.aov)

