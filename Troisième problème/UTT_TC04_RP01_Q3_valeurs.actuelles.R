#PROBLEME 1

rm(list=ls())

#CAS 1

apport <- 0.20
C0 <- 1
mensualite <-0.0246
TI2018 <- 0.018
TM <- ((1 + TI2018)^(1/12) - 1)
k<-1:36

VA1 <- C0*(1-apport)- sum(mensualite/(1+TM)^k)

print(paste("La valeur actuelle dans le cas 1 sans revente est de",VA1*100,"%"))

rm(list=ls())

#CAS 2


apport <- 0.14
C0<-1
mensualite <- c(rep(0.01422, times=35),0.53)
k<-1:36
TI2018<-0.018
TM <- ((1 + TI2018)^(1/12) - 1)

VA2 <- C0*(1-apport) - sum(mensualite/ (1+TM)^k)

print(paste("La valeur actuelle dans le cas 2 avec achat est de",VA2*100,"%"))

rm(list=ls())


#PROBLEME 2
rm(list=ls())

#CAS 1

apport <- 0.20
C0 <- 1
revente <-0.6
mensualite <-c(rep(0.0246,times=35),0.0246-revente)
TI2018 <- 0.018
TM <- ((1 + TI2018)^(1/12) - 1)
k<-1:36


VA3 <- C0*(1-apport)- sum(mensualite/(1+TM)^k)

print(paste("La valeur actuelle dans le cas 1 avec revente est de",VA3*100,"%"))

rm(list=ls())

#CAS 2


apport <- 0.14
C0<-1
mensualite <- c(rep(0.01422, times=35),0)
k<-1:36
TI2018<-0.018
TM <- ((1 + TI2018)^(1/12) - 1)

VA4 <- C0*(1-apport) - sum(mensualite/ (1+TM)^k)

print(paste("La valeur actuelle dans le cas 2 sans achat est de",VA4*100,"%"))

rm(list=ls())
