#Le taux d'inflation est donné constant sur les 3 ans du remboursement du pret 
#PROBLEME 1

rm(list=ls())

#CAS 1
apport <- 0.20
C0 <- 1
mensualite <-0.0246
TI <- 0.018 #en 2018
TM <- ((1 + TI)^(1/12) - 1)
k<-1:36

VA1 <- C0*(1-apport)- sum(mensualite/(1+TM)^k)
print(paste("La valeur actuelle dans le cas 1 sans revente est de",VA1*100,"%"))

#CAS 2
apport <- 0.14
mensualite <- c(rep(0.01422, times=35),0.53)

VA2 <- C0*(1-apport) - sum(mensualite/ (1+TM)^k)
print(paste("La valeur actuelle dans le cas 2 avec achat est de",VA2*100,"%"))

mensualite <- uniroot(function(m) C0*(1-apport) - sum(c(rep(m, times=35),0.53)/ (1+TM)^k) - VA1, c(0,1), extendInt="yes")$root
print(paste("La mensualité optimale pour que VA2 = VA1 est de", mensualite*100,"%"))

mensualite <- uniroot(function(n) C0*(1-apport) - sum(c(rep(0.01422, times=35),n)/ (1+TM)^k) - VA1, c(0,1), extendInt="yes")$root
print(paste("Ou alors, le pourcentage final de l'achat de la voiture pour que VA2 = VA1 doit être de ", mensualite*100,"%"))
                      
rm(list=ls())


#PROBLEME 2
#CAS 1
apport <- 0.20
C0 <- 1
revente <-0.6
mensualite <-c(rep(0.0246,times=35),0.0246-revente)
TI <- 0.018
TM <- ((1 + TI)^(1/12) - 1)
k<-1:36


VA3 <- C0*(1-apport)- sum(mensualite/(1+TM)^k)
print(paste("La valeur actuelle dans le cas 1 avec revente est de",VA3*100,"%"))

#CAS 2
apport <- 0.14
mensualite <- c(rep(0.01422, times=35),0)

VA4 <- C0*(1-apport) - sum(mensualite/ (1+TM)^k)
print(paste("La valeur actuelle dans le cas 2 sans achat est de",VA4*100,"%"))

mensualite <- uniroot(function(m) C0*(1-apport) - sum(c(rep(m, times=35),0)/ (1+TM)^k) - VA3, c(0,1), extendInt="yes")$root
print(paste("La mensualité optimale pour que VA4 = VA3 est de", mensualite*100,"%"))
                  
rm(list=ls())
