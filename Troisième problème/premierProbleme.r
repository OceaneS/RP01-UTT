rm(list=ls())
# Cas 1
print("Cas 1 :")
apport <- 20
mensualite <- 2.46
duree <- 36
print(paste("Je paye au final", apport + mensualite * duree,"% du prix de la voiture."))
# Calcul du taux d'intérêt
taux <- uniroot(function(t) (100-apport)*t/(1-(1+t)^(-duree))-mensualite, c(1e-10, 1), extendInt="yes")$root
print(paste("Le taux du credit est de", taux*100, "%"))

rm(list=ls())
# Cas 2
print("Cas 2 :")
prix <- 14
mensualite <- 1.422
duree <- 35
achat <- 53
prix.final <- prix + mensualite * duree + achat
print(paste("Je paye au final", prix.final, "% du prix de la voiture."))
# Calcul du taux d'intérêt
taux <- uniroot(function(t) sum(c(prix,rep(mensualite, times=duree))/((1+t)^(1:(duree+1))))-(100-achat), c(1e-10, 1), extendInt="yes")$root
print(paste("Le taux du credit est de", taux*100, "%"))
# Quelle est l'influence des différents paramètres ?
png("Analyse rapide.png", width=3*480)
par(mfrow=c(1,3))
# Sans proportionnalité
taux.variation <- (0:5000)/100000
resultat <- sapply(taux.variation, function(t) {
    resultat <- uniroot(function(mensualite.nouvelle) sum(c(prix,rep(mensualite.nouvelle, times=duree))/((1+t)^(1:(duree+1))))-(100-achat), c(1e-10, 100), extendInt="yes")$root
    return(resultat)
})
plot(taux.variation, resultat, type="l", main="Influence du taux d'interet sur les mensualites\nsans proportionnalite pour la premiere mensualite", xlab="Taux d'interet", ylab="Mensualite", cex.lab=1.2)
abline(v=taux, col="red")
abline(h=mensualite, col="red")
# Avec proportionnalité
resultat <- sapply(taux.variation, function(t) {
    resultat <- uniroot(function(mensualite.nouvelle) sum(c(prix/mensualite*mensualite.nouvelle,rep(mensualite.nouvelle, times=duree))/((1+t)^(1:(duree+1))))-(100-achat), c(1e-10, 100), extendInt="yes")$root
    return(resultat)
})
plot(taux.variation, resultat, type="l", main="Influence du taux d'interet sur les mensualites\navec proportionnalite pour la premiere mensualite", xlab="Taux d'interet", ylab="Mensualite", cex.lab=1.2)
abline(v=taux, col="red")
abline(h=mensualite, col="red")
# Option d'achat
achat.variation <- 0:100
resultat <- prix + mensualite * duree + achat.variation
plot(achat.variation, resultat, type="l", main="Influence du pourcentage de l'option d'achat", xlab="Pourcentage de l'option d'achat", ylab="Pourcentage de l'argent depense par rapport au prix de la voiture", cex.lab=1.2)
abline(v=achat, col="red")
abline(h=prix.final, col="red")
invisible(dev.off())