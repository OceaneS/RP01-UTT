rm(list=ls()) # On nettoie

VA <- function(apport, mensualite, tauxInflation) {
    tauxInflation <- (1 + tauxInflation)^(1/12) - 1 # On calcule le taux d'inflation pour chaque mois
    inflation <- sapply(1:length(tauxInflation), function(i) { # On calcule la suite des produits  de chaque 1+x pour voir l'influence des mois précédents
        tauxInflation <- 1 + tauxInflation
        resultat <- prod(tauxInflation[1:i])
        return(resultat)
    })
    resultat <- 100 - apport - sum(mensualite/inflation) # On utilise la formule du cours pour les valeurs actuelles
    return(resultat) # On retourne le résultat
}

tauxInflation <- c(rep(0, times=12), rep(0.002, times=12), rep(0.01, times=12)) # 2015 2016 2017

# On garde la voiture
valeurCible <- VA(20, rep(2.46, times=36), tauxInflation)
cat(paste("La valeur actuelle avec le cas 1 sans revente est de", valeurCible, "% du prix de la voiture.\n"))
cat(paste("La valeur actuelle avec le cas 2 avec achat est de", VA(14, c(rep(1.422, times=35),53), tauxInflation), "% du prix de la voiture.\n"))
mensualite <- uniroot(function(m) VA(14, c(rep(m, times=35),53/1.422*m), tauxInflation) - valeurCible, c(0,100), extendInt="yes")$root
cat(paste("Pour que le 2eme cas soit plus interessant, il faut que les mensualites soit inferieures a", mensualite, "%.\n"))

# C'est parti pour les plots !
plot(seq(0, 2, length=100), sapply(seq(0, 2, length=100), function(m) VA(14, c(rep(m, times=35),53/1.422*m), tauxInflation)), type="l", main="Valeur actuelle en fonction du pourcentage de la mensualite pour le cas 2 avec achat\n(on considere que la derniere mensualite est proportionnelle aux autres)", xlab="Pourcentage de la mensualite", ylab="Valeur actuelle")
abline(h=valeurCible, col="red", lty="dotted")
abline(v=mensualite, col="red", lty="dotted")

# On ne garde pas la voiture
valeurCible <- VA(20, c(rep(2.46,times=35),2.46 - as.numeric(readline(prompt="Quel est le pourcentage du prix de revente ?\n"))), tauxInflation)
cat(paste("La valeur actuelle avec le cas 1 avec revente est de", valeurCible, "% du prix de la voiture.\n"))
cat(paste("La valeur actuelle avec le cas 2 sans achat est de", VA(14, rep(1.422, times=35), tauxInflation[1:35]), "% du prix de la voiture.\n"))
mensualite <- uniroot(function(m) VA(14, rep(m, times=35), tauxInflation[1:35]) - valeurCible, c(0,100), extendInt="yes")$root
cat(paste("Pour que le 2eme cas soit plus interessant, il faut que les mensualites soit inferieures a", mensualite, "%.\n"))

dev.new() # On ouvre une nouvelle fenêtre
# C'est parti pour les plots !
plot(seq(0, 2, length=100), sapply(seq(0, 2, length=100), function(m) VA(14, rep(m, times=35), tauxInflation[1:35])), type="l", main="Valeur actuelle en fonction du pourcentage de la mensualite pour le cas 2 sans achat", xlab="Pourcentage de la mensualite", ylab="Valeur actuelle")
abline(h=valeurCible, col="red", lty="dotted")
abline(v=mensualite, col="red", lty="dotted")
