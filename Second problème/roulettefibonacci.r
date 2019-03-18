rm(list=ls()) # On nettoye l'environnement

# Fonction qui permet de jouer, prend en entrée une liste composée de la mise et d'un vecteur comportant les cases sur lesquelles on mise
jouer <- function(mise) {
    gain <- list("1"=35, "3"=11, "2"=17, "6"=5, "4"=8, "18"=1, "12"=2, "24"=0.5) # Les différents gain en fonction de sur combien de cases on mise
    resultat <- sample(0:36, 1) # On génère la case sur laquelle la bille s'arrête
    if (resultat %in% mise[[2]]) { # Si c'est une case sur laquelle on a misé
       return(mise[[1]] * (1 + gain[[paste(length(mise[[2]]))]])) # On récupère l'argent plus le gain
    } else { # Sinon
       return(0) # On perds de l'argent
    }
}

pair <- 2*(1:(36/2)) # On va miser sur les nombres pairs et j'ai la flemme de tout le temps retaper les cases, donc je les stocke
mise <- 1 # La mise de départ
argent <- 100 # L'argent que l'on posède initialement
dataVec <- c(argent) # Le vecteur qui garde l'historique de l'argent
fibonacci <- c(1) # On commence la suite de fibonacci

repeat{
   argent <- argent - mise # On dépose la mise
   gain <- jouer(list(mise, pair)) # On regarde combien on gagne
   argent <- argent + gain # On récupère l'argent que l'on gagne si on est chanceux
   dataVec <- c(dataVec, argent) # On garde l'argent pour le plotter après
   print(paste("On joue et on est au", length(fibonacci), "terme de la suite, le gain est de", gain, "et j'ai", argent))
   if (gain != 0) { # Je gagne
      # print(paste("J'arrete avec ", argent))
      if (length(fibonacci) > 2) {
         fibonacci <- fibonacci[1:(length(fibonacci)-2)]
      } else if (length(fibonacci) == 2) {
         fibonacci <- c(1)
      } else {
         break
      }
   } else { # Je perds
      mise <- fibonacci[length(fibonacci)]
      if (length(fibonacci) == 1) {
         fibonacci <- c(1, 1)
      } else {
         fibonacci <- c(fibonacci, fibonacci[length(fibonacci)-1]+fibonacci[length(fibonacci)])
      }
      # print(paste("J'ai ", argent, " et je compte miser ", mise))
   }
}
png("test.png") # On plot un png
plot(1:length(dataVec), dataVec, type="b", main="Plot de l'argent", xlab="Nombre de tours", ylab="Argent") # On dit ce que l'on plot et les différents titres et mise en page, "b" pour type signifie both donc lignes et points
dev.off() # On arrête de plotter