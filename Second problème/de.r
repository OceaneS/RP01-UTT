rm(list=ls())

# Simulation de lancers de dé

dice.faces <- 1:6 # Les différentes issues d'un lancer (les faces des celui-ci)
dice.fair.prob <- c(rep(1/length(dice.faces), times=length(dice.faces))) # Probabilités de tomber sur chaque face
dice.rigged.prob <- c(1/12, 3/12, rep(1/6, times=4))
sample.nbthrow <- 10000 # Nombre de lancer de dé
sample.evolution.start <- 1 # A partir de combien de lancers cherche-t-on a visualiser l'évolution des proportions
test.conf.level <- 0.95 # Niveau de confiance

sample.data <- sample(dice.faces, sample.nbthrow, replace = TRUE, prob = dice.rigged.prob) # On effectue les lancers

sample.factors <- table(sample.data) # On calcule l'effectif pour les lancers
sample.prop <- sample.factors/sample.nbthrow # On calcule les proportions observées
sample.diff <- abs(sample.prop - dice.fair.prob) # On calcule la valeur absolue de la différence entre la proportion observée et la probabilité théorique

# Affichage
# print("Facteurs")
# sample.factors
# print("Proportions observees")
# sample.prop
# print("Difference entre valeurs observees et theoriques")
# sample.diff


# Maintenant faisons comme si nous ne savions pas ces résultats et considérons ces lancers comme successifs
sample.evolution <- matrix(ncol=length(dice.faces)) # On crée une matrice de 6 colonnes contenant la proportion observée
for (index in c(sample.evolution.start:sample.nbthrow)) {
   if (index == sample.evolution.start) {
      sample.evolution <- tabulate(sample.data[1:index], nbins=length(dice.faces))/index
   } else {
      sample.evolution <- rbind(sample.evolution, tabulate(sample.data[1:index], nbins=length(dice.faces))/index) # On ajoute une ligne à la matrice donnant la proportion observée à cet "instant"
   }
}

# Affichage
# print("Evolution des probabilites pour chaque face")
# sample.evolution

# Affichage graphique
png("Resultat.png", width=1280, height=720, units="px")
# On affiche l'évolution pour la plage de nombre de lancers voulu en rouge
par(col="red")
for (index in 1:length(dice.faces)) {
   if (index == 1) {
      plot(sample.evolution.start:sample.nbthrow, sample.evolution[ ,1], type="l", main="Evolution des propotions", xlab="Nombre de lancers", ylab="Proportion", xlim=c(sample.evolution.start, sample.nbthrow), ylim=c(0, 1))
   } else {
      lines(sample.evolution.start:sample.nbthrow, sample.evolution[ ,index])
   }
}
# On affiche les probabilitées en bleu
par(col="blue")
for (index in 1:length(dice.faces)) {
   lines(sample.evolution.start:sample.nbthrow, rep(dice.fair.prob[index], times=length(sample.evolution.start:sample.nbthrow)))
}
par(col="black")
for (dice.face in dice.faces){
   # Interval de confiance
   test.result.bound.lower <- c()
   test.result.bound.upper <- c()
   for (index in sample.evolution.start:sample.nbthrow) {
      test.result <- binom.test(x=sample.evolution[index - sample.evolution.start + 1, dice.face]*index ,n=index ,p=dice.fair.prob[dice.face] ,conf.level=test.conf.level)$conf.int
      test.result.bound.lower <- append(test.result.bound.lower, test.result[1])
      test.result.bound.upper <- append(test.result.bound.upper, test.result[2])
   }
   lines(sample.evolution.start:sample.nbthrow, test.result.bound.lower)
   lines(sample.evolution.start:sample.nbthrow, test.result.bound.upper)
}
invisible(dev.off())