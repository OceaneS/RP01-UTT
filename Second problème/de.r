rm(list=ls())

# Simulation de lancers de dé

dice.faces <- 1:6 # Les différentes issues d'un lancer (les faces des celui-ci)
dice.fair.prob <- c(rep(1/length(dice.faces), times=length(dice.faces))) # Probabilités de tomber sur chaque face
sample.nbthrow <- 100 # Nombre de lancer de dé
sample.evolution.start <- 1 # A partir de combien de lancers cherche-t-on a visualiser l'évolution des proportions
test.conf.level <- 0.95 # Niveau de confiance

sample.data <- sample(dice.faces, sample.nbthrow, replace = TRUE, prob = dice.fair.prob) # On effectue les lancers

sample.factors <- table(sample.data) # On calcule l'effectif pour les lancers
sample.prop <- sample.factors/sample.nbthrow # On calcule les proportions observées
sample.diff <- abs(sample.prop - dice.fair.prob) # On calcule la valeur absolue de la différence entre la proportion observée et la probabilité théorique

# Affichage
print("Facteurs")
sample.factors
print("Proportions observees")
sample.prop
print("Difference entre valeurs observees et theoriques")
sample.diff


# Maintenant faisons comme si nous ne savions pas ces résultats et considérons ces lancers comme successifs
sample.evolution <- matrix(tabulate(sample.data[1:sample.evolution.start], nbins=length(dice.faces)), ncol=length(dice.faces))/sample.evolution.start # On crée une matrice de 6 colonnes contenant la proportion observée
for (index in c((sample.evolution.start + 1):sample.nbthrow)) {
   sample.evolution <- rbind(sample.evolution, tabulate(sample.data[1:index], nbins=length(dice.faces))/index) # On ajoute une ligne à la matrice donnant la proportion observée à cet "instant"
}

# Affichage
print("Evolution des probabilites pour chaque face")
sample.evolution

# Affichage graphique
png("Resultat.png", width=1280, height=720, units="px")
# On affiche l'évolution pour la plage de nombre de lancers voulu en rouge
par(col="red")
plot(sample.evolution.start:sample.nbthrow, sample.evolution[ ,1], type="l", main="Evolution des propotions", xlab="Nombre de lancers", ylab="Proportion")
for (index in 2:length(dice.faces)) {
   lines(sample.evolution.start:sample.nbthrow, sample.evolution[ ,index])
}
# On affiche les probabilitées en bleu
par(col="blue")
for (index in 1:length(dice.faces)) {
   lines(sample.evolution.start:sample.nbthrow, rep(dice.fair.prob[index], times=length(sample.evolution.start:sample.nbthrow)))
}
# Interval de confiance pour la face 1 par exemple
test.result.bound.lower <- c()
test.result.bound.upper <- c()
for (index in sample.evolution.start:sample.nbthrow) {
    test.result <- binom.test(x=sample.evolution[index, 1]*index ,n=index ,p=dice.fair.prob[1] ,conf.level=test.conf.level)$conf.int
    test.result.bound.lower <- append(test.result.bound.lower, test.result[1])
    test.result.bound.upper <- append(test.result.bound.upper, test.result[2])
}
par(col="black")
lines(sample.evolution.start:sample.nbthrow, test.result.bound.lower)
lines(sample.evolution.start:sample.nbthrow, test.result.bound.upper)

invisible(dev.off())