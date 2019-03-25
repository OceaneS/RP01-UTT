rm(list=ls())

# Simulation de lancers de dé

dice.faces <- 1:6 # Les différentes issues d'un lancer (les faces des celui-ci)
dice.fair.prob <- c(rep(1/length(dice.faces), times=length(dice.faces))) # Probabilités de tomber sur chaque face
dice.rigged.prob <- c(1/12, 3/12, rep(1/6, times=4))
sample.nbthrow <- 100 # Nombre de lancer de dé
sample.evolution.start <- 1 # A partir de combien de lancers cherche-t-on a visualiser l'évolution des proportions
test.conf.level <- 0.95 # Niveau de confiance

print("Lance de de")

sample.data <- sample(dice.faces, sample.nbthrow, replace = TRUE, prob = dice.rigged.prob) # On effectue les lancers

# Affichage
# print("Facteurs")
# sample.factors
# print("Proportions observees")
# sample.prop
# print("Difference entre valeurs observees et theoriques")
# sample.diff


# Maintenant faisons comme si nous ne savions pas ces résultats et considérons ces lancers comme successifs

print("Calcul de l'evolution des proportions")

sample.evolution <- matrix(ncol=length(dice.faces)) # On crée une matrice de 6 colonnes contenant la proportion observée
for (index in c(sample.evolution.start:sample.nbthrow)) {
   if (index == sample.evolution.start) {
      sample.evolution <- tabulate(sample.data[1:index], nbins=length(dice.faces))/index
   } else {
      sample.evolution <- rbind(sample.evolution, tabulate(sample.data[1:index], nbins=length(dice.faces))/index) # On ajoute une ligne é la matrice donnant la proportion observée é cet "instant"
   }
}

# Affichage
# print("Evolution des probabilites pour chaque face")
# sample.evolution

# Affichage graphique
png("Result.png", width=3840, height=2160, units="px")
par(mfrow=c(2,3))
for (index in 1:length(dice.faces)) {
	print(paste("Affichage du graphique pour la face", index))
	# On affiche l'évolution pour la plage de nombre de lancers voulu en rouge
	par(col="red")
	plot(sample.evolution.start:sample.nbthrow, sample.evolution[ ,index], type="l", main="Evolution des propotions", xlab="Nombre de lancers", ylab="Proportion", xlim=c(sample.evolution.start, sample.nbthrow), ylim=c(0, 0.6))
 	

	# On affiche les probabilitées en bleu
	par(col="blue")
	abline(h=dice.fair.prob[index])

	print(paste("Calcul des intervalles de confiance pour la face", index))

	# On affiche les intervalles de confiance en noir
	par(col="black")
   	# Intervalles de confiance
   	test.result.bound.lower <- c()
   	test.result.bound.upper <- c()
   	for (index2 in sample.evolution.start:sample.nbthrow) {
		test.result <- binom.test(x=sample.evolution[index2 - sample.evolution.start + 1, index]*index2 ,n=index2 ,p=dice.fair.prob[index] ,conf.level=test.conf.level)$conf.int
		test.result.bound.lower <- append(test.result.bound.lower, test.result[1])
		test.result.bound.upper <- append(test.result.bound.upper, test.result[2])
	}
   	lines(sample.evolution.start:sample.nbthrow, test.result.bound.lower)
   	lines(sample.evolution.start:sample.nbthrow, test.result.bound.upper)
}
invisible(dev.off())
print("C'est fini !")