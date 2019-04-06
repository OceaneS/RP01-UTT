rm(list=ls()) # On retire les variables précédentes : on travail dans un espace de travail tout propre !

print("On garde la voiture ! ") # Ben quoi c'est vrai, si on peut ! Allez ! On le dit aut et fort !

# Premier cas : mensualités constantes et emprunt classique
print("Cas 1 :") # On annonce la couleur
# Les mensualités sont constantes, on utilisera donc la formule de la partie 2.2.1
apport <- 20 # Le pourcentage de l'apport
mensualite <- 2.46 # Le pourcentage de la mensualité
duree <- 36 # La durée de l'emprunt
# Calculons combien on paye au final
print(paste("On paye au final", apport+duree*mensualite, "% du prix de la voiture.")) # On ne stocke pas combien on paye dans une variable car on utilise ce résultat qu'une seule fois, pas la peine de s'en souvenir ! Paste permet juste de concaténer les chaines de caractères et print affiche une chaine de caractères
# Essayons maintenant de calculer le taux d'intérêt en utilisant la formule du 2.2.1
taux <- uniroot(function(i) (100-apport)*i/(1-(1+i)^(-duree))-mensualite, c(1e-18, 1), extendInt="yes")$root # On utilise uniroot pour résoudre notre équation modifiée pour devenir une recherche de racine. Le premier paramètre est une fonction que j'écris directement, car je n'ai pas envie de la stocker dans une variable, puisque je vais ne l'utiliser qu'une fois. Le second paramètre est la zone de recherche de la solution, je n'inclue pas zéro pour éviter une division par zéro et je mets 1 comme maximum pour être large. extendInt donne l'autorisation à R d'augmenter l'intervalle s'il ne lui convient pas. Pour finir, le $root signifie que suite au calcul nous ne voulons que la racine, R pouvant nous donner des informations en plus mais elle ne nous intéressent pas dans notre cas. Le $ permet de faire une sélection avec un nom de donnéé dans un objet en contenant plusieurs.
print(paste("Le TAEG est de", ((1+taux)^12-1)*100, "%.")) # Calculons maintenant le TAEG associé avec la formule du cours et affichons le !

rm(list=ls()) # On fait le ménage !

# Second cas : mensualités non fixes et location
print("Cas 2 :") # On est sympa on prévient de ce qu'il va suivre
# Les mensualités ne sont pas constantes, on utilise donc la formule du 2.2
apport <- 14 # Le pourcentage de l'apport
mensualite <- c(rep(1.422, times=35), 53) # Le pourcentage des différentes mensualités : les 35 premières à 1.422 et la dernière (qui fait mal) à 53
# Calculons combien on paye au final
print(paste("On paye au final", apport+sum(mensualite), "% du prix de la voiture.")) # On fait comme dans le cas 1 ! On additionne toues les mensualités contenues dans le vecteur avec la fonction sum !
# Utilisons la formule du 2.2 pour calculer le taux d'intérêt
taux <- uniroot(function(i) sum(mensualite/((1+i)^(1:length(mensualite))))-(100-apport), c(1e-18, 1), extendInt="yes")$root # On utilise notre connaissance de la fonction sum pour tout faire sur une ligne. 1:n génère un vecteur avec des valeurs allant de 1 à n par pas de 1, pile ce qu'il nous faut ! Il nous suffit plus qu'à donner la valeur de n, qui est le nombre de mensualités et donc le nombre de valeurs contenue dans le vecteur du même nom. Il suffit juste de récupérer la valeur en utilisant la fonction length, et le tour est joué !
print(paste("Le TAEG est de", ((1+taux)^12-1)*100, "%.")) # On montre comme avant notre bon vieux TAEG

rm(list=ls()) # J'aime bien nettoyer derière moi 😀

print("Negociation time !") # La seconde offre n'est pas assez alléchante : c'est l'heure de faire jouer nos talents de négociateur !
apport <- 14 # L'apport ne change pas : facile !
taux <- 0.005601616 # On cherche à obenir un meilleur taux que celui-ci !
# Utilisons nos capacités incroyables de résolution d'équation pour trouver les mensualités corresponantes !
mensualite <- uniroot(function(a) sum(c(rep(a, times=35), a*53/1.422)/((1+taux)^(1:36)))-(100-apport), c(0, 100), extendInt="yes")$root # On recycle la formule que l'on a déjà écrite, on a juste à générer notre vecteur mensualité que l'on avait précédemment (on considère une proportionnalité entre les deux valeurs de mensualité)
# C'est parti pour la négoce ! 👨‍💼
print(paste("La concurrence me fait un meilleur taux que vous, c'est simple : je veux des mensualites de moins de", mensualite, "% et", mensualite*53/1.422, "% car sinon je vais voir la concurrence.")) # Comme vous pouvez vous en douter j'ai jamais négocié dans tout ce qui est commercial 😅

rm(list=ls()) # 🧹🧹🧹

print("On ne garde plus la voiture ! ") # 😢

print("Cas 1 :")
apport <- 20 # Le pourcentage de l'apport
mensualite <- 2.46 # Le pourcentage de la mensualité
duree <- 36 # La durée de l'emprunt
# Calculons combien on paye au final
print(paste("On paye au final", apport+duree*mensualite-100, "% du prix de la voiture.")) # *insert wow meme*
print("Le TAEG n'a pas change !") # Et oui ! Ça ne change rien pour notre emprunt !


rm(list=ls()) # 🧹🧹🧹 encore...

print("Cas 2 :")

apport <- 14 # Le pourcentage de l'apport
mensualite <- rep(1.422, times=35) # Le pourcentage des différentes mensualités : les 35 premières à 1.422
print(paste("On paye au final", apport+sum(mensualite), "% du prix de la voiture.")) # On a l'habitude maintenant
apport <- apport+53 # Le pourcentage de l'apport + ce que l'on paye pas (ce qui n'est oas dans le crédit)
taux <- uniroot(function(i) sum(mensualite/((1+i)^(1:length(mensualite))))-(100-apport), c(1e-18, 1), extendInt="yes")$root # Un dernier uniroot pour la root (jeu de mot/20)
print(paste("Le TAEG est de", ((1+taux)^12-1)*100, "%.")) # Le retour du TAEG

rm(list=ls()) # 😴

print("Negociation time !") # 🤓
apport <- 14+53 # L'apport ne change pas...
taux <- 0.005601616 # On cherche à obenir un meilleur taux que celui-là...
# Utilisons nos capacités incroyables de résolution d'équation pour trouver les mensualités corresponantes !
mensualite <- uniroot(function(a) sum(rep(a, times=35)/((1+taux)^(1:35)))-(100-apport), c(0, 100), extendInt="yes")$root # ♻
# 👨‍💼
print(paste("La concurrence me fait un meilleur taux que vous, c'est simple : je veux des mensualites de moins de", mensualite, "% car sinon je vais voir la concurrence.")) # 😅