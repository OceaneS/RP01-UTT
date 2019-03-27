rm(list=ls())
n <- 1:1000
conf.level <- 0.95
u <- qnorm(1 - (1-conf.level)/2)
conf.int <- u*sqrt(1/6*(1-1/6)/n)
png("Intervalle de confiance en fonction de n.png")
plot(n, conf.int, type="l", main="Intervalle de confiance en fonction de n", xlab="Nombre de lancers", ylab="Longueur de l'intervalle")
invisible(dev.off())
n <- 100
conf.level <- 0.01*(1:99)
u <- qnorm(1 - (1-conf.level)/2)
conf.int <- u*sqrt(1/6*(1-1/6)/n)
png("Intervalle de confiance en fonction du niveau de confiance.png")
plot(conf.level, conf.int, type="l", main="Intervalle de confiance en fonction du niveau de confiance", xlab="Niveau de confiance", ylab="Longueur de l'intervalle")
invisible(dev.off())
n <- 100
conf.level <- 0.95
u <- qnorm(1 - (1-conf.level)/2)
p <- 0.01*(1:99)
conf.int <- u*sqrt(p*(1-p)/n)
png("Intervalle de confiance en fonction de la probabilite.png")
plot(p, conf.int, type="l", main="Intervalle de confiance en fonction de la probabilite", xlab="Probabilite", ylab="Longueur de l'intervalle")
invisible(dev.off())