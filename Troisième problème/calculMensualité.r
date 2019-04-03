credit <- function(C0, N, t) { # C0 est ce que l'on emprunte, N le nombre de mois du crédit et t le taux du crédit
    M <- (C0*t*(1+t)^N)/((1+t)^N-1) # On calcule les mensualités
    C <- C0 # On calcul maintenant l'argent que je rembourse à la banque
    I <- NULL # Et les intérêts !
    for (index in 1:N) { # Regardons ce que l'on donne comme argent à la banque mois par mois
        I <- c(I, t*C[length(C)]) # Les intérêts
        C <- c(C, C[length(C)] - (M - I[length(I)])) # Et ce que je dois "réellement" à la banque
    }
    return(list(mensualite=M, interet=I, remboursement=C))
}

testCredit <- credit(150000, 240, 0.004)
png("Test.png", width=1280, height=720, unit="px")
par(mfrow=c(2,1))
plot(1:240, testCredit$interet, type="l", ylim=c(0, testCredit$mensualite), col="red", main="Mensualite, interet et paye", xlab="Temps (en mois)", ylab="Argent (en euros)", xaxs="i", yaxs="i")
lines(1:240, testCredit$mensualite - testCredit$interet, col="blue")
abline(h=testCredit$mensualite, col="black", lty=2)
legend("right", legend=c("Mensualite", "Interet", "Paye"), col=c("black", "red", "blue"), lty=c(2,1,1))
plot(0:240, testCredit$remboursement, main="Reste a payer", type="l", xlab="Temps (en mois)", ylab="Argent", xaxs="i", yaxs="i")
invisible(dev.off())