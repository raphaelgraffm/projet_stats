load("donnees_traitees")
donnees = donnees1[!colnames(donnees1)=="communityname"]
numcol <- ncol(donnees)
numrow <- nrow(donnees)

donnees_new <- matrix(ncol=3 * numcol-2, nrow = numrow)
noms <- c()
for (col in 1:(numcol-1)) {
    for (row in (1:numrow)) {
        donnees_new[row, 3*col-2] <- donnees[row,col]
        donnees_new[row, 3*col-1] <- donnees[row,col]^2
        donnees_new[row, 3*col] <- donnees[row,col]^3
    }
    nom_colonne <- names(donnees)[col]
    noms <- c(noms,paste(nom_colonne,3*col-2), paste(nom_colonne,"_2",sep=""), paste(nom_colonne,"_3",sep=""))
}
donnees_new[,3*numcol-2] <- donnees$ViolentCrimesPerPop
noms <- c(noms,"ViolentCrimesPerPop")
#noms
colnames(donnees_new) <- noms

save(donnees_new, file = "donnees_new")
