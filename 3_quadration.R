# Chargement et traitement de la base de donnees

load("donnees_traitees")

donnees <- donnees1
numcol <- ncol(donnees)
numrow <- nrow(donnees)

# On cree une nouvelle base de donnees
donnees_new <- matrix(ncol=3 * numcol-2, nrow = numrow)

# On stocke les noms des differentes colonnes que l'on cree
noms <- c()

# On remplit le tableau avec les colonnes, leurs carres et leurs cubes
for (col in 1:(numcol-1)) {

    for (row in (1:numrow)) {
        donnees_new[row, 3*col-2] <- donnees[row,col]
        donnees_new[row, 3*col-1] <- donnees[row,col]^2
        donnees_new[row, 3*col] <- donnees[row,col]^3
    }

    # On adopte la convention de nommage suivante :
    # Les colonnes de base sont renommees nom_colonne.numero_colonne (pour faciliter le comptage)
    # Les noms des colonnes "au carre" est : nom_colonne_2
    # Les noms des colonnes "au cube" est : nom_colonne_3
    nom_colonne <- names(donnees)[col]
    noms <- c(noms,paste(nom_colonne,3*col-2), paste(nom_colonne,"_2",sep=""), paste(nom_colonne,"_3",sep=""))
}

# On rajoute la colonne ViolentCrimesPerPop, qui elle n'est pas elevee a des puissances superieures
donnees_new[,3*numcol-2] <- donnees$ViolentCrimesPerPop
noms <- c(noms,"ViolentCrimesPerPop")

# On indique le nom des colonnes
colnames(donnees_new) <- noms
donnees_new <- data.frame(donnees_new)

# On sauvegarde la nouvelle base dans le fichier "donnees_new"
save(donnees_new, file = "donnees_new")
