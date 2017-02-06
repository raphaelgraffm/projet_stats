# On charge la base de donnes initiale
donnees  <- read.csv("donnees.csv", sep=";", stringsAsFactors=FALSE)

numcol <- ncol(donnees)
numrow <- nrow(donnees)

# On en fait une copie sur laquelle on va travailler
donnees1 <- donnees

# On supprime les colonnes comportant des donnees non remplies
invalid_cols = c()

for (col in 1:numcol) {

    num_absent_data <- 0

    for (row in 1:numrow) {
        if (donnees[row,col] == "?") {
            num_absent_data = num_absent_data + 1
        }
    }

    if (num_absent_data > 0) {
        invalid_cols = c(invalid_cols, col)
    }

}

donnees1 <- donnees1[,-invalid_cols]

# On supprime la colonne comportant des chaines de caractere
donnees1 = donnees1[!colnames(donnees1)=="communityname"]

# On sauvegarde la base de donnees traitee dans le fichier "donnees_traitees"
save(donnees1, file = "donnees_traitees")
