donnees  <- read.csv("donnees.csv", sep=";", stringsAsFactors=FALSE)
#summary(donnees)
numcol <- ncol(donnees)
numrow <- nrow(donnees)
#print(numcol)
#print(numrow)
donnees1 <- donnees
#summary(donnees1)

# Suppression des colonnes pour lesquelles on a peu d'informations
# Et replissage par la moyenne des donnees manquantes
invalid_cols = c()
incomplete_cols = c()
for (col in 1:numcol) {

    num_valid_data <- 0
    num_absent_data <- 0

    for (row in 1:numrow) {
        if (donnees[row,col] == "?") {
            num_absent_data = num_absent_data + 1
        }
        else {
            num_valid_data = num_valid_data + 1
        }
    }

    #if (num_absent_data > num_valid_data) {
    if (num_absent_data > 0) {
        invalid_cols = c(invalid_cols, col)
    }
    #else if (num_absent_data > 0) {
    #    incomplete_cols = c(incomplete_cols, col)
    #}
}

#for (col in incomplete_cols) {
#    somme <- 0
#    for (row in 1:numrow) {
#        if (donnees[row,col] != "?") {
#            somme <- somme + as.numeric(as.character(donnees[row,col]))
#        }
#    }
#    moyenne <- somme / numrow
#    for (row in 1:numrow) {
#        if (donnees[row,col] == "?") {
#            donnees1[row,col] <- moyenne
#        }
#    }
#}

donnees1 <- donnees1[,-invalid_cols]
#save(donnees1, file = "donnees_traitees")


#summary(donnees1)
