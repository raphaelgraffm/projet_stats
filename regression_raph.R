load("donnees_traitees")
donnees = donnees1[!colnames(donnees1)=="communityname"]
numcol <- ncol(donnees)
numrow <- nrow(donnees)

pourcentage_test <- 0.3

seuil = floor( (1 - pourcentage_test) *numrow)

donnees_entrainement <- donnees[1:seuil,]
donnees_test <- donnees[seuil+1:numrow,]

# Modele 1 : toutes les variables, regression purement linéaire
modele1 <- "donnees_entrainement$ViolentCrimesPerPop~"
for (nom in names(donnees)[1:(numcol-2)]) {
    modele1 <- paste(modele1,"donnees_entrainement$",nom,"+",sep="")
}
modele1 <- paste(modele1,"donnees_entrainement$",names(donnees)[numcol-1],sep="")
#modele1

#result1 <- lm(modele1,data=donnees_entrainement)
#summary(result1)

# Modele 2 : uniquement les variables significatives du modèle précédent, toujours purement linéaire
col2 <- c(1,5,10,14,17,18,21,26,28,30,35,40,41,50,63,66,68,69,70,71,73,76,77,80,81,84,87,90,92,100,numcol)
donnees_entrainement_2 <- donnees_entrainement[col2]
donnees_test_2 <- donnees_test[col2]
numcol2 <- length(col2)

modele2 <- "donnees_entrainement$ViolentCrimesPerPop~"
for (nom in names(donnees_entrainement_2)[1:(numcol2-2)]) {
    modele2 <- paste(modele2,"donnees_entrainement$",nom,"+",sep="")
}
modele2 <- paste(modele2,"donnees_entrainement$",names(donnees_entrainement_2)[numcol2-1],sep="")

#result2 <- lm(modele2,data=donnees_entrainement_2)
#summary(result2)

# Modele 3 : nouveau tri des variables significatives
col3 <- c(1,2,3,4,6,7,12,13,14,18,19,20,21,22,25,26,27,28,29,numcol2)
donnees_entrainement_3 <- donnees_entrainement_2[col3]
donnees_test_3 <- donnees_test_2[col3]
numcol3 <- length(col3)

modele3 <- "donnees_entrainement$ViolentCrimesPerPop~"
for (nom in names(donnees_entrainement_3)[1:(numcol3-2)]) {
    modele3 <- paste(modele3,"donnees_entrainement$",nom,"+",sep="")
}
modele3 <- paste(modele3,"donnees_entrainement$",names(donnees_entrainement_3)[numcol3-1],sep="")

#result3 <- lm(modele3,data=donnees_entrainement_3)
#summary(result3)

# Modele 4 : prise en compte de termes quadratiques
load("donnees_new")
#summary(donnees_new)
donnees_new <- data.frame(donnees_new)
donnees_new_entrainement <- donnees_new[1:seuil,]
donnees_new_test <- donnees_new[(seuil+1):numrow,]
numcol_new <- ncol(donnees_new)
numrow_new <- nrow(donnees_new)

modele4 <- "donnees_new_entrainement$ViolentCrimesPerPop~"
for (nom in colnames(donnees_new)[1:(numcol_new-2)]) {
    modele4 <- paste(modele4,"donnees_new_entrainement$",nom,"+",sep="")
}
modele4 <- paste(modele4,"donnees_new_entrainement$",colnames(donnees_new)[numcol_new-1],sep="")
#print(modele4)
result4 <- lm(modele4,data=donnees_new_entrainement)
summary(result4)

# Test sur donnees Test
# for (row in 1:nrow(donnees_new_test)) {
#     estimation <- predict(result4, newdata=donnees_new_test[row,])
#     estimation
# }
#summary(donnees_new_test)
rownames(donnees_new_test) <- 1:nrow(donnees_new_test)
#rownames(donnees_new_test)
#donnees_new_test[1,1:(numcol_new-1)]
estimation <- predict(result4,newdata=donnees_new_test[1,1:(numcol_new-1)])
estimation
# donnees_new_test[3,numcol_new]
