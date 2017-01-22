#setwd("C:/Users/Kodjo/Desktop/Cours Statistiques/Projet STNUM/Projet_stats")
load("donnees_traitees")
donnees = donnees1[!colnames(donnees1)=="communityname"]
numcol <- ncol(donnees)
numrow <- nrow(donnees)

pourcentage_test <- 0.3

seuil = floor( (1 - pourcentage_test) *numrow)

donnees_entrainement <- donnees[1:seuil,]
donnees_test <- donnees[(seuil+1):numrow,]

# Modele 1 : toutes les variables, regression purement linéaire
modele1 <- "donnees_entrainement$ViolentCrimesPerPop~"
for (nom in names(donnees)[1:(numcol-2)]) {
    modele1 <- paste(modele1,"donnees_entrainement$",nom,"+",sep="")
}
modele1 <- paste(modele1,"donnees_entrainement$",names(donnees)[numcol-1],sep="")
#modele1

result1 <- lm(modele1,data=donnees_entrainement)
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

result3 <- lm(modele3,data=donnees_entrainement_3)
#summary(result3)

#Detection de colinearite
#vif(result3)
# Trace du graph entre les deux variables que le test permet de faire ressortir
#plot(donnees_entrainement$RentLowQ,donnees_entrainement$MedRent)
#Conclu : il faudrait supprimer une des variables
#plot(donnees_entrainement$pctWInvInc,donnees_entrainement$MalePctNevMarr)

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
#summary(result4)

# Modele 5 : prise en compte de termes quadratiques significatifs
col5 <- c(4,5,6,15,16,17,18,20,21,35,36,43,48,52,64,65,90,113,114,136,137,138,139,140,141,142,143,155,156,163,164,165,166,167,168,222,227,228,253,254,255,259,260,261,265,274,275,276,numcol_new)
donnees_entrainement_5 <- donnees_new_entrainement[col5]
donnees_test_5 <- donnees_new_test[col5]
numcol5 <- length(col5)

modele5 <- "donnees_entrainement_5$ViolentCrimesPerPop~"
for (nom in names(donnees_entrainement_5)[1:(numcol5-2)]) {
    modele5 <- paste(modele5,"donnees_entrainement_5$",nom,"+",sep="")
}
modele5 <- paste(modele5,"donnees_entrainement_5$",names(donnees_entrainement_5)[numcol5-1],sep="")

result5 <- lm(modele5,data=donnees_entrainement_5)
#summary(result5)

# Modele 6 : prise en compte de termes quadratiques ENCORE PLUS significatifs
col6 <- c(4,5,6,7,12,13,14,17,20,21,23,24,25,numcol5)
donnees_entrainement_6 <- donnees_entrainement_5[col6]
donnees_test_6 <- donnees_test_5[col6]
numcol6 <- length(col6)

modele6 <- "donnees_entrainement_6$ViolentCrimesPerPop~"
for (nom in names(donnees_entrainement_6)[1:(numcol6-2)]) {
    modele6 <- paste(modele6,"donnees_entrainement_6$",nom,"+",sep="")
}
modele6 <- paste(modele6,"donnees_entrainement_6$",names(donnees_entrainement_6)[numcol6-1],sep="")

result6 <- lm(modele6,data=donnees_entrainement_6)
#summary(result6)

# Test du modele 1

# Test sur donnees d'entrainement
result_estim_entrainement_1 <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result1$coefficients
coef[is.na(coef)] <- 0
donnees_entrainement[is.na(donnees_entrainement)] <- 0

numrow_entrainement <- nrow(donnees_entrainement)
numcol_entrainement <- ncol(donnees_entrainement)

for (k in numrow_entrainement:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_entrainement[k,1:(numcol_entrainement-1)]
  for ( i in 1:(numcol_entrainement-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_entrainement[k,numcol_entrainement]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim_entrainement_1 <- rbind(r,result_estim_entrainement_1)
 }

# result_estim_entrainement_1

cat( sprintf("Erreur moyenne sur les donnees d'entrainement pour le modele 1 : %.2f\n", colMeans(result_estim_entrainement_1)[3] ) )

# Test sur donnees de test

result_estim_1 <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result1$coefficients
coef[is.na(coef)] <- 0
donnees_test[is.na(donnees_test)] <- 0

numrow_test <- nrow(donnees_test)
numcol_test <- ncol(donnees_test)

for (k in numrow_test:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_test[k,1:(numcol_test-1)]
  for ( i in 1:(numcol_test-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_test[k,numcol_test]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim_1 <- rbind(r,result_estim_1)
 }

# result_estim_1

cat( sprintf("Erreur moyenne sur les donnees de test pour le modele 1 : %.2f\n\n", colMeans(result_estim_1)[3] ) )

# Test du modele 4

# Test sur donnees d'entrainement
result_estim_entrainement <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result4$coefficients
coef[is.na(coef)] <- 0
donnees_new_entrainement[is.na(donnees_new_entrainement)] <- 0

numrow_entrainement <- nrow(donnees_new_entrainement)
numcol_entrainement <- ncol(donnees_new_entrainement)

for (k in numrow_entrainement:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_new_entrainement[k,1:(numcol_entrainement-1)]
  for ( i in 1:(numcol_entrainement-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_new_entrainement[k,numcol_entrainement]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim_entrainement <- rbind(r,result_estim_entrainement)
 }

# result_estim_entrainement

cat( sprintf("Erreur moyenne sur les donnees d'entrainement pour le modele 4 : %.2f\n", colMeans(result_estim_entrainement)[3] ) )

# Test sur donnees de test

result_estim <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result4$coefficients
coef[is.na(coef)] <- 0
donnees_new_test[is.na(donnees_new_test)] <- 0

numrow_test <- nrow(donnees_new_test)
numcol_test <- ncol(donnees_new_test)

for (k in numrow_test:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_new_test[k,1:(numcol_test-1)]
  for ( i in 1:(numcol_test-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_new_test[k,numcol_test]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim <- rbind(r,result_estim)
 }

# result_estim

cat( sprintf("Erreur moyenne sur les donnees de test pour le modele 4 : %.2f\n\n", colMeans(result_estim)[3] ) )

# Test du modele 3

# Test sur donnees d'entrainement
result_estim_entrainement_3 <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result3$coefficients
coef[is.na(coef)] <- 0
donnees_entrainement_3[is.na(donnees_entrainement_3)] <- 0

numrow_entrainement_3 <- nrow(donnees_entrainement_3)
numcol_entrainement_3 <- ncol(donnees_entrainement_3)

for (k in numrow_entrainement_3:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_entrainement_3[k,1:(numcol_entrainement_3-1)]
  for ( i in 1:(numcol_entrainement_3-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_entrainement_3[k,numcol_entrainement_3]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim_entrainement_3 <- rbind(r,result_estim_entrainement_3)
 }

# result_estim_entrainement_3

cat( sprintf("Erreur moyenne sur les donnees d'entrainement pour le modele 3 : %.2f\n", colMeans(result_estim_entrainement_3)[3] ) )

# Test sur donnees de test

result_estim_3 <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result3$coefficients
coef[is.na(coef)] <- 0
donnees_test_3[is.na(donnees_test_3)] <- 0

numrow_test_3 <- nrow(donnees_test_3)
numcol_test_3 <- ncol(donnees_test_3)

for (k in numrow_test_3:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_test_3[k,1:(numcol_test_3-1)]
  for ( i in 1:(numcol_test_3-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_test_3[k,numcol_test_3]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim_3 <- rbind(r,result_estim_3)
 }

# result_estim_3

cat( sprintf("Erreur moyenne sur les donnees de test pour le modele 3 : %.2f\n\n", colMeans(result_estim_3)[3] ) )

# Test du modele 6

# Test sur donnees d'entrainement
result_estim_entrainement_6 <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result6$coefficients
coef[is.na(coef)] <- 0
donnees_entrainement_6[is.na(donnees_entrainement_6)] <- 0

numrow_entrainement_6 <- nrow(donnees_entrainement_6)
numcol_entrainement_6 <- ncol(donnees_entrainement_6)

for (k in numrow_entrainement_6:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_entrainement_6[k,1:(numcol_entrainement_6-1)]
  for ( i in 1:(numcol_entrainement_6-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_entrainement_6[k,numcol_entrainement_6]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim_entrainement_6 <- rbind(r,result_estim_entrainement_6)
 }

# result_estim_entrainement_6

cat( sprintf("Erreur moyenne sur les donnees d'entrainement pour le modele 6 : %.2f\n", colMeans(result_estim_entrainement_6)[3] ) )

# Test sur donnees de test

result_estim_6 <- data.frame(Estimation=c(),ValeurReelle=c(),Ecart=c())

coef <- result6$coefficients
coef[is.na(coef)] <- 0
donnees_test_6[is.na(donnees_test_6)] <- 0

numrow_test_6 <- nrow(donnees_test_6)
numcol_test_6 <- ncol(donnees_test_6)

for (k in numrow_test_6:1) {
  estimate_value <- as.numeric(coef[1])
  record <- donnees_test_6[k,1:(numcol_test_6-1)]
  for ( i in 1:(numcol_test_6-1) ) {
    estimate_value <- estimate_value + as.numeric(coef[i+1]) * as.numeric(record[i])
  }
  real_value <- donnees_test_6[k,numcol_test_6]
  ecart <- abs(real_value - estimate_value)
  r <- data.frame(Estimation=c(estimate_value),ValeurReelle=c(real_value),Ecart=c(ecart))
  result_estim_6 <- rbind(r,result_estim_6)
 }

# result_estim_6

cat( sprintf("Erreur moyenne sur les donnees de test pour le modele 6 : %.2f\n\n", colMeans(result_estim_6)[3] ) )
