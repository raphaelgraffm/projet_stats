load("donnees_traitees")

# Elimination des colonnes non quantitatives
donnees_acp = donnees1[!colnames(donnees1)=="communityname"]
numcol <- ncol(donnees_acp)
numrow <- nrow(donnees_acp)

#install.packages("explor", repos="https://cloud.r-project.org")
library(FactoMineR)
library(explor)

# ACP
out_acp = PCA( donnees_acp, ncp=numcol, quanti.sup=numcol, graph=F )
save(out_acp, file = "acp")
#explor(out_acp)

# Graphe des individus
#plot.PCA(out_acp, axes=c(1,2), choix="ind", habillage="none")
#plot.PCA(out_acp, axes=c(1,3), choix="ind", habillage="none")

# Cercle de correlation
#plot.PCA(out_acp, axes=c(1,2), choix="var", new.plot=TRUE, title="Cercle des correlations")
#plot.PCA(out_acp, axes=c(1,3), choix="var", title="Cercle des correlations")

# Dimensions
#print(out_acp$eig)
#barplot(out_acp$eig[, 2], main="Histogramme des valeurs propres", new.plot=TRUE,
#    names.arg=rownames(out_acp$eig), xlab= "Axes", ylab= "Pourcentage d’inertie",
#    cex.axis=0.8, font.lab=3, col= "orange")
#dimdesc(out_acp, axes=c(1,2,3))
