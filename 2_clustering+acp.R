library(FactoMineR)
library(explor) # Package de visualisation d'une ACP

# Chargement et traitement de la base de donnees

load("donnees_traitees")

donnees_acp <- donnees1
donnees_clustering = donnees_acp[!colnames(donnees_acp)=="ViolentCrimesPerPop"]
numcol <- ncol(donnees_clustering)
numrow <- nrow(donnees_clustering)

# Determination du nombre de centres a conserver

kmax=10 # Nombre maximum de centres a tester
part_inertie_expl = rep(0, times=kmax) # Vecteur des parts d'inertie expliquees

for (k in 2:kmax) {
    cluster_kmeans = kmeans(donnees_clustering, centers = k, nstart = 20)
    part_inertie_expl[k] = cluster_kmeans$betweenss / cluster_kmeans$totss
}

#plot(part_inertie_expl, type="h", lwd=40, lend="butt", xlim=c(2,kmax),
#        xaxp=c(2,kmax,kmax-2), xlab="Nombre de clusters", ylab="Part d'inertie expliquee")

# On choisit de separer les donnees en trois clusters
cluster_kmeans_3cl = kmeans(donnees_clustering, centers = 3, nstart = 200)

# On rajoute les clusters a notre base de donnees
donnees_acp$Groupe = as.factor(t(cluster_kmeans_3cl$cluster))

# On affiche les clusters de quelques grandes villes americaines
donnees_acp$Groupe[565] # Los Angeles
donnees_acp$Groupe[1045] # Washington
donnees_acp$Groupe[1135] # Miami
donnees_acp$Groupe[1955] # San Diego

# On effectue une ACP, en indiquant les colonnes ViolentCrimesPerCap et Groupe comme supplementaires
out_acp = PCA(donnees_acp, scale.unit=T, ncp=numcol, quanti.sup=numcol+1,quali.sup=numcol+2, graph=FALSE)

# On peut tracer la projection des individus dans les premiers plans factoriels

# plot.PCA(out_acp,shadow=TRUE,cex=2,axes=c(1,2),choix="ind",habillage=numcol+2,label="none",new.plot=TRUE,
# title="")
# plot.PCA(out_acp,shadow=TRUE,cex=2,axes=c(1,3),choix="ind",habillage=numcol+2,label="none",
# title="")
# plot.PCA(out_acp,shadow=TRUE,cex=2,axes=c(2,3),choix="ind",habillage=numcol+2,label="none",
# title="")

# On explore le resultat dans le navigateur internet avec le package explor
explor(out_acp)
