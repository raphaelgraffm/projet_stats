load("donnees_traitees")

donnees_acp = donnees1[!colnames(donnees1)=="communityname"]
donnees_clustering = donnees_acp[!colnames(donnees_acp)=="ViolentCrimesPerPop"]
numcol <- ncol(donnees_clustering)
numrow <- nrow(donnees_clustering)

library(FactoMineR)
library(explor)

# Test de la variance expliquee
kmax=10
part_inertie_expl=rep(0,times=kmax)

for (k in 2:kmax) {
    cluster_kmeans=kmeans(donnees_clustering,centers=k,nstart=20)
    part_inertie_expl[k]=cluster_kmeans$betweenss/cluster_kmeans$totss
}

#plot(part_inertie_expl, type="h", lwd=40, lend="butt", xlim=c(2,kmax),
#        xaxp=c(2,kmax,kmax-2), xlab="Nombre de clusters", ylab="Part d inertie expliquee",
#        main="Part d inertie expliquee en fonction du nombre de classes")

cluster_kmeans_3cl = kmeans(donnees_clustering,centers=3,nstart=200)
# cluster_kmeans_2cl$cluster
donnees_acp$Groupe = as.factor(t(cluster_kmeans_3cl$cluster))

#summary(donnees_clustering)

out_acp = PCA(donnees_acp, scale.unit=T, ncp=numcol, quanti.sup=numcol+1,quali.sup=numcol+2, graph=FALSE)
explor(out_acp)
# plot.PCA(out_acp,shadow=TRUE,cex=2,axes=c(1,2),choix="ind",habillage=numcol+1,label="none",new.plot=TRUE,
#         title="")
# plot.PCA(out_acp,shadow=TRUE,cex=2,axes=c(1,3),choix="ind",habillage=numcol+1,label="none",
#         title="")
# plot.PCA(out_acp,shadow=TRUE,cex=2,axes=c(2,3),choix="ind",habillage=numcol+1,label="none",
#         title="")
