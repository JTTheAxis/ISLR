#7
#Proportion between Distances
library(ISLR)
set.seed(2)
US=scale(USArrests)
Euclid=dist(US)^2
Correlation=as.dist(1-cor(t(US)))
summary(Correlation/Euclid)

#8 
#PVE Calculation

#a) using sdev
set.seed(1)
pr.out=prcomp(USArrests, scale=TRUE)
pr.var=pr.out$sdev^2
pve1=pr.var/sum(pr.var)
pve1

#b) Directly from loadings
set.seed(1)
pr.out=prcomp(USArrests, scale=TRUE)
loadings=pr.out$rotation
deno=sum(US^2)
sumvar=0
pr.var=rep(0, 4)
for (m in 1:4){
  valsum=0
  for (i in 1:nrow(US)){
    minisum=0
    for (j in 1:ncol(US)){
      minisum=minisum+(loadings[j,m]*US[i, j])
    }
    valsum=valsum+(minisum^2)
  }
  var=valsum/deno
  pr.var[m]=var
  sumvar=sumvar+var
}
pve2=pr.var/sumvar
pve2
#As supposed in the question, the PVE is the same both ways

#9
#Hierarchical Clustering

#a) Clustering
US=USArrests
hc.complete=hclust(dist(US), "complete")

#b) Cutting to make 3 clusters
hc.cut=cutree(hc.complete, 3)
plot(hc.complete)
print(hc.cut)

#c) Clustering with standardization to standard deviation 1
US=scale(USArrests)
hc.completescale=hclust(dist(US), "complete")
hc.cutscale=cutree(hc.completescale, 3)
plot(hc.completescale)
print(hc.cutscale)

#10
#Simulation

#a) Generation
#1 class per 
set.seed(3)
a=matrix(rnorm(60*50), ncol=50)
a[1:20,1]=a[1:20,1]+15
a[20:40,1]=a[20:40,1]-5
a[41:60,1]=a[41:60,1]-15

#b) PCA
pr.out=prcomp(a)
plot(pr.out$x[,1])

#c) K-Means (K=3)
km.out3=kmeans(a, 3, nstart=20)
km.out3$cluster

#d) K-Means (K=2)
km.out2=kmeans(a, 2, nstart=20)
km.out2$cluster

#e) K-Means (K=4)
km.out4=kmeans(a, 4, nstart=20)
km.out4$cluster

#f) K-Means on the Principal Components
km.outp=kmeans(pr.out$x[,1:2], 3, nstart=20)
km.outp$cluster

#g) K-Means with scaling
km.outscale=kmeans(scale(a), 3, nstart=20)
km.outscale$cluster

#11
#Gene testing

#a) Loading data
gene=read.csv("Ch10Ex11.csv", header=F)
fix(gene)

#b) Hierarchical clustering
cor=as.dist(1-cor(gene))
hc.complete=hclust(cor, method="complete")
plot(hc.complete, main="Complete Linkage")

hc.average=hclust(cor, method="average")
plot(hc.average, main="Average Linkage")

hc.single=hclust(cor, method="single")
plot(hc.single, main="Single Linkage")

#c) Similarity between genes
pr.out=prcomp(t(gene))
pr.out$rotation
summation=apply(pr.out$rotation, 1, sum)
vary=order(abs(summation), decreasing=T)
vary[1:5]
