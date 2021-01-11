library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
NCI60
dim(nci.data)

nci.labs[1:4]
table(nci.labs)

#Using PCA on NCI60
pr.out=prcomp(nci.data, scale=TRUE)

#The Cols function assigns a unique colour to every element of a vector
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
#rainbow() takes its argument as a positive integer, returning a vector with that many distinct colors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z3")

#summary for a prcomp object returns the proportion of variance explained
summary(pr.out)
#plotting this object plots the PVE of the first few principal components
plot(pr.out)

#Plotting the PVE (invdividual and cumulative) of each principal component)
pve=100*pr.out$sdev^2/sum(pr.out$sdev)^2
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
#elements of PVE can be computed directly from the summary of pr.out
#Access the importance component and choose rows 2 and 3
#summary(pr.out$importance[2,] and pr.out$importance[3,])
#The elbow in the individual plot suggests that it would not be productive to examine any principal components after the 7th

#Using Clustering on NCI60
#optional standardization step
sd.data=scale(nci.data)

par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single Linkage", xlab="", sub="", ylab="")

#Dendrogram cutting
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out, 4)
table(hc.clusters, nci.labs)
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

hc.out

#k-means clustering and comparison to hierarchical clustering
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters, hc.clusters)

#performing hierarchical clustering on the first few principal components may suffice
#rather than doing it for the entire matrix
#In some cases doing this removes some noise that would've been present in the entire matrix but not the first few principal components
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
#K-means clustering can be performed on the first few principal components too