#K-Means Clustering
#kmeans() performs K-means clustering
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25, 1]=x[1:25, 1]+3
x[1:25,2]=x[1:25,2]-4
km.out=kmeans(x,2,nstart=20)
#nstart argument indicates how many random starting positions to be generated
#cluster component stores the assignments of each of the observations
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

#trying clustering on our generated data with K=3
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

set.seed(21342342)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss
#nstart=20 produces a superior result than nstart=1
#more possible states were chosen, thus leading to a higher chance of getting the optimal outcome
#tot.withinss is the total within-cluster sum of squares
#individual within-cluster sum of squares are in the withinss component
#In general, run kmeans() with a high starting value of nstart like 20 or 50

#Hierarchical Clustering
#hclust() for hierarchical clustering
hc.complete=hclust(dist(x), method="complete")
#dist() computes the inter-observation Euclidean distance matrix for a matrix of observations
#method argument determines what form of linkage to apply
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", ylab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", ylab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", ylab="", sub="", cex=.9)

#cutree() returns the cluster labels applied under a given cut of the dendrogram
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
#While complete/average linkage splits the observations correctly,
#single linkage has a one-point cluster
#4 clusters and single linkage is better but still has two singletons
cutree(hc.single, 4)
#scale() used to scale the variables
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="hierarchical Clustering with Scaled Features")

#Correlation-based distance can be computed with as.dist()
#Turns square symmetric matrices into forms that hclust() can recognize
#Note that correlation-based distance only makes sense for observations with 3+ features
#Absolute correlation between two observations, each with two predictors, will always be 1
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
     