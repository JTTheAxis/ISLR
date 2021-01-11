states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
#apply lets us apply a function to any row or column of the dataset inputted
#1 for rows, 2 for columns
apply(USArrests, 2, var)
#prcomp() in the base R package for PCA
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
#center and scale components correspond to the means and sdevs of the scaling variables
pr.out$center
pr.out$scale
#rotation component = principal component loadings
#Each column represents a principal component loading vector
pr.out$rotation

dim(pr.out$x)
#biplot() to plot the first two components at once
biplot(pr.out, scale=0)
#scale=0 ensures that arrows are scaled to represent loadings

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

#sdev component for standard deviation
pr.out$sdev
#square that for variance
pr.var=pr.out$sdev^2
pr.var

#proportion of explained variance by each component can be found as follows
pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0, 1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type="b")
#cumsum() for cumulative summation of the vector
a=c(1,2,8, -3)
cumsum(a)
