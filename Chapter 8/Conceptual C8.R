#3
#Comparing Gini index, classification error, and entropy
par(mfrow=c(1,1))
plot(1, xlim=c(0, 1), ylim=c(0, 1), xlab="pm1", ylab="node purity measure", type="n")
p1=seq(0, 1, by=.00001)
p2=1-p1
error=c()
for (i in 1:length(p1)){
  error=cbind(error, 1-max(p1[i], p2[i]))
}
gini=2*p1*p2
entropy=-(p1*log(p1)+p2*log(p2))
lines(p1, error)
lines(p1, gini)
lines(p1, entropy)
