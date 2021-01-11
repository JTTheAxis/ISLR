p=20
n=1000

set.seed(38)

# Generate noise
e=rnorm(n)

# Generate Observations
X=c()
for(i in 1:p)
  X=cbind(X,rnorm(n,mean = sample(1:100,1),sd = sample(0:50,1)))
colnames(X)<-paste('X',1:20,sep = "")
X

# Generate Coefficients
betas=sample(0:100,p,replace = T)
betas[ !sample(0:1,replace = T,20) ]=0
names(betas)<-paste('B',1:20,sep = "")
betas

# Generate Target Variable
y.mat=X %*% matrix(betas,nrow = 20)
y.mat
y=apply(cbind(y.mat,e),1,sum)
y
summary(y)