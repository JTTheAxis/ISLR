#Let x be the base vector, and y contain the chances that any observation is in the bootstrap sample
x=rep(1:100000)
y=rep(0, 100000)
for (i in 1:100000){
  n=x[i]
  y[i]=1-(1-1/n)^n
}  
plot(x, y)

#Numerical testing
store=rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample(1:100,rep=TRUE)==4)>0
}
mean(store)
#About 0.64