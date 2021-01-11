college=read.csv("College.csv", header=T, na.strings="N/A")
View(college)
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
attach(college)
plot(Outstate, Private)
Elite=rep("No", nrow(college))
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college, Elite)