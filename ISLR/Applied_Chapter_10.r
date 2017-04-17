# 7
library(ISLR)
set.seed(1)
scaled = scale(USArrests[1:2,])
dist = dist(scaled)^2
cor = as.dist(1 - cor(t(dist)))
# I don't see how these are proportional

# 8
# a)
pr.out = prcomp(USArrests, scale=TRUE)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve
# b)
data = scale(USArrests)
get.score = function(loadings, features){
return (sum(loadings * features)^2)
}
scores = 0

for (obs in 1:nrow(data)){
for (m in 1:4){
scores = scores + get.score(pr.out$rotation[,m], data[obs,])
}
}

features.score = apply(data, 2, sum)
features.score = features.score ^ 2
m1pve = scores / features.score # Equation 10.8
# No good

# 9
# a)
hc.complete = hclust(dist(scale(USArrests)), method="complete")
plot(hc.complete)
# b)
cutree(hc.complete, 3)
# c)
hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)
cutree(hc.complete, 3)
hc.s.complete = hclust(dist(scale(USArrests)), method="complete")
table(hc.s.complete)
table(cutree(hc.s.complete, 3))
# d)
# When the data isn't scaled, it gets skewed by the assaults variable
# 10
# Can't separate the three sets. The simulation is copied from https://github.com/asadoughi/stat-learning/blob/master/ch10/10.Rmd
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1
# b)
pc.out = prcomp(x)
plot(pc.out$x[,1:2], col=1:3, xlab="Z1", ylab="Z2", pch=20)
# c)
km.out = kmeans(x, 3, nstart=20)
summary(km.out)
km.out$cluster
table(km.out$cluster, c(rep(3,20), rep(1, 20), rep(2, 20)))
# It matches up 
# d)
km.out.2 = kmeans(x, 2, nstart=20)
km.out$cluster
km.out.2$cluster
# There seems to be a similar separation, but the first label adopted the last label
# e)
km.out.4 = kmeans(x, 4, nstart=20)
km.out.4$cluster
plot(km.out.4$cluster)
# The first class seems to be conflicting with itself
# f)
km.out.pc.2 = kmeans(pc.out$x[, 1:2], 3, nstart=20)
table(km.out.pc.2$cluster, c(rep(3,20), rep(1, 20), rep(2, 20)))
km.out.pc.2$clusters
km.out.pc.2$cluster
table(km.out.pc.2$cluster, c(rep(3,20), rep(2, 20), rep(1, 20)))
# It's a perfect match
# g)
km.scaled = kmeans(scale(x), 3, nstart=20)
km.scaled$cluster
# Doesn't look good this time. We change the distances to be closer to one another when we scale them

# 10
# a)
X = read.csv("Ch10Ex11.csv", header=F)
dim(X)
# b)
cor.dist = as.dist(1 - cor(X))
hc.complete = hclust(cor.dist, method="complete")
hc.single = hclust(cor.dist, method="single")
hc.average = hclust(cor.dist, method="average")
par(mfrow=c(1,3))
plot(hc.complete)
plot(hc.single)
plot(hc.average)
# Yes, it depends on the linkage method. "Average" returns three groups