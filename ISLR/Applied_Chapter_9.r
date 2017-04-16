# 4
set.seed(1)
x=matrix(rnorm(100*2), ncol=2)
y = c(rep(-1, 50), rep(1, 50))
x[y==1, ] = x[y==1, ] + 1
plot(x, col = (3 -y))
# The observations are not linearly separable
dat = data.frame(x=x, y=as.factor(y))
library(e1071)
svc = svm(y~., data = dat, kernel="linear", cost=10, scale=F)
svc$index
summary(svc)
x.test = matrix(rnorm(100), ncol=2)
y.test = c(rep(-1,50), rep(1, 50))
x[y==1, ] = x[y==1, ] + 1
plot(x.test, col = (3 -y.test))
dat.test = data.frame(x=x.test, y=as.factor(y.test))
svc.tune = tune(svm, y~., data=dat, kernel="linear",
ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(svc.tune)
svc.best = svc.tune$best.model
summary(svc.best)
x.test[y.test==1,] = x.test[y.test==1,] + 1
x.test = matrix(rnorm(100*2), ncol=2)
y.test = sample(c(-1,1), 100, rep=TRUE)
x.test[y.test==1,] = x.test[y.test==1,] + 1
dat.test = data.frame(x=x.test,y=as.factor(y.test))
svc.pred.train = predict(svc.best, dat)
table(predicted = svc.pred.train, truth = dat$y)
# 18% error on the training data with the linear kernel
svc.pred.test = predict(svc.best, dat.test$y)
summary(svc.best)
svc.pred.test = predict(svc.best, dat.test)
table(predicted=svc.pred.test, truth=dat.test$y)
# 17% error on test data, which is slightly better than that on the training data. Surprising
svm = svm(y~., data=dat, kernel="radial", gamma=1, cost=1)
# Unable to perform tuning
svm.pred.train = predict(svm, dat)
table(predicted=svm.pred.train, truth=dat$y)
# 19% error on the training data
svm.pred.test = predict(svm, dat.test)
table(predicted=svm.pred.test, truth=dat.test$y)
# 20% test error data
# The supper vector classifier outperforms the support vector machine on a non-linear set. I'm baffled, but I must've done something wrong.

# 5
# a)
set.seed(1)
x1 = runif(500) -0.5
x2 = runif(500) -0.5
y = 1 * (x1^2 - x2^2 > 0)
# b)
plot(x1, x2)
plot(x1[y==0], x2[y==0], col="blue")
points(x1[y==1], x2[y==1], col="red")
# c)
glm.fit = glm(y~x1+x2, family="binomial")
lines(glm.fit)
summary(glm.fit)
# d)
data = data.frame(x1, x2, y)
glm.probs = predict(glm.fit, data, type="response")
glm.pred = ifelse(glm.probs > 0.5, 1, 0)
pred.pos = data[glm.pred == 1, ]
pred.pos
pred.pos = data[glm.pred==1]
pred.pos
pred.neg = data[glm.pred==0,]
points(pred.pos$x1, pred.pos$x2, col="green", pch="+")
points(pred.neg$x1, pred.neg$x2, col="yellow", pch="-")
points(pred.neg$x1, pred.neg$x2, col="black", pch="-")
# e)
glm.nlfit = glm(y~poly(x1,2)+poly(x2,2), data=data, family=binomial)
# f)
glm.probs = predict(glm.nlfit, data, type="response")
glm.pred = ifelse(glm.probs > 0.5, 1, 0)
pred.pos = data[glm.pred==y,]
pred.pos = data[glm.pred==1,]
pred.neg = data[glm.pred==0,]
plot(pred.pos$x1, pred.pos$x2, col="blue", pch="+")
points(pred.neg$x1, pred.neg$x2, col="red", pch="-")
# g)
library(e1071)
svc = svm(as.factor(y)~x1+x2, kernel="linear", data=data, cost=1)
svc.pred = predict(svc, data)
svc.pos = data[svc.pred==1, ]
svc.neg = data[svc.pred==0, ]
plot(svc.pos$x1, svc.pos$x2, col="blue", pch="+")
svc = svm(as.factor(y)~., kernel="linear", data=data, cost=1)
svc.pred = predict(svc, data)
svc.pos = data[svc.pred==1, ]
svc.neg = data[svc.pred==0, ]
plot(svc.pos$x1, svc.pos$x2, col="blue", pch="+")
svc.pred
# The linear kernel cannot find a linear decision boundary
# h)
svm = svm(as.factor(y)~., kernel="radial", gamma=1, data=data, cost=1)
svm.pred = predict(svc, data)
svm.pred
svm.fit = svm(as.factor(y)~., data, kernel="radial", gamma=1)
svm.pred = predict(svm.fit, data)
svm.pred
svm.pos = data[svm.pred==1, ]
svm.neg = data[svm.pred==0, ]
plot(svm.pos$x1, svm.pos$x2, col="blue", pch="+")
points(svm.neg$x1, svm.neg$x2, col="red", pch="-")

# 6
set.seed(1)
x = matrix(rnorm(1000*2), ncol=2)
y = c(rep(-1, 500, rep(1, 500)))
x[y==1,] = x[y==1,] + 3.4
plot(x, col=(3-y))
# b)
data = data.frame(x,y)
svc = svm(as.factor(y)~., data, kernel="linear", cost=1, scale=FALSE)
plot(svc,data)
svc.tune = tune(method = "svm", y~x, data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1.5, 10, 100)))
summary(svc.tune)
svc.tune$performances$error
# The higher the cost, the lower the CV error
# c)
xtest = matrix(rnorm(1000*2), ncol=2)
ytest=sample(c(-1,1), 1000, rep=TRUE)
xtest[ytest==1,] = xtest[ytest==1,] + 3.4
testdat = data.frame(x=xtest,y=as.factor(ytest))
testdat.tune = tune(method="svm", y~., data=testdat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1.5, 10, 100)))
summary(testdat.tune)
costs = c(0.001, 0.01, 0.1, 1.5, 10, 100)
errors = rep(NA, 6)

for (i in 1:length(costs)){
fit = svm(as.factor(y)~x, data=data, kernel="linear", cost=costs[i])
pred = predict(fit, testdat)
errors[i] = sum(pred != testdat$y)
}

pred
errors
# All costs seem to be performing about the same, so I must've done something wrong

# 7
library(ISLR)
# a)
attach(Auto)
mileage.median = median(mpg)
binary.mpg = ifelse(mpg > mileage.median, 1, 0)
Auto$binary.mpg = as.factor(binary.mpg)
# b)
set.seed(1)
names(Auto)
svm.tune = tune("svm", binary.mpg~., data=Auto, kernel="linear", ranges=list(cost=c(0.1, 1, 10, 100)))
summary(svm.tune)
# Best when cost is 1
# c)
svm.rad.tune = tune("svm", binary.mpg~., data=Auto, kernel="radial",
ranges=list(cost=c(0.1, 1, 10, 100),gamma=c(1,5,10,100)))
svm.poly.tune = tune("svm", binary.mpg~., data=Auto, kernel="polynomial",
ranges=list(cost=c(0.1, 1, 10, 100),degree=c(2,3,4)))
summary(svm.rad.tune)
# cost - 10, gamma = 1 for rad
summary(svm.poly.tune)
# cost 100, degree 2 for polynomial
# d)
pairs(Auto)
svm.linear = svm(binary.mpg~.-name-mpg,data=Auto,kernel="linear",cost=1) 
svm.rad = svm(binary.mpg~.-name-mpg,data=Auto,kernel="linear",cost=10,gamma=1)))
# 8
library(ISLR)
# a)
train = sample(nrow(OJ), 800)
OJ.train = OJ[train,]
OJ.test = OJ[-train,]
# b)
svc.fit = svm(Purchase~., data=OJ.train, kernel="linear", cost=0.01)
summary(svc.fit)
# c)
train.pred = predict(svc.fit, OJ.train)
table(predicted=train.pred,truth=OJ.train$Purchase)
# 16.6% training error
test.pred = predict(svc.fit, OJ.test)
table(predicted=test.pred,truth=OJ.test$Purchase)
# 18.4% test error
# d)
svc.tune = tune("svm", Purchase~., data=OJ.train, ranges=list(cost=c(0.01,0.1,1,5,10)))
svc.tune = tune("svm", Purchase~., data=OJ.train, kernel="linear", ranges=list(cost=seq(0.01, 10, by=0.25)))
summary(svc.tune)
# best cost is 0.01
# e)
svc.best = svc.tune$best.model
train.pred = predict(svc.best, OJ.train)
table(train.pred, OJ.train$Purchase)
# 16.6% training
test.pred = predict(svc.best, OJ.test)
table(test.pred, OJ.test$Purchase)
# 18.4% test error
# The cost paramter and thus the errors are identical. I must've done something wrong