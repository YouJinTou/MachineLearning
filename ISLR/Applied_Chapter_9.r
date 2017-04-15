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