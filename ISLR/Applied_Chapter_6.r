# 8.
# a)
X = rnorm(100)
eps = rnorm(100)
# b)
y = 1 + 2 * X + 3 * X^2 + 4 * X ^ 3 + eps
# c)
history()
length(y)
y
library(leaps)
data = data.frame(y, X)
dim(data)
regfit.best = regsubsets(data$y ~ data$X, data)
data$y
data$X
?poly
regfit.best = regsubsets(y ~ poly(X, 10), data = data, nvmax = 10)
summary(regfit.best)
names(summary(regfit.best))
regfit.summary = summary(regfit.best)
regfit.summary$rsq
which.min(regfit.summary$cp)
which.min(regfit.summary$bic)
which.max(regfit.summary$adjr2)
regfit.summary$adjr2
plot(regfit.summary$cp)
plot(regfit.summary$bic)
plot(regfit.summary$adjr2)
coefficients(regfit.best)
coefficients(regfit.best, 1)
coefficients(regfit.best, 2)
coefficients(regfit.best, 3)
coefficients(regfit.best, 4)
# Forgot to seed, so got 4, 3 and 7 for CP, BIC, and Adjusted R Squared, respectively
regfit.summary
# Both #3 and #4 include include the second and third degree polynomials.
# d)
regfit.fwd = regsubsets(y ~ X, data = data, nvmax = 10, method = "forward")
X
y
regfit.fwd = regsubsets(y ~ X, data = data, nvmax = 10, method = "forward")
regfit.fwd = regsubsets(y ~ poly(X, 10), data = data, nvmax = 10, method = "forward")
regfit.bwd = regsubsets(y ~ poly(X, 10), data = data, nvmax = 10, method = "backward")
fwd.summary = summary(regfit.fwd)
bwd.summary = summary(regfit.bwd)
which.min(fwd.summary$cp)
# 4
which.min(fwd.summary$bic)
# 3
which.max(fwd.summary$adjr2)
# 7
which.min(bwd.summary$cp)
# 4
which.min(bwd.summary$bic)
# 3
which.max(bwd.summary$adjr2)
# 7
fwd.summary
bwd.summary
# We get mostly the same results, with backward selection choosing the third degree polynomial in favor of the second where poly(X, 2)
utils:::menuInstallPkgs()
library(glmnet)
regfit.lasso = glmnet(data$X, data$y, alpha = 1, lambda = grid)
X.mat = model.matrix(y ~ poly(X, 10), data = data)[, -1]
regfit.lasso = cv.glmnet(X.mat, y, alpha = 1, lambda = grid)
regfit.lasso = cv.glmnet(X.mat, y, alpha = 1)
plot(regfit.lasso)
best.lambda = regfit.lasso$lambda.min
best.lambda
# 0.07824949 is the value of lambda that brings the smallest MSE.
plot(regfit.lasso)
regfit.lasso.pred = glmnet(X.mat, y, alpha = 1)
lasso.pred = predict(regfit.lasso.pred, s = best.lambda, type = "coefficients")
lasso.pred
summary(lasso.pred)
# It pikcs X, X^2, X^3, and the rest are with small coefficients.
set.seed(1)
y = 1 + 7 * X ^ 7 + eps
data = data.frame(y, X)
regfit.best = regsubsets(y ~ poly(X, 10), data = data, nvmax = 10)
regfit.summary = summary(regfit.best)
which.min(regfit.summary$cp)
# 8
which.min(regfit.summary$bic)
# 7
which.max(regfit.summary$adjr2)
# 9
X.mat = model.matrix(y ~ poly(X, 10), data = data)[, -1]
regfit.lasso.cv = cv.glmnet(X.mat, y, alpha = 1)
best.lambda = reegfit.lasso.cv$lambda.min
best.lambda = regfit.lasso.cv$lambda.min
best.model = glmnet(X.mat, y, alpha = 1)
lasso.pred = predict(best.model, s = best.lambda, type = "coefficients")
lasso.pred
coefficients(regfit.best, 7)
# The lasso overestimates all coefficients and does not even select X^7
save.image("D:\\Programming\\ML\\Applied_Chapter_6.RData")
q()
set.seed(1)
# 9.
# a)
set.seed(1)
train=sample(1:nrow(College), nrow(College)/2)
# 9
# a)
library(ISLR)
set.seed(1)
train=sample(1:nrows(College),nrows(College) / 2)
train=sample(1:nrow(College),nrow(College) / 2)
test=!train
test=-train
test
test=!train
test
test=-(train)
test
train
test=(-train
)
test
# b)
names(College)
summary(College$Apps)
attach(College)
College.train = College[train,]
College.test=College[test,]
dim(College.test)
dim(College.train)
lm.fit = lm(Apps~.,data=College.train)
lm.fit
summary(lm.fit)
lm.pred = predict(lm.fit, College.test)
lm.pred
lm.error = mean((lm.pred - College.test[,"Apps"])^2)
lm.error
# 1,108,531
# c)
library(glmnet)
?cv.glmnet
cv.ridge = cv.glmnet(College.train[,-"Apps"], College.train[, "Apps"],alpha=0)
cv.ridge = cv.glmnet(College.train[,.-"Apps"], College.train[, "Apps"],alpha=0)
cv.ridge = cv.glmnet(College.train[,-Apps], College.train[, "Apps"],alpha=0)
ridge.train.mat = model.matrix(Apps~., data=College.train)
cv.ridge = cv.glmnet(ridge.train.mat, College.train[, "Apps"],alpha=0)
cv.ridge
best.lambda = cv.ridge$lambda$min
best.lambda = cv.ridge$lambda.min
cv.ridge = cv.glmnet(ridge.train.mat, College.train[, "Apps"],alpha=0,thresh=1e-12)
best.lambda = cv.ridge$lambda.min
best.lambda
ridge.test.mat = model.matrix(Apps~., data=College.test)
ridge.pred = predict(cv.ridge, s=best.lambda,newx=ridge.test.mat)
ridge.error = mean((ridge.pred-College.test[,"Apps"])^2)
ridge.error
lm.error
# Ridge error is 1,038,121 as opposed to linear regression's 1,108,531
# d)
cv.lasso = cv.glmnet(ridge.train.mat, College.train[, "Apps"], alpha = 1, thresh = 1e-12)
best.lambda.lasso = cv.lass$lambda.min
best.lambda.lasso = cv.lasso$lambda.min
best.lambda.lasso
lasso.pred = predict(cv.lasso, s=best.lambda.lasso,newx=ridge.test.mat)
lasso.error = mean((lasso.pred - College.test[,"Apps"])^2)
lasso.eror
lasso.erorr
lasso.error
# 1,044,840
lasso.mod = model.matrix(Apps~.,data=College)
lasso.coef = glmnet(lasso.mod, College[,"Apps"],alpha = 1)
predict(lasso.coef,s=best.lambda.lasso,type="coefficients")
# All coefficients but two are present, for a total of 15
# e)
library(pls)
utils:::menuInstallPkgs()
library(pls)
pcr.fit=pcr(Apps~.,data=College.train,scale=TRUE,validation="CV")
pcr.fit
summary(pcr.fit)
validationplot(pcr .fit ,val.type=" MSEP")
validationplot(pcr.fit, val.type=" MSEP")
validationplot(pcr.fit, val.type="MSEP")
validationplot(pcr.fit, val.type="MSEP")
# The error seems to plateau after 5 primary components
pcr.pred = predict(pcr.fit,College.test,ncomp=5)
pcr.error = mean((pcr.pred-College.test[,"Apps"])^2)
pcr.error
# 1,907,827, much higher
# f)
pls.fit = plsr(Apps~.,data=College.train,scale=T,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
# It seems to plateau at 5 again.
pls.pred=predict(pls.fit,College.test,ncomp=5)
pls.error = mean((pls.pred-College.test[,"Apps"])^2)
pls.error
# 1,158,597
# 1,158,597, roughly the same as LR, ridge, and the lasso.
# g)
summary(lm.pred)
lm.pred
test.mean = mean(College.test[,"Apps"])
test.mean
apps = College.test[,"Apps"]
SSt = mean((apps - test.mean)^2)
lm.r2 = 1 - mean((apps - lm.pred)^2) / SSt
ridge.r2 = 1 - mean((apps - ridge.pred)^2) / SSt
lasso.r2 = 1 - mean((apps - lasso.pred)^2) / SSt
pcr.r2 = 1 - mean((apps - pcr.pred)^2) / SSt
pls.r2 = 1 - mean((apps - pls.pred)^2) / SSt
barplot(c(lm.r2,ridge.r2,lasso.r2,pcr.r2,pls.r2),col="blue", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="R-Squared")
# As expected from the errors earlier, all perform equally well save for PCR with an R^2 below 90%
history()
history(max.show=inf)
history(max.show=Inf)
# 10.
# a)
n=1000
p=20
?matrix
X = matrix(rnorm(n*p), n, p)
b=rnorm(p) # vector of coefficients
b[2]=0
b[3]=0
b[4]=0
eps=rnorm(p)
length(eps)
y = X*b+eps
y
y= X %*% b + eps
y
dim(X %*% b)
X %*% b
# b)
train = sample(1:1000, 100, replace=F)
train
X.train = X[train,]
X.test = X[!train,]
X.test
X.test = X[-train,]
X.test
y.train = y[train,]
y.test = y[-train,]
# c)
best.subset.train = regsubsets(y.train~X.train,nvmax=p)
library(leaps)
best.subset.train = regsubsets(y.train~X.train,nvmax=p)
best.subset.train = regsubsets(y.train~X.train,data=data.frame(y.train,X.train),nvmax=p)
errors = rep(0,p)
coef(best.subset.train,id=1)
coef(best.subset.train,id=2)
coef(best.subset.train,id=3)
for (i in 1:p){
coef = coef(best.subset.train,id=i)
pred = X.train[, 
}
X.train
?colnames
names(coef)
coefi
coef
names(coef(best.regfit.train,1))
ls()
names(coef(best.subset.train,1))
x.cols = colnames(X.train,do.NULL=F,prefix="X.")
x.cols
x.cols = colnames(X.train,do.NULL=F,prefix="X.train")
x.cols
for (i in 1:p) {
coef = coef(best.subset.train, i)
pred = X.train[, x.cols %in% names(coef)] * coef[names(coef) %in% x.cols]
errors[i]=mean((pred - y.train)^2)
}
plot(errors)
# d)
for (i in 1:p){
coef = coef(best.subset.train, i)
pred = X.test[,x.cols %in% names(coef)] * coef[names(coef) %in% x.cols]
errors[i] = mean((pred-y.test)^2)
}
plot(errors)
# e)
which.min(errors)
# 1
coef(best.subset.train, 1)
# The test MSE is minimized by a model with an intercept + one feature
# f)
plot(errors)
errors
plot(errors)
best.subset.train
summary(best.subset.train)
# ???
# g)
# ???
coefficients.count = rep(0, p)
coefficients.error = rep(0, p)
for (i in 1:p){
coef = coef(best.subset.train, i)
coefficients.coun[i] = length(coef)-1 # excluding the intercept
coefficients.count[i] = length(coef)-1 # excluding the intercept
}
for (i in 1:p){
coef = coef(best.subset.train, i)
coefficients.count[i] = length(coef)-1 # excluding the intercept
coefficients.error = sqrt(sum(b[x.cols %in% names(coef)] - coef[names(coef) %in% x.cols])^2)
}
for (i in 1:p){
coefficients.count[i] = length(coef)-1 # excluding the intercept
coefficients.error[i] = sqrt(sum(b[x.cols %in% names(coef)] - coef[names(coef) %in% x.cols])^2)
}
plot(coefficients.count, coefficients.error)
coefficients.count
coef(best.subset.train, 1)
length(coef(best.subset.train, 1) - 1
}
length(coef(best.subset.train, 1)
)
length(coef(best.subset.train, 1)) - 1
for (i in 1:p){
coef = coef(best.subset.train, id=i)
coefficients.count[i] = length(coef)-1 # excluding the intercept
coefficients.error[i] = sqrt(sum(b[x.cols %in% names(coef)] - coef[names(coef) %in% x.cols])^2)
}
plot(coefficients.count,coefficients.error)
plot(errors)
par(mfrow=c(2,2))
plot(errors)
plot(coefficients.count,coefficients.error)
# I see a U-shaped curve -> as the coefficients' number increases, the difference decreases, until it begins increasing

