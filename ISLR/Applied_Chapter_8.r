# 7
library(randomForest)
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
names(Boston)
summary(medv)
attach(Boston)
summary(medv)
X.train = Boston[train, -14]
X.test = Boston[-train, "medv"]
predictors = c(3, 5, 8)
trees = seq(1:500)
mses.3 = rep(NA, 500)
mses.5 = rep(NA, 500)
mses.8 = rep(NA, 500)

for (predictor in predictors) {
	for (tree in trees) {
		model = randomForest(medv~.,data=Boston,subset=train,mtry=predictor,ntree=tree)
		pred = predict(model,newdata=Boston[-train,])
		mse = mean((pred - X.test)^2)
		
		if (predictor == 3) {
			mses.3[tree] = mse
		}
		if (predictor == 5) {
			mses.5[tree] = mse
		}
		if (predictor == 8) {
			mses.8[tree] = mse
		}
	}
}

matplot(seq(1,500,1),cbind(mses.3,mses.5,mses.8),col=c("red","green","blue"))

which.min(mses.8)
mses.8[42]
which.min(mses.5)
mses.5[13]
which.min(mses.3)
mses.3[28]
# MSE is smallest when m = 5 (amongst 3, 5 and 8) and n = 13 trees.

# 8 
# a)
library(ISLR)
attach(Carseats)
set.seed(1)
train = sample(nrow(Carseats), nrow(Carseats) / 2)
X.train = Carseats[train, ]
X.test = Carseats[-train, ]
# b)
utils:::menuInstallPkgs()
library(tree)
tree.fit = tree(Sales~., data = X.train)
plot(tree.fit)
text(tree.fit, pretty = 0)
pred = predict(tree.fit, data = X.test)
test.mse = mean((pred - Sales)^2)
test.mse
test.mse = mean((pred - X.test$Sales)^2)
test.mse
# 14.5% test MSE
# c)
tree.cv = cv.tree(tree.fit)
names(tree.cv)
tree.cv
par(mfrow=c(1,2))
plot(tree.cv$size, tree.cv$dev)
plot(tree.cv$k, tree.cv$dev)
which.min(tree.cv$dev)
tree.cv$dev[8]
# 8 is best
tree.prune = prune.misclass(tree.fit, best = 8)
tree.prune = prune.tree(tree.fit, best = 8)
tree.prune.pred = predict(tree.prune.pred, data = X.test)
tree.prune.pred = predict(tree.prune, data = X.test)
test.mse.prune = mean((tree.prune.pred - X.test$Sales)^2)
test.mse.prune
# 13.6% test MSE when pruned
# d)
library(randomForest)
names(Carseats)
bag.fit = randomForest(Sales~., data = X.train, mtry = 10, importance = TRUE)
bag.fit
bag.pred = predict(bag.fit, data = X.test)
bag.mse = mean((bag.pred - X.test$Sales)^2)
bag.mse
# 12.2%, better than regression trees
importance (bag.fit)
# Price, shelve location and age
# e)
# There seem to be five variables with importance higher than 10%
rf.fit = randomForest(Sales~., mtry = 5, data = X.train, importance = TRUE)
rf.fit
rf.pred = predict(rf.fit, data = X.test)
rf.mse = mean((rf.pred - X.test$Sales)^2)
rf.mse
# 11.6%, even better than bagging
importance(rf.fit)
# Price, shelve location and age are still the most importance, but their importance has dropped somewhat

# 9
library(ISLR)
attach(OJ)
summary(OJ)
# a)
set.seed(1)
train = sample(nrow(OJ), 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]
# b)
library(tree)
tree.fit = tree(Purchase~., data = OJ.train)
summary(tree.fit)
# c)
plot(tree.fit)
text(tree.fit, pretty = 0)
# Thre uses 4 variables to make the splits and has slightly more terminal nodes
tree.fit
# SpecialCH -> split is 0.5, there are 70 observations in the subtree, the deviance is not very  high compared to other leaf nodes
# The outcome  is MM, and yprob is something like the purity of a node, I imagine, in this case 84/16
# d)
plot(tree.fit)
text(tree.fit, pretty = 0)
# LoyalCH  predominates in most splits
# e)
tree.pred = predict(tree.fit, data = OJ.test)
table(tree.pred, OJ.test$Purchase)
tree.pred = predict(tree.fit, data = OJ.test, type="class")
table(tree.pred, OJ.test$Purchase)
summary(tree.pred)
table(tree.pred, Purchase)
length(tree.pred)
length(Purchase)
length(OJ.test)
dim(OJ.test)
dim(tree.pred)
length(tree.pred)
dim(OJ)
tree.pred = predict(tree.fit, OJ.test, type="class")
length(tree.pred)
table(tree.pred, Purchase)
table(tree.pred, OJ.test$Purchase)
(12 + 49) / 270
# ~23% test error rate
# f)
tree.cv = cv.tree(tree.fit, FUN=prune.misclass)
# g)
tree.cv
plot(tree.cv$size, tree.cv$dev)
# h)
# 2, 5 and 8 splits give the same error 
# i)
tree.pruned = prune.tree(tree.fit, best = 5)
# j)
summary(tree.pruned)
summary(tree.fit)
# 16.5% for non-pruned tree and #18.3% for pruned
# k(
# k)
pruned.pred = predict(tree.pruned, OJ.test, type="class")
table(pruned.pred, OJ.test$Purchase)
(40+30) / 270
# Unpruned -> 22.6%; pruned -> 25.9%
# Something must've gone wrong

# 10
library(ISLR)
set.seed(1)
attach(Hitters)
# a)
Hitters = Hitters[-which(is.na(Salary), ]
Hitters = Hitters[-which(is.na(Salary)), ]
Salary = log(Salary)
summary(Hitters)
# b)
train = 1:200
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]
# c)
library(gbm)
mses = rep(NA, 10)
lambdas = seq(0, 1, 0.1)
lambdas
for (lambda in lambdas) {
boost.fit = gbm(Salary~., data = Hitters.train, distribution="gaussian", n.trees = 1000, shrinkage=lambda)
boost.pred = predict(boost.fit, data = Hitters.test, n.trees = 1000)
mse = mean((boost.pred - Hitters.test$Salary)^2)
mses[which(lambdas[lambda])] = mse
}
# The dataset is too small error
# g)
library(randomForest)
names(Hitters)
rf.fit = randomForest(Salary~., data = Hitters.train, ntree = 500, mtry = 19)
# Errors related to there being too few unique responses

# 11
# a)
library(ISLR)
set.seed(1)
train = 1:1000
attach(Caravan)
X.train = Caravan[train, ]
X.test = Caravan[-train, ]
# b)
library(gbm)
boost.fit = gbm(Purchase~., data = X.train, distribution = "bernoulli", n.trees = 1000, shrinkage = 0.01)
summary(boost.fit)
# The first three are  PREPSAUT, MKOOPKLA and MOPLHOOG
# c)
boost.prob = predict(boost.fit, data = X.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(boost.pred, X.test$Purchase)

glm.fit = glm(Purchase~., data = X.train, family = binomial)
glm.prob = predict(glm.fit, X.test, type = "response")
glm.pred = ifelse(glm.prob > 0.2, 1, 0)

# 12
library(ISLR)
attach(College)
set.seed(1)
train = sample(nrow(College), nrow(College) / 2)
X.train = College[train, ]
X.test = College[-train, ]
names(College)
Private
College
summary(College)
fix(College)
# Logistic Regression
glm.fit = glm(Private~., data = X.train, family = "binomial")
glm.probs = predict(glm.fit, data = X.test, type = "response")
glm.pred = rep("No", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != X.test$Private)
# 39%
# Boosting
library(gbm)
boost.fit = gbm(Private~., data = X.train, distribution = "bernoulli", n.trees = 1000)
BinaryPrivate = ifelse(College$Private == "Yes", 1, 0)
boost.fit = gbm(BinaryPrivate~.-Private, data = X.train, distribution = "bernoulli", n.trees = 1000)
fit(College)
fix(College)
length(BinaryPrivate)
dim(X.train)
BinaryPrivate = ifelse(X.train$Private == "Yes", 1, 0)
boost.fit = gbm(BinaryPrivate~.-Private, data = X.train, distribution = "bernoulli", n.trees = 1000)
boost.fit = gbm(BinaryPrivate~.-Private, data = X.train, distribution = "bernoulli", n.trees = 5000)
boost.probs = predict(boost.fit, data = X.test, n.trees = 5000)
boost.pred = rep(0, length(boost.probs))
boost.pred[boost.probs > 0.5] = 1
mean(boost.pred != X.test$BinaryPrivate[-train]
)
College$BinaryPrivate = ifelse(College$Private == "Yes", 1, 0)
boost.fit = gbm(BinaryPrivate~.-Private, data = X.train, distribution = "bernoulli", n.trees = 5000)
boost.probs = predict(boost.fit, data = X.test, n.trees = 5000)
boost.pred = rep(0, length(boost.probs))
boost.pred[boost.probs > 0.5] = 1
mean(boost.pred != College$BinaryPrivate[-train])
# 40%
# Bagging
library(randomForest)
names(College)
bagging.fit = randomForest(Private~., data = X.train, mtry = 15)
bagging.pred = predict(bagging.fit, data = X.test)
mean(bagging.pred != X.test$Private)
# 41%
# Random forests
rf.fit = randomForest(Private~., data = X.train, mtry = 4)
rf.pred = predict(rf.fit, data = X.test)
mean(rf.pred != X.test$Private)
# 38%
# Looks like random forests perform best