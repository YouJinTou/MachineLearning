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
