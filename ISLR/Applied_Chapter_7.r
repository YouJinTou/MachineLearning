# 6
# a)
set.seed(1)
library(ISLR)
summary(Wage)
library(boot)
degrees = rep(NA,10)
for (i in 1:10){
glm.fit = glm(wage~poly(age,i), data=Wage)
degrees[i] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}
degrees
best.degree = min(degrees)
best.degree
plot(degrees)
best.degree = min.which(degrees)
best.degree = which.min(degrees)
best.degree
attach(Wage)
a.1 = lm.fit(wage~poly(age,1))
a.1 = lm.fit(wage~poly(age,1), data=Wage)
a.1 = lm(wage~poly(age,1))
wage.polys = rep(NA,10)
for(i in 1:10){
wage.polys[i]=lm(wage~poly(age,i))
}
a.1 = lm(wage~poly(age,1))
a.2 = lm(wage~poly(age,2))
a.3 = lm(wage~poly(age,3))
a.4 = lm(wage~poly(age,4))
a.5 = lm(wage~poly(age,5))
a.6 = lm(wage~poly(age,6))
a.7 = lm(wage~poly(age,7))
a.8 = lm(wage~poly(age,8))
a.9 = lm(wage~poly(age,9))
a.10 = lm(wage~poly(age,10))
a.sum = anova(a.1,a.2,a.3,a.4,a.5,a.6,a.7,a.8,a.9,a.10)
a.sum
# Fourth degree polynomial still has significance, and, for some reason, 9 as well
# ANOVA and cross-validation reached the same conclusion
lm.fit = lm(wage~poly(age,3))
lm.pred=predict(lm.fit)
lm.pred
plot(lm.pred)
plot(age)
ages = range(age)
ages
ages=seq(from=ages[1],to=ages[2])
ages
lines(ages,lm.pred)
lm.pred=predict(lm.fit,data.frame(age=ages))
lines(ages,lm.pred)
# b)
cvs = rep(NA,10)
for(i in 2:10){
cut = cut(age,i)
lm.fit=glm(wage~age)
cvs[i]=cv.glm(Wage,lm.fit,K=10)$delta[2]
}
cvs
plot(cvs)
# 9 seems to be lowest
lm.fit = glm(wage~cut(age,9))
ages=range(age)
ages=seq(from=ages[1], to=ages[2])
lm.pred=predict(lm.fit,data.frame(age=ages))
plot(age)
lines(ages,lm.pred)
# 7
names(Wage)
summary(Wage)
pairs(Wage)
plot(wage, maritl)
summary(jobclass)
plot(wage,jobclass)
lm.fit = lm(wage~poly(maritl,3))
lm.fit = lm(wage~maritl)
lm.fit
summary(lm.fit)
lm.fit(wage~maritl+jobclass)
lm.fit=lm(wage~maritl+jobclass)
summary(lm.fit)
# Looks like the jobclass and maritl variables carry significance
lm.cut = lm(wage~cut(jobclass,2))
library(gam)
utils:::menuInstallPkgs()
gam.fit = gam(wage~maritl+jobclass)
library(gam)
gam.fit = gam(wage~maritl+jobclass)
gam.fit
summary(gam.fit)
gam.fit = gam(wage~maritl+jobclass+poly(age,4))
summary(gam.fit)
# 8
summary(Auto)
set.seed(1)
attach(Auto)
detach(Wage)
attach(Auto)
detach(Auto)
detach(Auto)
detach(Auto)
attach(Auto)
cov(Auto)
cov(Auto-name)
cov(data.frame(Auto-name))
pairs(Auto)
#mpg -> displacement, horsepower, weight, acceleration, year
# Polynomial linear regression
errors = rep(NA, 10)
for (i in 1:10){
lm.fit = mpg
}
summary(lm(mpg~displacement,horsepower,weight,acceleration))
names(Auto)
summary(lm(mpg~displacement+horsepower+weight+acceleration))
# I'll go with weight
?deviance
fit.1 = lm(mpg~weight)
fit.2 = lm(mpg~poly(weight,2))
fit.3 = lm(mpg~poly(weight,3))
fit.4 = lm(mpg~poly(weight,4))
fit.5 = lm(mpg~poly(weight,5))
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
# Second degree is best
cv.errors = rep(NA, 5)
for (d in 1:5){
fit = glm(mpg~poly(weight,d))
cv.errors[d] = cv.glm(Auto, fit,K=10)$delta[2]
}
which.min(cv.errors)
# Third degree according to CV
cv.errors = rep(NA, 5)
for (d in 1:5){
}
for (c in 2:5) {
cut = cut(weight,c)
fit =glm(mpg~cut)
cv.errors[c]=cv.glm(Auto,fit,K=10)$delta[2]
}
cut(weight,2)
# ?
library(splines)
errors = rep(NA,5)
for(k in 3:5){
fit = glm(mpg~ns(weight,df=k))
errors[k]=cv.glm(Auto,fit,K=10)$delta[2]
}
which.min(errors)
errors
#  4
fit = gam(mpg~ns(weight,4)+ns(horsepower,4))
summary(fit)
summary(Boston)
library(MASS)
detach(Auto)
attach(Boston)
lm.fit = lm(nox~poly(dis,3))
summary(lm.fit)
dis.range = range(dis)
dis.grid = seq(from=dis.range[1],to=dis.range[2])
lm.pred=predict(lm.fit,dis.grid)
lm.pred=predict(lm.fit,dis=dis.grid)
plot(nox~dis)
lines(dis.grid,lm.pred)
length(lm.pred)
length(dis.grid)
# b)
rss = rep(NA,10)
for (i in 1:10){
lm.fit = lm(nox~poly(dis,i))
rss[i]=sum(lm.fit$residuals^2)
}
plot(rss)
min(rss)
min.which(rss)
which.min(rss)
# 10th degree
# c
d = rep(NA,10)
for (i in 1:10){
glm.fit = glm(nox~poly(dis,i))
d[i] = cv.glm(Boston,glm.fit,K=10)$delta[2]
}
plot(d)
which.min(d)
# 10 degrees, strange
# Though 3-5 seem pretty close
# d)
summary(dis)
plot(dis)
fit = lm(nox~bs(dis,knots=c(4, 8, 12))
)
summary(fit)
fit = lm(nox~bs(dis,df=4,knots=c(4, 8, 12)))
summary(fit)
# I chose the knots where there were some signs of separation
pred = predict(fit,list(dis=dis.grid))
plot(nox~dis)
lines(dis.grid,pred)
# e)
rss = rep(NA,15)
for (i in 3:15){
fit = lm(nox~bs(dis,df=i))
rss[i] = sum(fit$residuals^2)
}
rss[-c(1,2)]
plot(rss)
# The training RSS decreases, of course, as more degrees are added
# f)
cv = rep(NA, 15)
for (i in 3:15){
lm.fit = glm(nox~bs(dis,df=i))
cv[i] = cv.glm(Boston,lm.fit,K=10)$delta[2]
}
warnings()
plot(3:15,cv[-c(1,2)])
# 6 df is best
cv
plot(3:15,cv[-c(1,2)])
min.which(cv)
which.min(cv)
# 10
# a)
detach(Boston)
attach(College)
names(College)
set.seed(1)
train = sample(length(College), length(College)/2)
train
train = sample(length(Outstate), length(Outstate)/2)
train
test = -train
X.train = College[train,]
X.test = College[test,]
library(leaps)
reg.fit = regsubsets(Outstate~.,data=X.train,nvmax=18,method="forward")
summary(reg.fit)
summary(reg.fit)$cp
reg.sum = summary(reg.fit)
min.cp = min(reg.sum$cp)
min.bic = min(reg.sum$bic)
max.adjr2 = max(reg.sum$adjr2)
min.cp
min.bic
max.adjr2
plot(reg.sum$adjr2)
plot(reg.sum$cp)
plot(reg.sum$bic)
# Looks like 7 produces the best fit
# for all metrics
# eyeballing it
best.fit = regsubsets(Outstate~.,data=College,method="forward")
best.coef = coef(best.fit, id=7)
best.coef
# b)
plot(Room.Board)
plot(Personal)
plot(PhD)
plot(perc.alumni)
plot(Expend)
plot(Grad.Rate)
plot(Expend)
gam.fit = gam(Outstate ~ Private + s(Room.Board, df=2) + s(Personal, df=2) + s(Phd, df=2) +
s(perc.alumni, df=2) + s(Expend, df=3) + s(Grad.Rate,df=2)
)
gam.fit = gam(Outstate ~ Private + s(Room.Board, df=2) + s(Personal, df=2) + s(PhD, df=2) +
s(perc.alumni, df=2) + s(Expend, df=3) + s(Grad.Rate,df=2))
plot(gam.fit)
par(mfrow=c(2,3))
plot(gam.fit)
par(mfrow=c(3,3))
plot(gam.fit)
# The plots seem awfully off???
# c)
gam.pred = predict(gam.fit,X.test)
gam.error = mean((X.test$Outstate - gam.pred)^2)
gam.error
gam.tss = mean((X.test$Outstate - mean(X.test$Outstate))^2)
test.rss = 1 - gam.error / gam.tss
test.rss
# 48%
gam.tss = sum((X.test$Outstate - mean(X.test$Outstate))^2)
test.rss = 1 - gam.error / gam.tss
test.rss
test.r2 = 1 - gam.error / gam.tss
test.r2
gam.tss = mean((X.test$Outstate - mean(X.test$Outstate))^2)
test.r2 = 1 - gam.error / gam.tss
test.r2
# 78% of variance explained
# d)
summary(gam.fit)
# Expend -- strong non-linearity; Personal, PhD and Room.Board moderate
# 11
# a)
set.seed(1)
x1 = rnorm(100
)
x2 = rnorm(100)
eps = rnorm(100)
b1 = 1.5
b2 = 3.1
b0 = 1.1
y = b0 + b1*x1 + b2*x2 + eps
# b)
b.est1 = 50
# c)
a = y - b.est1 *x1
beta2 = lm(a~x2)$coef[2]
# d)
a = y - beta2 * x2
beta1= lm(a~x1)$coef[2]
# e)
for (i in 1:1000){
}
beta0 = rep(NA,1000)
beta1 = rep(NA,1000)
beta2 = rep(NA,1000)
beta1 = 50
for (i in 1:1000) {
a = y - beta1[i] * x1
beta2[i] = lm(a~x2)$coef[2]
a = y-beta2[i]*x2
beta1[i] = lm(a~x1)$coef[2]
]
for (i in 1:1000) {
a = y - beta1[i] * x1
beta2[i] = lm(a~x2)$coef[3]
a = y - beta2[i]*x2
beta1[1] = lm(a~x1)$coef[2]
beta1[i] = lm(a~x1)$coef[2]
a = y - beta2[i]*x2
lm.fit = lm(a~x1)
beta1[i] = lm.fit$coef[2]
beta0[i] = lm.fir$coef[1]
}
# ???