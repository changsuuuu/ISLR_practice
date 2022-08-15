
## The Stock Market Data
# install.packages("MASS")
# install.packages("ISLR")
# install.packages("car")
# install.packages("ISLR2")
###
library(ISLR2)
# names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
###
cor(Smarket)
cor(Smarket[, -9])
###
attach(Smarket)
plot(Volume)

## Logistic Regression

###
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial
  )
summary(glm.fits)
# lag1이 가장 작은 p value를 갖는다. 원래 같으면 전 날과 반대로 방향이 예측될텐데,
# pvalue가 높으므로 명확한 근거가 될 수 없다.

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]
###
glm.probs <- predict(glm.fits, type = "response")
# type = 'response' >> P(Y=1|X)를 계산
glm.probs[1:10]
contrasts(Direction)
###
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
###
table(glm.pred, Direction) # confusion matrix 생성
(507 + 145) / 1250
mean(glm.pred == Direction)
###
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]
###
glm.fits <- glm(
    Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    data = Smarket, family = binomial, subset = train
  )
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")
###
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)
###
# 가장 낮은 p value 들로 돌린 것
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket,
    family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005,
    type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106 + 76)
###
predict(glm.fits,
    newdata =
      data.frame(Lag1 = c(1.2, 1.5),  Lag2 = c(1.1, -0.8)),
    type = "response"
  )

## Linear Discriminant Analysis

###
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
lda.fit
plot(lda.fit)
###
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
###
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
###
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
###
lda.pred$posterior[1:20, 1]
lda.class[1:20]
###
sum(lda.pred$posterior[, 1] > .9)

## Quadratic Discriminant Analysis

###
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
qda.fit
###
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

## Naive Bayes

###
# install.packages('e1071')
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket,
    subset = train)
nb.fit
###
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])
###
nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005)
###
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]
## $K$-Nearest Neighbors
# 4가지의 input이 필요함
# 1. train.x 라고 이름 붙은 training data와 관련된 예측변수들을 포함한 행렬
# 2. test.x 라고 붙은 예측하고 싶은 변수와 관련된 행렬
# 3. train.Direction 이라고 붙은 training 관측치의 class label을 포함한 벡터
# 4. K 의 값 
###
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- Direction[train]
###
set.seed(1) # We set a random seed before we apply knn() 
# because if several observations are tied as nearest neighbors,
# then R will randomly break the tie. Therefore, a seed must be set 
# in order to ensure reproducibility of results.
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83 + 43) / 252
###
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
###
dim(Caravan) # p 가 매우 큰 데이터
attach(Caravan)
summary(Purchase)
348 / 5822
# knn은 데이터의 scale이 매우 중요하다. 스케일이 큰 변수는 관측치들 간에 거리에 크게
# 영향을 줄 것 이기 때문. 단위를 없애는 측면에서도 중요함
standardized.X <- scale(Caravan[, -86]) # 86은 질적변수이므로 스케일링 제외
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])
###
test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
###
table(knn.pred, test.Y)
9 / (68 + 9)
###
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5 / 26
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4 / 15
###
glm.fits <- glm(Purchase ~ ., data = Caravan,
    family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test, ],
    type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred, test.Y)
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred, test.Y)
11 / (22 + 11)

## Poisson Regression

###
attach(Bikeshare)
dim(Bikeshare)
# names(Bikeshare)
### 먼저 lm부터 해보면,
mod.lm <- lm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare
  )
summary(mod.lm)
###
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod.lm2 <- lm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare
  )
summary(mod.lm2) # 이렇게 코딩한 것과 이전 코딩의 차이는, 마지막 수준의 결과가 없다?
# This means that the coefficients of hr and mnth in mod.lm2 will always
# sum to zero, and can be interpreted as the difference from the mean
# level.
###
sum((predict(mod.lm) - predict(mod.lm2))^2) # 합이 0이라는 의미
###
all.equal(predict(mod.lm), predict(mod.lm2))
###
coef.months <- c(coef(mod.lm2)[2:12],
    -sum(coef(mod.lm2)[2:12]))
###
plot(coef.months, xlab = "Month", ylab = "Coefficient",
    xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A",
    "M", "J", "J", "A", "S", "O", "N", "D"))
###
coef.hours <- c(coef(mod.lm2)[13:35],
    -sum(coef(mod.lm2)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
    col = "blue", pch = 19, type = "o")
###
mod.pois <- glm(
    bikers ~ mnth + hr + workingday + temp + weathersit,
    data = Bikeshare, family = poisson
  )
summary(mod.pois)
###
coef.mnth <- c(coef(mod.pois)[2:12],
    -sum(coef(mod.pois)[2:12]))
plot(coef.mnth, xlab = "Month", ylab = "Coefficient",
     xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35],
     -sum(coef(mod.pois)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient",
    col = "blue", pch = 19, type = "o")
###
plot(predict(mod.lm2), predict(mod.pois, type = "response"))
abline(0, 1, col = 2, lwd = 3)
###