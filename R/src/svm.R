library("e1071")
library("rminer")
setwd("~/Projects/Computational-Inteligence/R")

archive <- read.csv("data/numbers/numbers2.csv")
# test <- read.csv("data/numbers/numbers.csv")
# row <- 1:nrow(archive)
attach(archive)

inputsForTrain <- subset(archive, select = -Number)
test <- archive$Number

model <- svm(inputsForTrain, test, type = "C-classification", kernel = "sigmoid")
summary(model)

pred <- predict(model, inputsForTrain)
pred <- fitted(model)
table(pred, test)

plot(inputsForTrain, col = 1:10 %in% model$index + 1, xlim = c(-5,5), ylim=c(-5,5))
plot(inputsForTrain, test)
confue <- table(pred, test)
print(model)
summary(model)

library("e1071")
library("rminer")
setwd("~/Projects/Computational-Inteligence/R")

archive <- read.csv("data/numbers/numbers2.csv")
# test <- read.csv("data/numbers/numbers.csv")
# row <- 1:nrow(archive)

#model <- svm(Number ~ ., data = archive)

#split
H <- holdout(archive$Number, ratio = 0.75, mode="stratified") 
training <- archive[H$tr,] 
test <- archive[H$ts,]

# alternatively the traditional interface:
xTraining <- subset(training, select = -Number)
yTraining <- subset(training, select = Number)

xTest <- subset(test, select = -Number)
yTest <- subset(test, select = Number)

model <- svm(xTraining, yTraining, type = "C-classification", kernel = "radial") 

print(model)
summary(model)

# test with train data
pred <- predict(model, xTest)

# Check accuracy:
matrix <- table(pred, yTest)
summary(pred)

plot(model$labels, summary(pred), xlim = c(0,10), xlab = "Numbers", ylab = "Classifications")

#accuracy
acc <- ((sum(diag(matrix)) / sum(matrix)) * 100)

#col
col <- ncol(archive)
col <- col - 1

model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)
