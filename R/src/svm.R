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

# compute decision values and probabilities:
# pred <- predict(model, x, decision.values = TRUE)
# attr(pred, "decision.values")[1:col]

# visualize (classes by color, SV by crosses):

# plot(cmdscale(dist(archive[,-col])), col = as.integer(archive[,col]), pch = c("-","*")[1:150 %in% model$index + 1])

# ## try regression mode on two dimensions
# 
# # create data
# x <- seq(0.1, 5, by = 0.05)
# y <- log(x) + rnorm(x, sd = 0.2)
# 
# # estimate model and predict input values
# m   <- svm(x, y)
# new <- predict(m, x)
# 
# # visualize
# plot(x, y)
# points(x, log(x), col = 2)
# points(x, new, col = 4)
# 
# ## density-estimation
# 
# # create 2-dim. normal with rho=0:
# X <- data.frame(a = rnorm(1000), b = rnorm(1000))
# attach(X)
# 
# # traditional way:
# m <- svm(X, gamma = 0.1)
# 
# # formula interface:
# m <- svm(~., data = X, gamma = 0.1)
# # or:
# m <- svm(~ a + b, gamma = 0.1)
# 
# # test:
# newdata <- data.frame(a = c(0, 4), b = c(0, 4))
# predict (m, newdata)
# 
# # visualize:
# plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
# points(newdata, pch = "+", col = 2, cex = 5)
# 
# # weights: (example not particularly sensible)
# i2 <- archive
# levels(i2$Number)[3] <- 2
# summary(i2$Number)
# wts <- 100 / table(i2$Number)
# wts
# m <- svm(Number ~ ., data = i2, class.weights = wts)