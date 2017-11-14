library("RSNNS")
library("RWeka")
# demo(iris)
# 
# data("iris")
# df <- iris[sample(nrow(iris)),]
# # df <- iris[sample(nrow(iris)),]
# dfValues <- df[,1:4]
# dfTargets <- decodeClassLabels(df[,5])
# #dfTargets <- decodeClassLabels(df[,5], valTrue=0.9, valFalse=0.1)
# df <- splitForTrainingAndTest(dfValues, dfTargets, ratio=0.15)
# #normalizedata
# df <- normTrainingAndTestSet(df)
# model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1), maxit=50, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
#model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="BackpropBatch", learnFuncParams=c(10, 0.1), maxit=100, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
#model <- mlp(df$inputsTrain, df$targetsTrain, size=5, learnFunc="SCG", learnFuncParams=c(0, 0, 0, 0), maxit=30, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
#model <- rbfDDA(df$inputsTrain, df$targetsTrain)
#model <- elman(df$inputsTrain, df$targetsTrain, size=5, learnFuncParams=c(0.1), maxit=100, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
#model <- rbf(df$inputsTrain, df$targetsTrain, size=40, maxit=200, initFuncParams=c(-4, 4,  0.0,  0.2,  0.04), learnFuncParams=c(1e-3, 0, 1e-3, 0.1, 0.8), linOut=FALSE)
#model <- rbf(df$inputsTrain, df$targetsTrain, size=40, maxit=600, initFuncParams=c(0, 1,  0.0,  0.2,  0.04), learnFuncParams=c(1e-5, 0, 1e-5, 0.1, 0.8), linOut=TRUE)
##experimental..:
##model <- rbf(df$inputsTrain, df$targetsTrain, size=20, maxit=50, initFunc="RBF_Weights_Kohonen", initFuncParams=c(50,  0.4,  0), learnFuncParams=c(0.01, 0, 0.01, 0.1, 0.8))
#summary(model)
# par(mfrow=c(2,2))
# plotIterativeError(model)
#######################################################
#######################################################
#######################################################
setwd("~/Projects/Computational-Inteligence/R")
numbers <- read.csv("src/saida.csv")
set.seed(2)
df <- numbers[sample(nrow(numbers)),]
dfValues <- df[,1:(ncol(numbers) - 1)]
dfTargets <- decodeClassLabels(df[,ncol(numbers)])
df <- splitForTrainingAndTest(dfValues, dfTargets, ratio=0.15)
df <- normTrainingAndTestSet(df)
model <- mlp(df$inputsTrain, df$targetsTrain, size=36, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1), maxit=50, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
par(mfrow=c(2,2))
predictions <- predict(model, df$inputsTest)
plotIterativeError(model)
# plotRegressionError(predictions[,2], df$targetsTest[,2])
# confusionMatrix(df$targetsTrain,fitted.values(model))
# confusionMatrix(df$targetsTest,predictions)
# plotROC(fitted.values(model)[,2], df$targetsTrain[,2])
# plotROC(predictions[,2], df$targetsTest[,2])
# confusion matrix with 402040-method
confusionMatrix(df$targetsTrain, encodeClassLabels(fitted.values(model), method="402040", l=0.4, h=0.6))
