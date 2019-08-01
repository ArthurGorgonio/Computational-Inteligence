library("RSNNS")
# library("RWeka")
# library("rJava")
setwd("~/Projects/Computational-Inteligence/R")
numbers <- read.csv("data/numbers/numbers2.csv")
set.seed(2)
df <- numbers[sample(nrow(numbers)),]
dfValues <- df[,1:(ncol(numbers) - 1)]
dfTargets <- decodeClassLabels(df[,ncol(numbers)])
df <- splitForTrainingAndTest(dfValues, dfTargets, ratio=0.15)
df <- normTrainingAndTestSet(df)
model <- mlp(df$inputsTrain, df$targetsTrain, size = 35, maxit=50, inputsTest=df$inputsTest, targetsTest=df$targetsTest)
predictions <- predict(model, df$inputsTest)
plotIterativeError(model)
# plotRegressionError(predictions[,2], df$targetsTest[,2])
# confusionMatrix(df$targetsTrain,fitted.values(model))
# confusionMatrix(df$targetsTest,predictions)
# plotROC(fitted.values(model)[,2], df$targetsTrain[,2])
# plotROC(predictions[,2], df$targetsTest[,2])
# confusion matrix with 402040-method
confusionMatrix(df$targetsTrain, encodeClassLabels(fitted.values(model), method="402040", l=0.4, h=0.6))

