# Seting to code directory
setwd("~/Projects/Computational-Inteligence/R/src")

########################
##                    ##
##   Class Settings   ##
##                    ##
########################
#class name = perceptron, attributes X,W

setClass("perceptron", slots = list(x="vector", w="vector"))
perceptron <- new("perceptron", x=c(1), w=c(1))

########################
##                    ##
##  Global Variables  ##
##                    ##
########################
data <- c()
row <- c()
col <- c()
file <- c()
error <- 0
learningRate <- 0.1
########################
##                    ##
##   Weight Funtion   ##
##                    ##
########################
random_weights <- function(){
  return(runif(col,-1,1))
}

# examples Vector, weight Vector, learningRate Float, error Float
set_weight <- function(perceptron, current_row, learningRate, error){
  for (i in 1:col) {
    perceptron@w[c(i)] <- perceptron@w[c(i)] + (learningRate * error * perceptron@x[c((current_row - 1) * col + i)])
  }
  return(perceptron@w)
}

########################
##                    ##
##   Active Funtion   ##
##                    ##
########################

activation <- function(perceptron, current_row){
  active <- 0
  for(i in 1:col){
    active <- active + sum(perceptron@x[c((current_row - 1) * col + i)] * perceptron@w[c(i)])
  }
  if (active > 0){
    return (1)
  }else{
    return (0)
  }
}

########################
##                    ##
##  Training Funtion  ##
##                    ##
########################
training <- function(data, p, error){
  perceptron_output <- c()
  learning <- TRUE
  count <- 0
  iterationWrong <- data.frame(
    iteration = c(),
    absolute = c(),
    stringsAsFactors = FALSE
  )
  while(count < 10000 && learning){
    absoluteError <- 0
    count <- count + 1
    for(i in 1:row){
      perceptron_output[c(i)] <- activation(perceptron, i)
    }
    k <- which(data$class != perceptron_output)
    if(length(k) != 0){
      error <- data$class[c(k[c(1)])] - perceptron_output[c(k[c(1)])]
      perceptron@w <- set_weight(perceptron, k[c(1)], learningRate, error)
    }else{
      learning = FALSE
    }
    for(l in 1:length(k)){
      error <- data$class[c(k[c(l)])] - perceptron_output[c(k[c(l)])]
      absoluteError <- absoluteError + abs(error)
      print(k[c(l)])
    }
    
    if(count == 1){
      plot(aa, type = "o", col = collor[c(i)], xlab = "Iteration", ylab = "error", main = "Learning")
    }else{
      lines(aa, type = "o", col = collor[c(i)])
    }
  }
  cat("Epochs: ", count, "\n")
  return(perceptron@w)
}

########################
##                    ##
##   Read Funtions    ##
##                    ##
########################

archive_read <- function(file){
  data <- read.csv(file)
  # setwd("~/Projects/Computational-Inteligence/R/src")
  return(data)
}

set_col <- function(data){
  return(ncol(data))
}

set_row <- function(data){
  return(nrow(data))
}
#x1 x2 bias
convert_to_vector <- function(data, row, col){
  for(i in 1:row) {
    perceptron@x[c(((i - 1) * col) + 1)] <- 1 #bias
    for(j in 2:col) {
      perceptron@x[c(((i - 1) * col) + j)] <- data[i, j-1] #number of csv
    }
  }
  return(perceptron@x)
}

# the archives that will be read in perceptron
archive_name <- function(){
  
  cat("Input a number between 1-2!\n1 - logical ports \n2 - numbers")
  switch(readline(),
    "1"={
      setwd("~/Projects/Computational-Inteligence/R/csv")
      file <- list.files(pattern="*.csv")
    },
    "2"={
      setwd("~/Projects/Computational-Inteligence/R/csv/numbers")
      file <- list.files(pattern="*.csv")
    },
    stop("You do not write a valid digit!")
  )
  return(file)
}

collor <- c("red", "blue", "black", "yellow", "green", "purple", "pink", "orange", "gray", "brown")

main <- function(data, row, col, perceptron, error){
  file <<- archive_name()
  for(i in 1:length(file)){
    data <<- archive_read(file[c(i)])
    row <<- set_row(data)
    col <<- set_col(data)
    perceptron@x <<- convert_to_vector(data, row, col)
    perceptron@w <<- random_weights()
    w <<- training(data, perceptron, error)
    write.csv(w, paste("~/Projects/Computational-Inteligence/R/weights/", file[c(i)], sep = "", collapse = ""), row.names = TRUE)
2
  }
}

main(data, row, col, perceptron, error)
