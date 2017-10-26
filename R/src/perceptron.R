
## X -> (vector) Examperceptronles
## W -> (vector) Weights
## error -> perceptronOutput - IdealOutperceptronut

# Seting to code directory
setwd("~/Projects/Computational-Inteligence/R/src")

########################
##                    ##
##   Class Settings   ##
##                    ##
########################
#class name = perceptron, attributes X,W
perceptron <- c()

setClass("perceptron", slots = list(x="vector", w="vector"))
perceptron <- new("perceptron", x=c(1), w=c(1))

########################
##                    ##
##   Global Settings  ##
##                    ##
########################
# archive to learning
data <- c()

# not mutable variables
col <- c()
row <- c()

# #vectors for weight and the examples
# w <- c()
# x <- c()

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
  while(count < 10000 && learning){
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
  }
  cat("Epochs: ", count, "\n")
  return(perceptron@w)
}

########################
##                    ##
##    Read Funtion    ##
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
  }
}

main(data, row, col, perceptron, error)
