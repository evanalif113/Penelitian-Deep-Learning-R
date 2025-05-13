#install library yang dibutuhkan --> packages
install.packages("neuralnet")
install.packages("dplyr")
install.packages("caret")
install.packages("shiny")

library(neuralnet)
library(dplyr)
library(caret)
library(shiny)
iris
data_iris <- iris
data_iris$Species <- as.numeric(factor(data_iris$Species ))

#Transformasi_data
scl <- function(x){(x-min(x))/(max(x)-min(x))}
scl_iris <- data.frame(lapply(data_iris[,1:5],scl))

#split_data
set.seed(456)
nTest <- sample(1:nrow(data_iris), nrow(data_iris)*0.3)
trainIris <- cbind(scl_iris[-nTest,], Species = data_iris$Species[-nTest])
testIris  <- cbind(scl_iris[nTest,],  Species = data_iris$Species[nTest])

#Pemodelan
NNIris = neuralnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
                      data = trainIris,
                      hidden = c(6,7,5), 
                      learningrate = 0.03,
                      act.fct = "tanh", 
                      linear.output = FALSE)
plot(NNIris)
weights(NNIris)
NNIris$result.matrix["error",]
