library(neuralnet)
library(dplyr)
library(caret)
library(shiny)

library(readxl)
X2024 <- read_excel("D:/Data Iklim/2024.xlsx", 
                    col_types = c("skip", "skip", "numeric", 
                                  "numeric", "numeric", "skip", "skip", 
                                  "skip", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "text", "skip", 
                                  "skip", "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "skip", "skip", "skip", 
                                  "text", "text", "text", "skip"))


prediksi <- X2024
jumlah_nan <- sum(is.na(prediksi))
print(paste("Jumlah NaN dalam dataframe:", jumlah_nan))
prediksi <- na.omit(prediksi)
prediksi$precipprob <- as.numeric(factor(prediksi$precipprob))
prediksi$icon <- as.numeric(factor(prediksi$icon))
prediksi$description <- as.numeric(factor(prediksi$description))
prediksi$conditions <- as.numeric(factor(prediksi$conditions))
prediksi <- subset(prediksi, select = -preciptype)

#transformasi_data
ncol(prediksi)
scl <- function(x){(x-min(x))/(max(x)-min(x))}
scl_cuaca <- data.frame(lapply(prediksi[,1:ncol(prediksi)],scl))

#split_data
set.seed(456)
nTest <- sample(1:nrow(prediksi), nrow(prediksi)*0.3)
trainCuaca <- cbind(scl_cuaca[-nTest,], Probability = prediksi$precipprob[-nTest])
testCuaca  <- cbind(scl_cuaca[nTest,],  Probability = prediksi$precipprob[nTest])

#Pemodelan
NNCuaca <- neuralnet(Probability ~ temp + tempmax + tempmin + humidity + windspeed + sealevelpressure,
                      data = trainCuaca,
                      hidden = c(6, 12, 4, 4,2), 
                      learningrate = 0.01,
                      act.fct = "logistic", 
                      linear.output = TRUE)


plot(NNCuaca)
weights(NNCuaca)
summary(NNCuaca)
print(NNCuaca$result.matrix["error",])

# Menggunakan model NNCuaca untuk membuat prediksi
pred <- predict(NNCuaca, testCuaca[, c("temp", "tempmax", "tempmin", "humidity", "windspeed", "sealevelpressure")])

# Hasil prediksi
predicted_values <- pred

# Menampilkan hasil prediksi
print("Hasil Prediksi:")
print(predicted_values)

# Jika ingin membandingkan dengan nilai aktual
actual_values <- testCuaca$Probability
comparison <- data.frame(Actual = actual_values, Predicted = predicted_values)
print("Perbandingan Nilai Aktual dan Prediksi:")
print(comparison)
