#install library yang dibutuhkan --> packages
install.packages("neuralnet")
install.packages("dplyr")
install.packages("caret")
install.packages("shiny")

library(neuralnet)
library(dplyr)
library(caret)
library(shiny)

#import data
Klasifikasi_Penguin$island <- as.numeric(factor(Klasifikasi_Penguin$island ))
Klasifikasi_Penguin$species <- ifelse(Klasifikasi_Penguin$species == "Adelie", 0, 1)

#Transformasi Data -> normalisasi
scl <- function(x){(x-min(x))/(max(x)-min(x))}
sclPenguin <- data.frame(lapply(Klasifikasi_Penguin[,1:6],scl))

#split data
set.seed(123)
nTest <- sample(1:nrow(Klasifikasi_Penguin), nrow(Klasifikasi_Penguin)*0.3)
trainPenguin <- cbind(sclPenguin[-nTest,], species = Klasifikasi_Penguin$species[-nTest])
testPenguin  <- cbind(sclPenguin[nTest,],  species = Klasifikasi_Penguin$species[nTest])

#Pemodelan
NNPenguin = neuralnet(species~island+bill_length_mm+bill_depth_mm+flipper_length_mm+body_mass_g,
                      data = trainPenguin, hidden = c(6,7), act.fct = "logistic", linear.output = FALSE)
plot(NNPenguin)
weights(NNPenguin)

#Prediksi dan Evaluasi
ptest <- predict(NNPenguin, testPenguin[,-7])
prediksi <- ifelse(ptest > 0.5, "Gentoo", "Adelie")
true_labels <- ifelse(testPenguin$species == 1, "Gentoo", "Adelie")
confusionMatrix(factor(prediksi, levels = c("Adelie", "Gentoo")),
                factor(true_labels, levels = c("Adelie", "Gentoo")))

# Tampilan Antar Muka
# Data Training
scl <- function(x, min_x, max_x) {
  (x - min_x) / (max_x - min_x)
}

minmax_vals <- list(
  island = c(1, 3),
  bill_length_mm = c(32, 60),
  bill_depth_mm = c(13, 21),
  flipper_length_mm = c(170, 230),
  body_mass_g = c(2700, 6300),
  year = c(2007, 2009)
)

ui <- fluidPage(
  titlePanel("Prediksi Spesies Penguin: Adelie vs Gentoo (ANN)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("island", "Island (1=Dream, 2=Biscoe, 3=Torgersen):", choices = c(1, 2, 3)),
      numericInput("bill_length", "Bill Length (mm):", 40, min = 30, max = 60),
      numericInput("bill_depth", "Bill Depth (mm):", 17, min = 13, max = 21),
      numericInput("flipper_length", "Flipper Length (mm):", 200, min = 170, max = 230),
      numericInput("body_mass", "Body Mass (g):", 4000, min = 2700, max = 6300),
      numericInput("year", "Year:", 2009, min = 2007, max = 2009),
      actionButton("predictBtn", "Prediksi")
    ),
    
    mainPanel(
      verbatimTextOutput("result"),
      plotOutput("networkPlot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predictBtn, {
    
    data_norm <- data.frame(
      island = scl(as.numeric(input$island), minmax_vals$island[1], minmax_vals$island[2]),
      bill_length_mm = scl(input$bill_length, minmax_vals$bill_length_mm[1], minmax_vals$bill_length_mm[2]),
      bill_depth_mm = scl(input$bill_depth, minmax_vals$bill_depth_mm[1], minmax_vals$bill_depth_mm[2]),
      flipper_length_mm = scl(input$flipper_length, minmax_vals$flipper_length_mm[1], minmax_vals$flipper_length_mm[2]),
      body_mass_g = scl(input$body_mass, minmax_vals$body_mass_g[1], minmax_vals$body_mass_g[2]),
      year = scl(input$year, minmax_vals$year[1], minmax_vals$year[2])
    )
    pred <- predict(NNPenguin, data_norm)
    label <- ifelse(pred > 0.5, "Gentoo", "Adelie")
    
    output$result <- renderPrint({
      cat("???? Hasil Prediksi:\n")
      cat("Spesies =", label, "\n")
      cat("Probabilitas Gentoo =", round(pred, 4))
    })
  })
  
  output$networkPlot <- renderPlot({
    plot(NNPenguin)
  })
}
shinyApp(ui = ui, server = server)  
