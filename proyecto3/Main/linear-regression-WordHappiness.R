# Clear plots
if(!is.null(dev.list())) dev.off() #borra panel de graficos
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(caret)
library(ggplot2)
library(RKEEL)

# Read data from CSV: 
data <- read.csv(file=("../data/2020 - 2019 world happiness.csv"), sep=",", header = TRUE)
# Remove non-numerical columns of the data
data$Country.or.region = NULL
# % de ejemplos para entrenar
training_p <- 0.80

#Valores actualizables a cada iteracion del bucle
count <- 1
mean_avg_error_Todos <- 0
mean_avg_error_GPD <- 0
mean_avg_error_Social <- 0
mean_avg_error_Healthy <- 0
mean_avg_error_Freedom <- 0
mean_avg_error_Generosity <- 0
mean_avg_error_Corruption <- 0



while(count<10){
  #Crea una vector de indices con el 80% de los ejemplos : 
  training_samples <- createDataPartition(y = data$Score, p = training_p, list = FALSE)
  # Split training and test data:
  training_data    <- data[training_samples, ] #Tabla de ejemplos de entrenar
  test_data        <- data[-training_samples, ] #Tabla de ejemplos de test, el menos es aquellos que no tiene el vector
  # Formula modelo lineal: formula-> columna a aprender~., data-> ejemplos que se utilizan para generar el modelo
  #Modelo 1: Modelo de los 6 atributos
  model_Todos      <- lm(formula = Score ~., data = training_data)
  #Modelo 2: GPD per capita
  model_GPD            <- lm(formula = Score ~`GDP.per.capita`, data = training_data)
  #Modelo 3: Social support
  model_Social            <- lm(formula = Score ~`Social.support`, data = training_data) 
  #Modelo 4: Healthy life expectancy
  model_Healthy           <- lm(formula = Score ~`Healthy.life.expectancy`, data = training_data) 
  #Modelo 5: Freedom to make life choices
  model_Freedom           <- lm(formula = Score ~`Freedom.to.make.life.choices`, data = training_data) 
  #Modelo 6: Generosity
  model_Generosity         <- lm(formula = Score ~Generosity, data = training_data) 
  #Modelo 7: Perceptions of corruption
  model_Corruption         <- lm(formula = Score ~`Perceptions.of.corruption`, data = training_data) 
  
  #Valida el modelo lineal comparandolo con los test:
  
  #Modelo 1: Modelo de los 6 atributos
  prediction_Todos       <- predict(model_Todos, test_data)
  #Modelo 2: GPD per capita
  prediction_GPD       <- predict(model_GPD, test_data)
  #Modelo 3: Social support
  prediction_Social      <- predict(model_Social, test_data)
  #Modelo 4: Healthy life expectancy
  prediction_Healthy     <- predict(model_Healthy, test_data)
  #Modelo 5: Freedom to make life choices
  prediction_Freedom     <- predict(model_Freedom, test_data)
  #Modelo 6: Generosity
  prediction_Generosity     <- predict(model_Generosity, test_data)
  #Modelo 7: Perceptions of corruption
  prediction_Corruption   <- predict(model_Corruption, test_data)
  
  
  #Calcula la media del valor absoluto de la resta del atributo generado en el modelo lineal (prediction) con el valor de la data 

  mean_avg_error_Todos <- ((mean(abs(prediction_Todos - test_data$Score))) + mean_avg_error_Todos) / 2
  mean_avg_error_GPD   <- ((mean(abs(prediction_GPD - test_data$Score))) + mean_avg_error_GPD) / 2
  mean_avg_error_Social  <- ((mean(abs(prediction_Social - test_data$Score))) + mean_avg_error_Social) / 2
  mean_avg_error_Healthy   <- ((mean(abs(prediction_Healthy - test_data$Score))) + mean_avg_error_Healthy) / 2
  mean_avg_error_Freedom   <- ((mean(abs(prediction_Freedom - test_data$Score))) + mean_avg_error_Freedom) / 2
  mean_avg_error_Generosity   <- ((mean(abs(prediction_Generosity - test_data$Score))) + mean_avg_error_Generosity) / 2
  mean_avg_error_Corruption   <- ((mean(abs(prediction_Corruption - test_data$Score))) + mean_avg_error_Corruption) / 2
  

  
  count <- count + 1
  
}

#imprime valor
print(paste0("- Mean average error Todos: ", mean_avg_error_Todos)) 
print(paste0("- Mean average error GPD: ", mean_avg_error_GPD)) 
print(paste0("- Mean average error Social: ", mean_avg_error_Social)) 
print(paste0("- Mean average error Healthy: ", mean_avg_error_Healthy)) 
print(paste0("- Mean average error Freedom: ", mean_avg_error_Freedom)) 
print(paste0("- Mean average error Generosity: ", mean_avg_error_Generosity)) 
print(paste0("- Mean average error Corruption: ", mean_avg_error_Corruption)) 

# Print model summary
print(paste0("--------- Summary TODOS -------------")) 
summary(model_Todos) #Devuelve valores de la regresion lineal para determinar la calidad
print(paste0("--------- Summary GPD -------------")) 
summary(model_GPD) #Devuelve valores de la regresion lineal para determinar la calidad
print(paste0("--------- Summary Social -------------")) 
summary(model_Social) #Devuelve valores de la regresion lineal para determinar la calidad
print(paste0("--------- Summary Healthy -------------")) 
summary(model_Healthy) #Devuelve valores de la regresion lineal para determinar la calidad
print(paste0("--------- Summary Freedom -------------")) 
summary(model_Freedom) #Devuelve valores de la regresion lineal para determinar la calidad
print(paste0("--------- Summary Generosity -------------")) 
summary(model_Generosity) #Devuelve valores de la regresion lineal para determinar la calidad
print(paste0("--------- Summary Perception of corruption -------------")) 
summary(model_Corruption) #Devuelve valores de la regresion lineal para determinar la calidad

# Plot model: Crea graficos
par(mfrow = c(2,2))
plot(model_Todos, main = "Grafico modelo completo")
plot(model_GPD,main = "Grafico modelo PIB per Capita")
plot(model_Social,main = "Grafico modelo Apoyo Social")
plot(model_Healthy,main = "Grafico modelo Esperanza de vida sana")
plot(model_Freedom,main = "Grafico modelo Libertad de eleccion de vida")
plot(model_Generosity,main = "Grafico modelo Generosidad")
plot(model_Corruption,main = "Grafico modelo Percepcion de la corrupcion")

