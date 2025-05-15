---
  title: 

---
  

{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)




Caricamento delle librerie necessarie

library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

# Caricamento del dataset
data <- read.csv("Dataset/ParisHousingClass.csv") 

# Visualizzazione delle prime righe del dataset
head(data)


# Descrizione statistica
summary(data)

# Matrice di correlazione
correlation_matrix <- cor(data %>% select(-category))
correlation_matrix

# Visualizzazione della matrice di correlazione
library(corrplot)
corrplot(correlation_matrix, method = "circle")

# Boxplot per visualizzare la distribuzione dei metri quadri tra le categorie
ggplot(data, aes(x = category, y = squareMeters)) + 
  geom_boxplot() + 
  ggtitle('Distribuzione dei metri quadri per categoria')

# Boxplot per visualizzare la distribuzione del numero di stanze tra le categorie
ggplot(data, aes(x = category, y = numberOfRooms)) + 
  geom_boxplot() + 
  ggtitle('Distribuzione del numero di stanze per categoria')

# Boxplot per visualizzare la distribuzione del prezzo tra le categorie
ggplot(data, aes(x = category, y = price)) + 
  geom_boxplot() + 
  ggtitle('Distribuzione del prezzo per categoria')



# Preparazione dei dati
data$category <- ifelse(data$category == "Luxury", 1, 0)

# Divisione dei dati in training e test set
set.seed(123)
trainIndex <- createDataPartition(data$category, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

# Creazione del modello
model <- rpart(category ~ ., data = dataTrain, method = "class")

# Visualizzazione dell'albero decisionale
rpart.plot(model)

# Predizione e valutazione
predictions <- predict(model, dataTest, type = "class")
confusionMatrix(as.factor(predictions), as.factor(dataTest$category))

