

# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben 3
# Autor:  Dominik Peter
# Date:   2017-03-28
# ------------------------------------------------------------------------------------------------

rm(list = ls())
set.seed(2323)

library(dplyr)
library(caret)
library(readr)
library(scales)
library(parallelSVM)
library(ranger)
library(ggplot2)


# Aufgabe 3 - Classification
# ------------------------------------------------------------------------------------------------

## Das MNIST Datenset ist sozusagen das HelloWorld Program wenn es um Classification mit DeepLearning
## geht. Wir missbrauchen hier dieses Datenset für traditionelle Classification.
## 1. Führen Sie u.g. Source Code aus und beantworten Sie folgende Fragen
##a) Wieviele Bilder sind in der Matrix mnist_matrix encodiert
##b) Da es sich um einen Supervised Machine Learning task handelt muss ein Label (Target Variable)
##bereitgestellt sein - welche Spalte der Matrix enthält das Label?
##c) Wieviele Pixel haben die Bilder?
##d) Wie hoch/breit sind die Bilder?


mnist_matrix = read.csv( 'https://github.com/romeokienzler/developerWorks/raw/master/train.csv' )
dim(mnist_matrix)
sort(unique(mnist_matrix[,1]))

par( mfrow = c(10,10), mai = c(0,0,0,0))
for(i in 1:100){
  y = as.matrix(mnist_matrix[i, 2:785])
  dim(y) = c(28, 28)
  image( y[,nrow(y):1], axes = FALSE, col = gray(255:0 / 255))
  text( 0.2, 0, mnist_matrix[i,1], cex = 3, col = 2, pos = c(3,4))
}

# ----------------------------------------------------------

df <- mnist_matrix %>% as_data_frame()

# a)
# ------------------------
# Wieviele Bilder sind in der Matrix mnist_matrix encodiert
nrow(df)
# [1] 42000
# 42'000 Bilder


# b)
# -----------------------------------------------
# Da es sich um einen Supervised Machine Learning task handelt muss ein Label (Target Variable)
# bereitgestellt sein - welche Spalte der Matrix enthält das Label?
which(!grepl("pixel", colnames(df)))
# [1] 1
which(grepl("label", colnames(df)))
# [1] 1
# Die erste Spalte namens "label" hat das Label

# c) 
# -----------------------------------------------
# Wieviele Pixel haben die Bilder?
sum(grepl("pixel", colnames(df)))
# [1] 784
# die Bilder haben 784 Pixel

# d)
# -----------------------------------------------
# Wie hoch/breit sind die Bilder?
sum(grepl("pixel", colnames(df))) %>% sqrt()
# [1] 28
# Die Bilder sind 28 Pixel hoch sowie breit


# -----------------------------------------------------------------------------------------------------
# 2. Nehmen Sie einen Classifier Ihrer Wahl und trainieren Sie Ihn mit der bereitgestellten Matrix.
# a) Teilen Sie die Matrix in ein sinnvolles Training und Test set auf, lesen Sie hierzu diesen
# Thread: http://stats.stackexchange.com/questions/19048/what-is-the-difference-between-test-set-and-validation-set
# (Ein Validation Set wird hier nicht benötigt da nicht erwartet wird Parameter des
# Classifiers zu tunen)
# b) Verwenden Sie nun das Training Set um einen Classifier Ihrer Wahl zu trainieren
# c) Berechnen Sie den Prozentsatz der richtig klassifizierten Daten indem Sie Ihren 
# trainierten Classifier auf das Test Set anwenden (Hinweis: Die Qualität des Classifiers
# wird nicht bewertet)

df$label <- as.factor(df$label)

# Split into Train and Test Set
index <- createDataPartition(df$label, times=1, p=.75,list=FALSE)

train_set <- df[index, ]
test_set <- df[-index, ]


# Löschen von Kolonnen mit keinerlei Varianz (alle Wert 0)
# Zeros nur anhand des Trainsets definieren um Bias beim Testen zu vermeiden
zeros <- sapply(train_set[,-1], function(x) all(x == 0))

train_set <- train_set[, c(TRUE, !zeros)]
test_set <- test_set[, c(TRUE, !zeros)]

#Skalieren der Werte
train_set[,-1] <- lapply(train_set[,-1], scale)
test_set[,-1] <- lapply(test_set[,-1], scale)


# Support Vector Machine
# -----------------------------------------------------------------------------------------------------
start <- Sys.time()

model <- parallelSVM(label~., data=train_set, numberCores=10) # Train with 10 Cores

pred <- model %>%
  predict(test_set[,2:ncol(test_set)])

Sys.time()-start
# Time difference of 12.49559 mins
# MacBook Pro (Retina, 13-inch, Late 2013)
# Prozessor  2.8 GHz Intel Core i7
# Speicher 16 GB 1600 MHz DDR3  

confusionMatrix(table(pred, test_set$label))
# Accuracy : 0.9096         
# 95% CI : (0.9039, 0.915)





# Random Forest
# -----------------------------------------------------------------------------------------------------
start <- Sys.time()

model_rf <- ranger(label~., data=train_set, num.trees=250)

pred <- model_rf %>%
  predict(test_set[,2:ncol(test_set)])

Sys.time()-start
# Time difference of 44.8462 secs


confusionMatrix(table(pred$predictions, test_set$label))
# Accuracy : 0.9622          
# 95% CI : (0.9584, 0.9657)



# example with python and sklearn
# https://htmlpreview.github.io/?https://github.com/dominikpeter/homework/blob/master/mining/Uebung3/archiv/mnist.html




<