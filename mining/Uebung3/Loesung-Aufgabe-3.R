

# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben 3
# Autor:  Dominik Peter
# Date:   2017-03-28
# ------------------------------------------------------------------------------------------------

rm(list = ls())
set.seed(2323)


if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("scales")) install.packages("scales")
if (!require("parallelSVM")) install.packages("parallelSVM")
if (!require("ranger")) install.packages("ranger")


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
# Die erste Spalte namens "label" hat das Label, Spalte 1

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
# Die Bilder sind 28 Pixel hoch sowie breit (28x28)


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
# Wahrscheinlich werden die Algos auch schneller mit weniger Variablen
zeros <- sapply(train_set[,-1], function(x) all(x == 0))
train_set <- train_set[, c(TRUE, !zeros)]
test_set <- test_set[, c(TRUE, !zeros)]

#Skalieren der Werte
train_set[,-1] <- lapply(train_set[,-1], rescale)
test_set[,-1] <- lapply(test_set[,-1], rescale)


# Support Vector Machine
# -----------------------------------------------------------------------------------------------------
start <- Sys.time()
model <- parallelSVM(label~., data=train_set, numberCores=10) # Train with 10 Cores
pred <- model %>%
  predict(test_set[,2:ncol(test_set)])
Sys.time()-start
# Time difference of 10.75163 mins
# MacBook Pro (Retina, 13-inch, Late 2013)
# Prozessor  2.8 GHz Intel Core i7
# Speicher 16 GB 1600 MHz DDR3  
# Netzwerkbetrieb

# mean(pred == test_set$label)

# die Caret Funktion gibt noch mehr Informationen
caret::confusionMatrix(table(pred, test_set$label))
# Accuracy : 0.9181          
# 95% CI : (0.9127, 0.9232)
# Der Algo hat Probleme mit der 3, 8 und 9

# Random Forest
# -----------------------------------------------------------------------------------------------------
start <- Sys.time()
model_rf <- ranger(label~., data=train_set, num.trees=250)
pred <- model_rf %>%
  predict(test_set[,2:ncol(test_set)])
Sys.time()-start
# Time difference of 44.8462 secs

caret::confusionMatrix(table(pred$predictions, test_set$label))
# Accuracy : 0.968           
# 95% CI : (0.9644, 0.9713)
# Hat Learner hat Probleme mit 3, 8 und 9 jedoch im besser als SVM


# Experiment mit Python and Sklearn
# https://htmlpreview.github.io/?https://github.com/dominikpeter/homework/blob/master/mining/Uebung3/archiv/mnist.html


