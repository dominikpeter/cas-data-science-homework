rm(list=ls())
library(caret)
library(readr)
library(scales)
library(parallelSVM)
library(ranger)
library(ggplot2)
library(dplyr)
library(adabag)

set.seed(2323)

mnist_matrix = read_csv( 'https://github.com/romeokienzler/developerWorks/raw/master/train.csv' )

df <- mnist_matrix

df$label <- as.factor(df$label)

index <- createDataPartition(df$label,
                             times = 1,
                             p = 0.75,
                             list = FALSE)

train_set <- df[index, ]
test_set <- df[-index, ]

zeros <- sapply(train_set[,-1], function(x) all(x == 0))

train_set <- train_set[, c(TRUE, !zeros)]
train_set[,-1] <- lapply(train_set[,-1], rescale)

test_set <- test_set[, c(TRUE, !zeros)]
test_set[,-1] <- lapply(test_set[,-1], rescale)


start <- Sys.time()
model <- parallelSVM(label~., data = train_set, numberCores = 10)
pred <- predict(model, test_set[,2:ncol(test_set)])
Sys.time()-start

confusionMatrix(table(pred, test_set$label))


models = list()
for (i in seq(1, 500, 50)){
  model_rf <- ranger(label~., data=train_set, num.trees=i)
  pred <- predict(model_rf, test_set[,2:ncol(test_set)])
  confu <- confusionMatrix(table(pred$predictions, test_set$label))
  acc <- confu$overall[1]
  models[[i]] <- c("Accuracy" = acc %>% as.numeric, "Number of Trees" = model_rf$num.trees)
}

accuracy_df <- data.frame("Accuracy"=unlist(sapply(models, `[[`, 1)),
                          "NTree"=unlist(sapply(models, `[[`, 2)))


accuracy_df %>% 
  ggplot(aes(x=NTree, y = Accuracy, group=1)) + 
  geom_point() +
  geom_line() +
  xlab("Anzahl Bäume") +
  ylab("Trefferquote")

# ab mehr als ~200 Trees wird die Trefferquote nicht mehr signifikant grösser

model_rf <- ranger(label~., data=train_set, num.trees=200)
pred <- predict(model_rf, test_set[,2:ncol(test_set)])

confusionMatrix(table(pred$predictions, test_set$label))


dimx <- sqrt(df %>% select(-label) %>% ncol())

nbr <- 7

m <- df[nbr,] %>% select(-label) %>% matrix(ncol=dimx)
im_numbers <- apply(m, 2, as.numeric)
image(1:28, 1:28, im_numbers)

predict(model_rf, df[nbr,] %>% select(-label))$predictions


