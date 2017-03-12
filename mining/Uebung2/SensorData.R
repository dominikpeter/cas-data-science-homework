# ------------------------------------all data
rm(list = ls())

library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(caret)

library(doParallel)

# Enable parallel processing.
cl <- makeCluster(detectCores())

list_dir <- list.dirs(path = "mining/HMP_Dataset", full.names = TRUE)[-1]


load_all <- function(dir) {
  files <- list.files(path = dir, full.names = TRUE)
  files <- files[grepl(".txt", x = files)]
  all_files <- mclapply(files, function(x) read_delim(x, delim = " ",
                                                    col_names = c("x","y","z"),
                                                    col_types = cols(
                                                      x = "d",
                                                      y = "d",
                                                      z = "d"
                                                    )))
  df <- bind_rows(all_files)
  df$Label <- dir
  df
}

df <- bind_rows(mclapply(list_dir, load_all))

df <- df %>%
  mutate(Label = str_replace_all(Label, "\\w+\\w+/", ""),
         Label = Label %>% as.factor)

Average_resultant_acceleration <- function(x, y, z, n){
  (1 / n * sum(sqrt(x^2+y^2+z^2)))
}

df <- df %>%
  group_by(Label) %>% 
  mutate(mean.x  = mean(x),
         mean.y  = mean(y),
         mean.z  = mean(z),
         var.x   = var(x),
         var.y   = var(y),
         var.z   = var(z),
         aad.x   = abs(x - mean.x),
         aad.y   = abs(y - mean.y),
         aad.z   = abs(z - mean.z),
         ara     = Average_resultant_acceleration(x, y, z, nrow(df)),
         max.y   = max(y)) %>% 
  ungroup() %>% 
  mutate(Label = Label %>% as.factor)

trainIndex <- createDataPartition(df$Label, p = .8, 
                                  list = FALSE, 
                                  times = 1)

df_train <- df[trainIndex, ]
df_test <- df[-trainIndex, ]

pp_hpc <- preProcess(df_train %>% dplyr::select(-Label), 
                     method = c("center", "scale", "nzv"))

transformed <- predict(pp_hpc, newdata = df_train %>% dplyr::select(-Label))
transformed$Label <- df_train$Label

fitControl <- trainControl(## 10-fold CV
  method = "boot",
  repeats = 5)
  
cl <- makeCluster(detectCores())
  
model <- train(Label ~ ., data = transformed, 
               method = 'AdaBag', 
               trControl = fitControl,
               verbose = FALSE)



