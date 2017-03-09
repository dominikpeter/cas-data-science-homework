rm(list = ls())
#set the working directory specific to my machine
setwd("./")


library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(caret)

#create a data frame from all files in specified folder
create_activity_dataframe = function(activityFolder,classId) {
  file_names = dir(activityFolder)
  file_names = lapply(file_names, function(x){ paste(".",activityFolder,x,sep = "/")})
  your_data_frame = do.call(rbind,lapply(file_names,function(x){read.csv(x,header = FALSE,sep = " ")}))
  your_data_frame = cbind(data.frame(rep(classId,nrow(your_data_frame))),your_data_frame)
  your_data_frame = cbind(data.frame(1:nrow(your_data_frame)),your_data_frame)
  colnames(your_data_frame) = c("timestep","class","x","y","z")
  your_data_frame
}
df1 = create_activity_dataframe("Brush_teeth",1)
View(df1)
library(ggplot2)
df1_sample = df1[sample(nrow(df1), 500), ]
ggplot(df1_sample, aes(timestep)) + 
  geom_line(aes(y = x, colour = "x")) + 
  geom_line(aes(y = y, colour = "y")) + 
  geom_line(aes(y = z, colour = "z"))

df2 = create_activity_dataframe("Climb_stairs",2)
df = rbind(df1,df2)

write.csv(df,"dsx_movement_pattern.csv")


# Determine number of clusters
determine_number_of_clusters = function(df) {
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(df,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares") 
}

number_of_clusters=2
n = nrow(df)

kmeans(df,centers=number_of_clusters)$centers

df_x_y = cbind(df$x,df$y)
determine_number_of_clusters(df_x_y)
km = kmeans(df_x_y,centers=number_of_clusters)


truthVector = km$cluster == df$class
good = length(truthVector[truthVector==TRUE])
bad = length(truthVector[truthVector==FALSE])
good/(good+bad)

library(scatterplot3d)
df_sample = df
df_sample$cluster = km$cluster
df_sample = df_sample[sample(nrow(df_sample), 1000), ]
with(df_sample, {
  scatterplot3d(x,y,z, color = cluster)
})

library(dplyr)
library(purrr)

# df <- df %>% 
#   rowwise() %>% 
#   mutate(meanacc = mean(x, y, z))

Average_resultant_acceleration <- function(x, y, z, n){
  (1 / n * sum(sqrt(x^2+y^2+z^2)))
}

df <- df %>%
  # group_by(class) %>% 
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
  ungroup()

km2 = kmeans(df %>% select(-timestep, -class),
            centers=number_of_clusters)


df <- df %>% 
  mutate(cluster = km2$cluster %>% as.integer(),
         correct = cluster == class)

mean(df$correct)

# ------------------------------------all data
rm(list = ls())
setwd("Homework/mining/HMP_Dataset")
list_dir <- list.dirs()[-1]

load_all <- function(dir) {
  files <- list.files(path = dir, full.names = TRUE)
  files <- files[grepl(".txt", x = files)]
  all_files <- lapply(files, function(x) read_delim(x, delim = " ",
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

df <- bind_rows(lapply(list_dir, load_all))

df <- df %>%
  mutate(Label = str_replace(Label, "./", ""),
         Label = Label %>% as.factor)

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
  mutate(Label = Label %>% )

trainIndex <- createDataPartition(df$Label, p = .3, 
                                  list = FALSE, 
                                  times = 1)

df_train <- df[trainIndex, ]
df_test <- df[-trainIndex, ]



pp_hpc <- preProcess(df_train %>% select(-Label), 
                     method = c("center", "scale", "nzv"))

transformed <- predict(pp_hpc, newdata = df_train %>% select(-Label))
transformed$Label <- df_train$Label



fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 2)

model <- train(Label ~ ., data = transformed, 
                 method = "LogitBoost", 
                 trControl = fitControl,
                 verbose = FALSE)




