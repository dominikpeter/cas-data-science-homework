
# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben 2
# Autor:  Dominik Peter
# Date:   2017-02-25
# ------------------------------------------------------------------------------------------------

# Ausgabe 2 - Clustering
# ------------------------------------------------------------------------------------------------
# 1. Verwenden Sie das bereitgestellte cluster.r script aus der Vorlesung und replizieren die
# Demonstration indem Sie die Daten vom UCI Machine Learning Repository verwenden

# https://archive.ics.uci.edu/ml/datasets/Dataset+for+ADL+Recognition+with+Wrist-worn+Accelerometer#

# 2. Versuchen Sie durch generieren zusätzlicher "Features" die Qualität des Clusterings zu erhönen
# Hinweis: http://www.duchess-france.org/analyze-accelerometer-data-with-apache-spark-and-mllib/ (Abschnitt DETERMINE AND COMPUTE FEATURES FOR THE MODEL)

rm(list = ls())
set.seed(2323)

#set the working directory specific to my machine
setwd("~/Homework/mining/HMP_Dataset")



library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(caret)

# 1.
# ------------------------------------------------
#set the working directory specific to my machine
setwd("~/Homework/mining/HMP_Dataset")

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
#View(df1)
library(ggplot2)
df1_sample = df1[sample(nrow(df1), 500), ]

df2 = create_activity_dataframe("Climb_stairs",2)

ggplot(df2, aes(timestep)) + 
  geom_line(aes(y = x, colour = "x")) + 
  geom_line(aes(y = y, colour = "y")) + 
  geom_line(aes(y = z, colour = "z"))

df = rbind(df1,df2)
# View(df)

#write.csv(df,"dsx_movement_pattern.csv")


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

df_clas_x_y_z = cbind(df$class,df$x,df$y,df$z)
kmeans(df_clas_x_y_z,centers=number_of_clusters)$centers

df_x_y_z = cbind(df$x,df$y,df$z)
determine_number_of_clusters(df_x_y_z)
km = kmeans(df_x_y_z,centers=number_of_clusters)


truthVector = km$cluster != df$class
good = length(truthVector[truthVector==TRUE])
bad = length(truthVector[truthVector==FALSE])
acc <- good/(good+bad)
max(1-acc, acc)


library(scatterplot3d)
df_sample = df[sample(nrow(df), 500), ]
with(df_sample, {
  scatterplot3d(x,y,z)
})

centers_df = km$centers
colnames(centers_df) = c("x","y","z")
with(data.frame(centers_df), {
  scatterplot3d(x,y,z)
})


centroid1 = data.frame(0,5,km$centers[1,1],km$centers[1,2],km$centers[1,3])
colnames(centroid1) = c("timestep","class","x","y","z")
centroid2 = data.frame(0,6,km$centers[2,1],km$centers[2,2],km$centers[2,3])
colnames(centroid2) = c("timestep","class","x","y","z")
data2plot = rbind(centroid1,centroid2,df_sample)
ds3 = scatterplot3d(data2plot$x,data2plot$y,data2plot$z,color = as.numeric(data2plot$class))


# 2.
# -------------------------------------------------

# Load all Data

# get all file names
dirs <- list.dirs(full.names = TRUE)
files <- lapply(dirs, function(x) list.files(path = paste(x), full.names = TRUE))
files <- files[grepl(".txt", files)] %>% unlist()

read_all_files <- function(path){
  require(stringr)
  require(dplyr)
  df <- read_delim(path, delim = " ",
                   col_names = c("x", "y", "z"),
                   col_types = cols(
                     x = col_double(),
                     y = col_double(),
                     z = col_double()
                     ))
  df <- df %>%
    mutate(class = basename(dirname(path)),
           timestamp = str_extract_all(path, "\\d+-\\d+-\\d+-\\d+-\\d+-\\d+",
                                       simplify = TRUE))

}

df <- bind_rows(lapply(files, read_all_files))

average_resultant_acceleration <- function(x, y, z, n){
  (1 / n * sum(sqrt(x^2+y^2+z^2)))
}


df_transformed <- df %>%
  group_by(class, timestamp) %>% 
  mutate(mean.x  = mean(x),
         mean.y  = mean(y),
         mean.z  = mean(z),
         var.x   = var(x),
         var.y   = var(y),
         var.z   = var(z),
         aad.x   = abs(x - mean.x),
         aad.y   = abs(y - mean.y),
         aad.z   = abs(z - mean.z),
         ara     = average_resultant_acceleration(x, y, z, nrow(df)),
         max.y   = max(y)) %>% 
  ungroup() %>% 
  filter(class %in% c("Brush_teeth","Climb_stairs")) # just using 2 movements

#just using mean of y
km2 = kmeans(df_transformed %>% select(mean.y),
             centers=2)

df_transformed <- df_transformed %>% 
  mutate(cluster = km2$cluster %>% as.integer()) %>% 
  left_join(data.frame(class = c("Brush_teeth","Climb_stairs"), classID = c(1,2)))


correct <- mean(df_transformed$classID == df_transformed$cluster)

print(paste("Accuracy of Clustering:" , round(max(1-correct, correct)*100, 2), "%"))
# Max because of random clustering from the kmeans algo even when setting a seed

# [1] "Accuracy of Clustering: 97.41 %"





