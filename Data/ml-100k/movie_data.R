rm(list = ls())

setwd("~/Documents/Soft-Impute-MovieLens/Data/ml-100k/")

data <- read.table("u.data", header =FALSE)
head(data)
data <- data[,1:3]
set.seed(580)
train_index <- sample(1:100000,70000)
validation_index <- sample(setdiff(1:100000,train_index), 15000)
test_index <- setdiff(1:100000, c(train_index, validation_index))

train_data <- data[train_index,]
validation_data <- data[validation_index,]
test_data <- data[test_index,]

train_mat <- matrix(NA,length(unique(data[,2])),length(unique(data[,1])))
validation_mat <- matrix(NA,length(unique(data[,2])),length(unique(data[,1])))
test_mat <- matrix(NA,length(unique(data[,2])),length(unique(data[,1])))

for(i in 1:70000)
  train_mat[train_data[i,2],train_data[i,1]] <- train_data[i,3]
for(i in 1:15000)
  validation_mat[validation_data[i,2], validation_data[i,1]] <- validation_data[i,3]
for(i in 1:15000)
  test_mat[test_data[i,2],test_data[i,1]] <- test_data[i,3]

write.table(train_mat, file="movie_train.txt", col.names = FALSE, row.names = FALSE)
write.table(validation_mat, file="movie_validation.txt", col.names = FALSE, row.names = FALSE)
write.table(test_mat, file="movie_test.txt", col.names = FALSE, row.names = FALSE)
