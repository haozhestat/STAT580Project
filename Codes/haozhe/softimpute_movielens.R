rm(list = ls())
library(svd)

setwd("~/Documents/Soft-Impute-MovieLens/Data/ml-100k/")
source("~/Documents/Soft-Impute-MovieLens/Codes/Haozhe/softimpute_rpacksvd.R")

train_mat <- read.table("movie_train.txt", header=FALSE)
validation_mat <- read.table("movie_validation.txt", header = FALSE)
test_mat <- read.table("movie_test.txt", header=FALSE)

train_mat <- as.matrix(train_mat)
validation_mat <- as.matrix(validation_mat)
test_mat <- as.matrix(test_mat)

thres_ratio <- 0.01
tmp_mat <- train_mat
tmp_mat[is.na(tmp_mat)] <- 0
eigen_sample <- svd(tmp_mat)$d
lambda_prob <- seq(0.82,0.83,0.001)
sapply(quantile(eigen_sample, lambda_prob),
       function(lambda){
         Z <- softimpute_rpacksvd(train_mat,lambda,thres_ratio)
         return(sqrt(mean((Z[!is.na(validation_mat)] - validation_mat[!is.na(validation_mat)])^2)))
       })
Z <- softimpute_rpacksvd(train_mat,quantile(eigen_sample,0.82),thres_ratio)
sqrt(mean((Z[!is.na(test_mat)] - test_mat[!is.na(test_mat)])^2))
