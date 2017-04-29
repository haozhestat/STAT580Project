library(dplyr)
library(foreach)
library(doParallel)

#setwd("~/Documents/Soft-Impute-MovieLens/")
#source("Codes/haozhe/softimpute_internalsvd.R")
source("softimpute_internalsvd.R")

# mat_train <- read.table("Data/ml-100k/movie_train.txt")
# mat_validation <- read.table("Data/ml-100k/movie_validation.txt")
# mat_test <- read.table("Data/ml-100k/movie_test.txt")

mat_train <- read.table("movie_train.txt")
mat_validation <- read.table("movie_validation.txt")
mat_test <- read.table("movie_test.txt")

tmp_train <- mat_train
tmp_train[is.na(tmp_train)] <- 0
tmp_svd <- svd(tmp_train)$d

cl <- makeCluster(16)
registerDoParallel(cl)

lambda_grid_1 <- quantile(tmp_svd, seq(0.05,0.95,0.05))
sim_result_1 <- foreach(i = 1:length(lambda_grid_1), .combine = 'rbind') %dopar%{
  softimpute_internalsvd(mat_train, lambda_grid_1[i], 10^(-3), mat_validation)
}

print(sim_result_1)

lambda_grid_2 <- seq(lambda_grid_1[which.max(sim_result_1[,1])] - 5, 
                     lambda_grid_1[which.max(sim_result_1[,1])] + 5, 1)
sim_result_2 <- foreach(i = 1:length(lambda_grid_2),.combine = 'rbind') %dopar%{
  softimpute_internalsvd(mat_train, lambda_grid_2[i], 10^(-3), mat_validation)
}

print(sim_result_2)

lambda_grid_3 <- seq(lambda_grid_2[which.max(sim_result_2[,1])] - 1, 
                     lambda_grid_2[which.max(sim_result_2[,1])] + 1, 0.1)
sim_result_3 <- foreach(i = 1:length(lambda_grid_3),.combine = 'rbind') %dopar%{
  softimpute_internalsvd(mat_train, lambda_grid_3[i], 10^(-3), mat_validation)
}

print(sim_result_3)

lambda_grid_4 <- seq(lambda_grid_3[which.max(sim_result_3[,1])] - 0.1, 
                     lambda_grid_3[which.max(sim_result_3[,1])] + 0.1, 0.01)
sim_result_4 <- foreach(i = 1:length(lambda_grid_4),.combine = 'rbind') %dopar%{
  softimpute_internalsvd(mat_train, lambda_grid_4[i], 10^(-3), mat_validation)
}

print(sim_result_4)

stopCluster(cl)

write.csv(rbind(sim_result_1,sim_result_2, sim_result_3, sim_result_4),
          file="sim_result_internalsvd.csv", row.names = FALSE)

softimpute_internalsvd(mat_train, lambda_grid_4[which.max(sim_result_4[,1])],
                    10^(-3), mat_test)
