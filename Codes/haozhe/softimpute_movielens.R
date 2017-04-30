rm(list = ls())

library(dplyr)
library(foreach)
library(doParallel)

setwd("~/Documents/Soft-Impute-MovieLens/")
source("Codes/haozhe/softimpute_internalsvd.R")
#source("softimpute_rpacksvd.R")

mat_train <- read.table("Data/ml-100k/movie_train.txt")
mat_validation <- read.table("Data/ml-100k/movie_validation.txt")
mat_test <- read.table("Data/ml-100k/movie_test.txt")

mat_train <- as.matrix(mat_train)
mat_validation <- as.matrix(mat_validation)
mat_test <- as.matrix(mat_test)

# mat_train <- read.table("movie_train.txt")
# mat_validation <- read.table("movie_validation.txt")
# mat_test <- read.table("movie_test.txt")

tmp_train <- mat_train
tmp_train[is.na(tmp_train)] <- 0
tmp_svd <- svd(tmp_train)$d

cl <- makeCluster(2)
registerDoParallel(cl)

lambda_grid_1 <- quantile(tmp_svd, seq(0.05,0.95,0.05))
sim_result_1 <- sapply(lambda_grid_1, 
                       function(lambda) {
                         softimpute_internalsvd(mat_train, lambda, 0.01, mat_validation)})


print(sim_result_1)

lambda_grid_2 <- seq(lambda_grid_1[which.min(sim_result_1[1,])] - 5, 
                     lambda_grid_1[which.min(sim_result_1[1,])] + 5, 1)
sim_result_2 <- sapply(lambda_grid_2, 
                       function(lambda) {
                         softimpute_internalsvd(mat_train, lambda, 0.01, mat_validation)})
print(sim_result_2)

lambda_grid_3 <- seq(lambda_grid_2[which.min(sim_result_2[1,])] - 1, 
                     lambda_grid_2[which.min(sim_result_2[1,])] + 1, 0.1)
sim_result_3 <- sapply(lambda_grid_3, 
                       function(lambda) {
                         softimpute_internalsvd(mat_train, lambda, 0.01, mat_validation)})

print(sim_result_3)

lambda_grid_4 <- seq(lambda_grid_3[which.min(sim_result_3[1,])] - 0.1, 
                     lambda_grid_3[which.min(sim_result_3[1,])] + 0.1, 0.01)
sim_result_4 <- sapply(lambda_grid_4, 
                       function(lambda) {
                         softimpute_internalsvd(mat_train, lambda, 0.01, mat_validation)})

print(sim_result_4)

stopCluster(cl)



softimpute_internalsvd(mat_train, lambda_grid_4[which.max(sim_result_4[1,])],
                    10^(-3), mat_test)

result <- rbind(
  data.frame(lambda = lambda_grid_1, RMSE_validation = sim_result_1[1,],
             Time = sim_result_1[2,], Step = "Step 1"),
  data.frame(lambda = lambda_grid_2, RMSE_validation = sim_result_2[1,],
             Time = sim_result_2[2,], Step = "Step 2"),
  data.frame(lambda = lambda_grid_3, RMSE_validation = sim_result_3[1,],
             Time = sim_result_3[2,], Step = "Step 3"),
  data.frame(lambda = lambda_grid_4, RMSE_validation = sim_result_4[1,],
             Time = sim_result_4[2,], Step = "Step 4"))

write.csv(result,file="sim_result_internalsvd_001.csv", row.names = FALSE)

library(ggplot2)
ggplot(result) + geom_line(aes(x=lambda,y=RMSE_validation), linetype= 2)+
  geom_point(aes(x=lambda,y=RMSE_validation))+
  facet_wrap(~Step, scales = "free")

ggplot(result) + geom_line(aes(x=lambda,y=Time), linetype= 2)+
  geom_point(aes(x=lambda,y=Time))

result1 <- read.csv("sim_result_rpacksvd_001.csv", header = TRUE)
result$method <- "internalsvd"
result1$method <- "rpacksvd"
result2 <- rbind(result,result1)

ggplot(result2[result2$Time<100,]) + 
  geom_line(aes(x=lambda,y=Time, colour=method), linetype= 2)+
  geom_point(aes(x=lambda,y=Time, colour=method), size=0.8)
