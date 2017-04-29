setwd("~/Desktop/ISU/STAT580/FinalProject")
lenadata <- scan(file = "lena256")
lenatemp <- lenadata[-c(1,2)]
lenavec <- NULL
for (j in 1:lenadata[1]){
  for (i in 1:lenadata[2]){
    lenavec[(j-1)*lenadata[1]+i] <- lenatemp[(lenadata[1]-i)*lenadata[1]+j]
  }
}
lena <- matrix(lenavec,ncol = lenadata[2], nrow = lenadata[1], byrow = T)
image(lena,axes=FALSE ,col = grey(seq(0,1,length=256)))
lenasample <- lena
set.seed(12345)
lenasample[sample(length(lena),size = trunc(0.4*length(lena)))] <- 0
sampleseq <- seq(1,length(lena))
sampleindex <- sampleseq*(lenasample!=0)
nonzerosize <- length(sampleindex[sampleindex!=0])
training_index <- sort(sample(sampleindex[sampleindex!=0],size = trunc(0.7*nonzerosize)))
validation <- lenasample
validation[training_index] <- 0
validation_index <- sampleseq*(validation!=0)
training <- lenasample*(validation==0)
validation1 <- validation[validation_index]

lambda <- svd(training)
lambda <- lambda$d
lambda1 <- sort(quantile(lambda,probs = seq(0,0.9,0.1),names = F),decreasing = T)


RMSE1 <- NULL
for(i in 1:length(lambda1)){
  S1 <- SoftImpute_svd(training,lambda1[i])
  S1_validation <- S1[validation_index]
  RMSE1[i] <- sqrt(sum((validation1-S1_validation)^2)/length(validation1))
}
plot(lambda1,RMSE1,type = "b")

minRMSE_index <- length(lambda1)-order(RMSE1)[1]
lambda2 <- sort(quantile(lambda,probs = seq(minRMSE_index/10-0.1,minRMSE_index/10+0.1,0.01),names = F),decreasing = T)
RMSE2 <- NULL
for(i in 1:length(lambda2)){
  S1 <- SoftImpute_svd(training,lambda2[i])
  S1_validation <- S1[validation_index]
  RMSE2[i] <- sqrt(sum((validation1-S1_validation)^2)/length(validation1))
}
plot(lambda2,RMSE2,type = "b")

minRMSE_index2 <- length(lambda2)-order(RMSE2)[1]
lambda3 <-sort(quantile(lambda,probs = seq(minRMSE_index2/100-0.01,minRMSE_index2/100+0.01,0.001),names = F),decreasing = T)
RMSE3 <- NULL
for(i in 1:length(lambda3)){
  S1 <- SoftImpute_svd(training,lambda3[i])
  S1_validation <- S1[validation_index]
  RMSE3[i] <- sqrt(sum((validation1-S1_validation)^2)/length(validation1))
}
plot(lambda3,RMSE3,type = "b")
lambda_best <- lambda3[order(RMSE3)[1]]



S1 <- SoftImpute_svd(lenasample,lambda_best)
image(lenasample,axes=FALSE ,col = grey(seq(0,1,length=256)))
image(S1,axes=FALSE ,col = grey(seq(0,1,length=256)))















