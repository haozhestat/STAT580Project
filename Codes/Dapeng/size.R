set.seed(12345)
size <- c(20,50,100,500,1000)
size0 <- 100
rank0 <- 8
missrate0 <- 0.5
training_error <- function(x,xhat){
  a <- sum((x-xhat)^2)/sum(x^2)
  return(a)
}
library(microbenchmark)
#default matrix
m0 <- matrix(rnorm(size0*rank0),ncol = rank0,nrow = size0)
n0 <- matrix(rnorm(size0*rank0),ncol = size0,nrow = rank0)
e0 <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S0 <- m0%*%n0+e0
#20*20
set.seed(12345)
m <- matrix(rnorm(size[1]*rank0),ncol = rank0,nrow = size[1])
n <- matrix(rnorm(size[1]*rank0),ncol = size[1],nrow = rank0)
e <- matrix(rnorm(size[1]*size[1]),ncol = size[1],nrow = size[1])
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),unit = "s")


#50*50
set.seed(12345)
m <- matrix(rnorm(size[2]*rank0),ncol = rank0,nrow = size[2])
n <- matrix(rnorm(size[2]*rank0),ncol = size[2],nrow = rank0)
e <- matrix(rnorm(size[2]*size[2]),ncol = size[2],nrow = size[2])
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),unit = "s")




#100*100
set.seed(12345)
m <- matrix(rnorm(size[3]*rank0),ncol = rank0,nrow = size[3])
n <- matrix(rnorm(size[3]*rank0),ncol = size[3],nrow = rank0)
e <- matrix(rnorm(size[3]*size[3]),ncol = size[3],nrow = size[3])
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50,unit = "s")



#500*500
set.seed(12345)
m <- matrix(rnorm(size[4]*rank0),ncol = rank0,nrow = size[4])
n <- matrix(rnorm(size[4]*rank0),ncol = size[4],nrow = rank0)
e <- matrix(rnorm(size[4]*size[4]),ncol = size[4],nrow = size[4])
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 1L,unit = "s")

#1000*1000
set.seed(12345)
m <- matrix(rnorm(size[5]*rank0),ncol = rank0,nrow = size[5])
n <- matrix(rnorm(size[5]*rank0),ncol = size[5],nrow = rank0)
e <- matrix(rnorm(size[5]*size[5]),ncol = size[5],nrow = size[5])
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 1L,unit = "s")



