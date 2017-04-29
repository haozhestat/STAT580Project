size0 <- 100
rank0 <- 8
missrate0 <- 0.5
missrate <- c(0.4,0.5,0.6,0.7,0.9)
library(microbenchmark)
# 0.4 missing rate
set.seed(12345)
m <- matrix(rnorm(size0*rank0),ncol = rank0,nrow = size0)
n <- matrix(rnorm(size0*rank0),ncol = size0,nrow = rank0)
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate[1]*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50,unit = "s")

# 0.5 missing rate
set.seed(12345)
m <- matrix(rnorm(size0*rank0),ncol = rank0,nrow = size0)
n <- matrix(rnorm(size0*rank0),ncol = size0,nrow = rank0)
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate[2]*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")

# 0.6 missing rate
set.seed(12345)
m <- matrix(rnorm(size0*rank0),ncol = rank0,nrow = size0)
n <- matrix(rnorm(size0*rank0),ncol = size0,nrow = rank0)
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate[3]*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")

# 0.7 missing rate
set.seed(12345)
m <- matrix(rnorm(size0*rank0),ncol = rank0,nrow = size0)
n <- matrix(rnorm(size0*rank0),ncol = size0,nrow = rank0)
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate[4]*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")


# 0.9 missing rate
set.seed(12345)
m <- matrix(rnorm(size0*rank0),ncol = rank0,nrow = size0)
n <- matrix(rnorm(size0*rank0),ncol = size0,nrow = rank0)
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate[5]*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")


