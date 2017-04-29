size0 <- 100
rank0 <- 8
missrate0 <- 0.5
rank1 <- c(4,8,12,16)
library(microbenchmark)
# rank=4
set.seed(12345)
m <- matrix(rnorm(size0*rank1[1]),ncol = rank1[1],nrow = size0)
n <- matrix(rnorm(size0*rank1[1]),ncol = size0,nrow = rank1[1])
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")

# rank=8
set.seed(12345)
m <- matrix(rnorm(size0*rank1[2]),ncol = rank1[2],nrow = size0)
n <- matrix(rnorm(size0*rank1[2]),ncol = size0,nrow = rank1[2])
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")

# rank=12
set.seed(12345)
m <- matrix(rnorm(size0*rank1[3]),ncol = rank1[3],nrow = size0)
n <- matrix(rnorm(size0*rank1[3]),ncol = size0,nrow = rank1[3])
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_svdpack(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")

# rank=16
set.seed(12345)
m <- matrix(rnorm(size0*rank1[4]),ncol = rank1[4],nrow = size0)
n <- matrix(rnorm(size0*rank1[4]),ncol = size0,nrow = rank1[4])
e <- matrix(rnorm(size0*size0),ncol = size0,nrow = size0)
S <- m%*%n+e
S[sample(length(S),size = trunc(missrate0*length(S)))] <- 0
microbenchmark(SoftImpute_svd(S,10),SoftImpute_Rcpp(S,10),SoftImpute_irlba(S,10),times = 50L,unit = "s")
