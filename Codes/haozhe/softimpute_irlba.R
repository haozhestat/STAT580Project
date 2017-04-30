library("irlba")
library("Matrix")
SoftImpute_irlba <- function(x,lambda,epsilon=0.00001,maxiter=1000){
  z_old <- matrix(0, nrow = dim(x)[1], ncol=dim(x)[2])
  i <- 0
  n <- trunc(min(dim(x))/2-1)
  D_lambda <- 0
  while(1){
    if(i==0){
      svd1 <- irlba(x+(x==0)*z_old,n)
    } else{
      D <- diag(D_lambda)
      m <- length(D[D>0])
      svd1 <- irlba(x+(x==0)*z_old,min(n,m))
    }
    D_lambda <- diag((svd1$d-lambda>0)*(svd1$d-lambda))
    z_new <- svd1$u %*% D_lambda %*% t(svd1$v)
    if(sum((z_new-z_old)^2)/sum(z_old^2)<epsilon){
      break
    }
    if(i>maxiter){
      break
    }
    z_old <- z_new
    i <- i+1
  }
  return(z_new)
}