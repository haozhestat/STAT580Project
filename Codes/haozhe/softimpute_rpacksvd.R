library(svd)

soft_thres <- function(x,y){
  ifelse((x-y)>0, x-y, rep(0,length(x)))
}

softimpute_rpacksvd <- function(mat, lambda, thres_ratio){
  na_index <- is.na(mat)
  Z_old <- mat
  Z_old[na_index] <- 0 
  ratio <- 1 + thres_ratio
  while(ratio>thres_ratio){
    mat[na_index] <- Z_old[na_index]
    svd_result <- svd(mat)
    Z_new <- svd_result$u%*%diag(soft_thres(svd_result$d, lambda))%*%t(svd_result$v)
    ratio <- sum((Z_old-Z_new)^2)/sum((Z_old)^2)
    print(ratio)
    Z_old <- Z_new
  }
  return(Z_new)
}
