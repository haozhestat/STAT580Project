library(svd)

soft_thres <- function(x,y){
  ifelse((x-y)>0, x-y, rep(0,length(x)))
}

softimpute_rpacksvd <- function(mat_train, lambda, thres_ratio = 10^(-3), mat_validation = NULL){
  starting_time <- Sys.time()
  na_index <- is.na(mat_train)
  Z_old <- mat_train
  Z_old[na_index] <- 0 
  ratio <- 1 + thres_ratio
  while(ratio>thres_ratio){
    mat_train[na_index] <- Z_old[na_index]
    svd_result <- propack.svd(mat_train)
    Z_new <- svd_result$u%*%diag(soft_thres(svd_result$d, lambda))%*%t(svd_result$v)
    ratio <- sum((Z_old-Z_new)^2)/sum((Z_old)^2)
    #print(ratio)
    Z_old <- Z_new
  }
  end_time <- Sys.time()
  if(is.null(mat_validation))
    return(list(Z_new, end_time - starting_time))
  else{
    return(c(sqrt(mean((mat_validation - Z_new)[!is.na(mat_validation)]^2)),
             as.numeric(end_time - starting_time, units = "secs")))
  }
}
