RandMat_FixRank <- function(N, P, FIXRANK){
  mat <- matrix(rnorm(N*P,0,10), N, P)
  mat_svd <- svd(mat)
  return(mat_svd$u%*%diag(c(mat_svd$d[1:FIXRANK], rep(0,P-FIXRANK)))%*%t(mat_svd$v))
}

MissingIndex <- function(N, P, MISSRATE, REPLACE){
  Total_Grid <- expand.grid(row=1:N, column =1:P)
  Grid_Sample <- sample(1:(N*P),round(N*P*MISSRATE), replace=REPLACE)
  return(Total_Grid[Grid_Sample,])
}

write.table(RandMat_FixRank(100,50,10), file = "~/Documents/Soft-Impute-MovieLens/Codes/SimRandMat_test.txt",
            row.names = FALSE, col.names = FALSE)
write.table(MissingIndex(100,50,0.1,FALSE), file = "~/Documents/Soft-Impute-MovieLens/Codes/MIndex_test.txt",
            row.names = FALSE, col.names = FALSE)
