#keep only components that explain more than the mean variance
choosePCs_mean <- function(svd){
  Q <- max(which(svd$d > mean(svd$d)))
  U <- svd$u[,1:Q]
  return(U)
}

#keep components that have high kurtosis
choosePCs_kurtosis <- function(svd){
  U <- svd$u #<-- U matrix
  #first remove components that explain less than 99% of variation
  cumvarexp <- cumsum(svd$d/sum(svd$d))
  keep <- (cumvarexp < .99)
  U <- U[,keep] #<-- U matrix
  #compute kurtosis of remaining PCs
  kurt <- apply(U, 2, rob_kurtosis)
  keep <- which(kurt > 2)
  U <- U[,keep]
  return(U)
}
