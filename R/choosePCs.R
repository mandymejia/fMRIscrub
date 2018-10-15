#keep only components that explain more than the mean variance
choosePCs_mean <- function(svd, method){
  #This is actually keeping those with above-average s.d.?
  #Square of eigenvectors is proportional to variance.
  rel_sd = svd$d

  if(method=='robdist'){
    #We want to keep PCs with above-average variance, but 
    # covMcd() requires n_PCs < n_voxels/3.
    #First, set the maximum number to keep
    # as floor(n_voxels/3), restrained between 1 and n_PCs.
    max_keep <- min(max(floor(nrow(U)/3),1), ncol(U))
    #Then, keep up to that many PCs with above-average sd,
    # prioritizing those with greatest sd. 
    min_sd <- max(mean(rel_sd), order(-rel_sd)[max_keep])
    keep <- rel_sd[rel_sd >= min_sd]
  } else if(method=='robdist_subset'){
    #Since we will partition the data into thirds,
    # the new requirement is n_PCs < n_voxels/3/3.
    max_keep <- min(max(floor(nrow(U)/9),1), ncol(U))
    min_sd <- max(mean(rel_sd), order(-rel_sd)[max_keep])
    keep <- rel_sd[rel_sd >= min_sd]
  } else {
    keep <- rel_sd[rel_sd > mean(rel_sd)]
  }

  U <- svd$u[,keep]
  return(U)
}

#keep components that have high kurtosis
choosePCs_kurtosis <- function(svd, method){
  U <- svd$u #<-- U matrix
  #first remove components that explain less than 99% of variation
  cumvarexp <- cumsum(svd$d/sum(svd$d))
  keep <- (cumvarexp < .99)
  U <- U[,keep] #<-- U matrix
  #compute kurtosis of remaining PCs
  kurt <- apply(U, 2, rob_kurtosis)
  keep <- which(kurt > 2)

  if(method=='robdist'){
    #We want to keep PCs with kurt > 2, but 
    # covMcd() requires n_PCs < n_voxels/3.
    #First, set the maximum number to keep
    # as floor(n_voxels/3), restrained between 1 and n_PCs.
    max_keep <- min(max(floor(nrow(U)/3),1), ncol(U))
    #Then, keep up to that many PCs with kurt > 2,
    # prioritizing those with greatest kurt. 
    min_kurt <- max(2, order(-kurt)[max_keep])
    keep <- kurt[kurt >= min_kurt]
  } else if(method=='robdist_subset'){
    #Since we will partition the data into thirds,
    # the new requirement is n_PCs < n_voxels/3/3.
    max_keep <- min(max(floor(nrow(U)/9),1), ncol(U))
    min_kurt <- max(2, order(kurt)[max_keep])
    keep <- kurt[kurt >= min_kurt]
  } else {
    keep <- kurt[kurt > 2]
  }

  U <- U[,keep]
  return(U)
}
