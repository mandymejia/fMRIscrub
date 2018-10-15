#keep only components that explain more than the mean variance
choosePCs_mean <- function(svd, method){
  U <- svd$u
  #This is actually keeping those with above-average s.d.?
  #Square of eigenvectors is proportional to variance.
  rel_sd = svd$d

  if(method %in% c('robdist','robdist_subset')){
    #We want to keep PCs with above-average variance, but 
    # covMcd() requires n_PCs < n_voxels/3.
    #First, set the maximum number to keep
    # as floor(n_voxels/3), restrained between 1 and n_PCs.
    # (n_voxels/9 for robdist_subset, since data is split in thirds.)
    div = switch(method, robdist=3, robdist_subset=9)
    max_keep <- min(max(floor(nrow(U)/div),1), ncol(U))
    #Then, keep up to that many PCs with above-average sd,
    # prioritizing those with greatest sd. 
    min_sd <- max(mean(rel_sd), rel_sd[order(-rel_sd)][max_keep])
    keep <- which(rel_sd >= min_sd)
  } else {
    keep <- which(rel_sd > mean(rel_sd))
  }

  U <- U[,keep]
  return(U) #also return min_sd?
}

#keep components that have high kurtosis
choosePCs_kurtosis <- function(svd, method){
  U <- svd$u #<-- U matrix
  #Remember original number of PCs.
  n_PCs = ncol(U)
  #first remove components that explain less than 99% of variation
  cumvarexp <- cumsum(svd$d/sum(svd$d)) #sd?
  keep <- (cumvarexp < .99)
  U <- U[,keep] #<-- U matrix
  #compute kurtosis of remaining PCs
  kurt <- apply(U, 2, rob_kurtosis)

  if(method %in% c('robdist','robdist_subset')){
    div = switch(method, robdist=3, robdist_subset=9)
    #We want to keep PCs with kurt > 2, but 
    # covMcd() requires n_PCs < n_voxels/3.
    max_keep <- min(max(floor(nrow(U)/div),1), n_PCs)
    min_kurt <- max(2, kurt[order(-kurt)][max_keep])
    keep <- which(kurt >= min_kurt)
  } else {
    keep <- which(kurt > 2)
  }

  U <- U[,keep]
  return(U) #also return min_kurt?
}
