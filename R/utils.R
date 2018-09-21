scale_med <- function(mat){
  #x is nxp, we want to scale the columns
  n <- nrow(mat)
  p <- ncol(mat)

  ctr <- colMedians(mat, na.rm=TRUE)
  ctr_mat <- matrix(ctr, nrow=n, ncol=p, byrow=TRUE)

  mad <- 1.4826 * colMedians(abs(mat - ctr_mat), na.rm=TRUE)
  mad_mat <- matrix(mad, nrow=n, ncol=p, byrow=TRUE)

  #check for voxels with MAD = 0
  zero_mad <- mad == 0
  if(any(zero_mad)){
    warning(cat("Warning: ", sum(zero_mad),
      " zero-variance voxels. These will be set to zero.\n", sep=""))
  }

  mat_scaled <- ifelse(mad_mat == 0, 0, (mat - ctr_mat)/mad_mat)
  return(mat_scaled)
}

get_mad <- function(mat){
  n <- nrow(mat)
  p <- ncol(mat)

  ctr <- colMedians(mat, na.rm=TRUE)
  ctr_mat <- matrix(ctr, nrow=n, ncol=p, byrow=TRUE)
  mad <- 1.4826 * colMedians(abs(mat - ctr_mat), na.rm=TRUE)
  return(mad)
}

logL.F <- function(par, vals, cutoff){
  df1 <- par[1]
  df2 <- par[2]
  vals <- vals[vals <= cutoff]
  -1*sum(log(df(vals, df1, df2)) - log(pf(cutoff, df1, df2)))
}

logL.lnorm <- function(par, vals, cutoff){
  mean <- par[1]
  sd <- par[2]
  vals <- vals[vals <= cutoff]
  -1*sum(log(dlnorm(vals, mean, sd)) - log(plnorm(cutoff, mean, sd)))
}


