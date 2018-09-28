id_out.leverage <- function(leverage){
  out.lev3 <- (leverage > 3*median(leverage))
  out.lev4 <- (leverage > 4*median(leverage))
  out.lev5 <- (leverage > 5*median(leverage))

  out <- data.frame(out.lev3, out.lev4, out.lev5)
  names(out) <- c('3med_outlier','4med_outlier','5med_outlier')
  return(out)
}


id_out.robdist_subset <- function(distance, inMCD, Q, Fparam){

  ## Distance Outliers (discontiguous time series)
  t <- length(distance)
  t2 <- floor(t/3)*3
  h3 <- sum(inMCD)/3
  c <- Fparam$c
  m <- Fparam$m
  df <- Fparam$df

  #label outliers
  cutoffs <- qf(p=c(0.99,0.999,0.9999), df1=df[1], df2=df[2])
  out.mah99 <- out.mah999 <- out.mah9999 <- rep(FALSE, t)
  out.mah99[!inMCD] <- (distance[!inMCD] > cutoffs[1])
  out.mah999[!inMCD] <- (distance[!inMCD] > cutoffs[2])
  out.mah9999[!inMCD] <- (distance[!inMCD] > cutoffs[3])

  out <- data.frame(out.mah99, out.mah999, out.mah99)
  names(out) <- c('0.99 quantile outlier','0.999 quantile outlier','0.9999 quantile outlier')

  result <- list(outliers=out, cutoffs=cutoffs)
  return(result)
}


id_out.robdist <- function(distance, inMCD, Q, Fparam){

  ## Distance Outliers (discontiguous time series)
  t <- length(distance)
  c <- Fparam$c
  m <- Fparam$m
  df <- Fparam$df

  #label outliers
  cutoffs <- qf(p=c(0.99,0.999,0.9999), df1=df[1], df2=df[2])
  out.mah99 <- out.mah999 <- out.mah9999 <- rep(FALSE, t)
  out.mah99[!inMCD] <- (distance[!inMCD] > cutoffs[1])
  out.mah999[!inMCD] <- (distance[!inMCD] > cutoffs[2])
  out.mah9999[!inMCD] <- (distance[!inMCD] > cutoffs[3])

  out <- data.frame(out.mah99, out.mah999, out.mah99)
  names(out) <- c('0.99 quantile outlier','0.999 quantile outlier','0.9999 quantile outlier')

  result <- list(outliers=out, cutoffs=cutoffs)
  return(result)
}

