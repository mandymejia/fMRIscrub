#keep only components that explain more than the mean variance
choosePCs_mean <- function(svd, method){
	U <- svd$u
	var = svd$d

	if(method %in% c('robdist','robdist_subset')){
		#Let q = n_PCs/n_timepoints (ncol(U)/nrow(U)).
		#covMcd() requires q <= 1/2.
		#Higher q will use more components for estimation,
		#	thus retaining a higher resolution of information.
		#Lower q will have higher breakdown points,
		#	thus being more resistant to outliers.
		#(At q = 1/2, the breakdown point ~= .25)
		#(At q = 1/20, the breakdown point ~= .475)
		q <- 1/3
		div = switch(method, robdist=1/q, robdist_subset=1/3*q)
		#Keep components with above-average variance, up to
		#	(q*n_timepoints) components. 
		n_keep <- min(max(floor(nrow(U)*div),1), ncol(U)) 
		min_var <- max(mean(var), var[order(-var)][n_keep])
		keep <- which(var >= min_var)
	} else {
		keep <- which(var > mean(var))
	}

	U <- U[,keep]
	return(U)
}

#keep components that have high kurtosis
choosePCs_kurtosis <- function(svd, method){
	U <- svd$u #<-- U matrix
	#Remember original number of PCs.
	n_PCs <- ncol(U)
	#first remove components that explain less than 99% of variation
	cumvarexp <- cumsum(svd$d/sum(svd$d)) #sd?
	keep <- (cumvarexp < .99)
	U <- U[,keep] #<-- U matrix
	#compute kurtosis of remaining PCs
	kurt <- apply(U, 2, rob_kurtosis)

	if(method %in% c('robdist','robdist_subset')){
		q <- 1/3
		div <- switch(method, robdist=q, robdist_subset=1/3*q)
		#Keep components with kurt > 2, up to 
		#	(q*n_timepoints) components. 
		max_keep <- min(max(floor(nrow(U)*div),1), n_PCs)
		min_kurt <- max(2, kurt[order(-kurt)][max_keep])
		keep <- which(kurt >= min_kurt)
	} else {
		keep <- which(kurt > 2)
	}

	U <- U[,keep]
	return(U)
}
