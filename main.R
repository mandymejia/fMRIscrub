library(oro.nifti)
library(ggplot2)

vectorize_NIftI = function(NIfTI_fname, mask_fname){
	dat = readNIfTI(NIfTI_fname)
	mask = readNIfTI(mask_fname)
	mask = 1*(mask > 0)
	T = dim(dat)[4]
	V = sum(mask)

	Dat = matrix(NA, T, V)
	for(t in 1:T){
	  dat_t = dat[,,,t]
	  Dat[t,] = dat_t[mask==1]
	}

	return(Dat)
}

# Not really necessary. Just is nice for not overwriting files.
generate_fname = function(existing_fname){
	last_period_index <- regexpr("\\.[^\\.]*$", existing_fname)
	extension <- substr(existing_fname, last_period_index, nchar(existing_fname))
	# If parenthesized number exists...
	if(substr(existing_fname, last_period_index-1, last_period_index-1) == ')'){
		in_last_parenthesis <- gsub(paste0('(*\\))', extension), '', 
			gsub('.*(*\\()', '', existing_fname))
		n <- suppressWarnings(as.numeric(in_last_parenthesis))
		if(is.numeric(n)){
			if(!is.na(n)){
				# Add one and return.
				np1 <- as.character(n + 1)
				n <- as.character(n)
				return(gsub(
						paste0( '(\\(', n, '\\))', extension), 
						paste0('\\(', np1, '\\)', extension),
						existing_fname))
			}
		}
	}
	# Otherwise, append "(1)".
	return(gsub(extension, paste0('(1)', extension), existing_fname))
}

# Check for valid arguments.
stopifnot(exists('NIfTI_fname'))
stopifnot(exists('mask_fname'))

if(exists('plot_fname')){
	stopifnot(is.character(plot_fname))
	stopifnot(!endsWith('.png', plot_fname))
} else {
	# Plot (and csv table) will be saved in the working directory.
	plot_fname <- gsub('\\.nii.*','_clever\\.png', gsub('.*\\/', '', NIfTI_fname))
}
if(file.exists(plot_fname)){
	plot_fname <- generate_fname(plot_fname)
}

if(exists('csv_fname')){
	stopifnot(is.character(csv_fname))
	stopifnot(!endsWith('.csv', csv_fname))
} else {
	csv_fname <- gsub('\\.nii.*','_clever\\.csv', gsub('.*\\/', '', NIfTI_fname))
}
if(file.exists(csv_fname)){
	csv_fname <- generate_fname(csv_fname)
}

# Vectorize data.
print('vectorizing...')
Dat <- vectorize_NIftI(NIfTI_fname, mask_fname)

# Perform clever.
print('performing clever...')
args <- list(x=Dat, choosePCs=NULL, method=NULL, id_out=NULL)
for(i in 2:length(args)){
	if(exists(names(args)[i])){
		args[i] <- get(names(args)[i])
	}
}
args <- args[!sapply(args, is.null)]
out <- do.call(clever, args)

print('saving results...')

# Save to png.
ggsave(filename = plot_fname, plot(out, type='n'))

# Save to csv.
choosePCs <- out$params$choosePCs
method <- out$params$method
measure <- switch(method,
	leverage=out$leverage,
	robdist=out$robdist,
	robdist_subset=out$robdist)
outliers <- out$outliers$outliers
cutoffs <- out$outliers$cutoffs

table <- data.frame(measure)
if(!is.null(outliers)){
	table <- cbind(table, outliers)
}
names(table) <- c(
	paste0(method, '. PCs chosen by ', choosePCs), 
	paste0(names(outliers), ' = ', cutoffs))
if(!is.null(out$in_MCD)){
	table <- cbind(table, in_MCD=out$in_MCD)
}

write.csv(table, csv_fname, row.names=FALSE)