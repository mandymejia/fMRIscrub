#' Visualizes the outlier distribution of a clever object.
#' 
#' Prints a dot plot of the observations (x-axis) against their outlyingness (y-axis), i.e.
#'  their leverage or robust distance. 
#' 
#' Cutoffs for each outlier level are marked by horizontal dashed lines. Outliers are 
#'  highlighted by vertical lines which extend down to the x-axis; they are colored yellow, 
#'  orange and red in order of increasing outlyingness. 
#' 
#' If the outlyingness measure is robust distance, observations within the MCD are plotted
#'  separately from those outside the MCD. Also, the y-axes will be log10-scaled.
#'
#' @param clev A clever object. 
#' @return The clever ggplot.
#'
#' @import ggplot2
#' @export
#'
#' @examples
plot.clever <- function(clev, ...){
	choosePCs <- clev$params$choosePCs
	method <- clev$params$method
	measure <- switch(method,
		leverage=clev$leverage,
		robdist=clev$robdist,
		robdist_subset=clev$robdist)
	outliers <- clev$outliers
	cutoffs <- clev$cutoffs

	choosePCs_formatted <- switch(choosePCs,
		kurtosis='Kurtosis',
		mean='Mean')
	method_formatted <- switch(method, 
		leverage='Leverage',
		robdist='Robust Distance',
		robdist_subset='Robust Distance Subset')
    args <- list(...)

	#Log the y-axis if the measurement is robust distance.
	log_measure <- switch(method,
		leverage=FALSE,
		robdist=TRUE,
		robdist_subset=TRUE)

	# Identify outliers and their levels of outlyingness.
	index <- 1:length(measure)
	outlier_level_num <- apply(outliers, 1, sum)  # get outlier levels as a single factor
	outlier_level_names <- c('not an outlier', colnames(outliers))
	outlier_level <- factor(outlier_level_names[outlier_level_num + 1], levels=outlier_level_names)
	d <- data.frame(index, measure, outlier_level)
	if(method %in% c('robdist','robdist_subset')){
	  d$inMCD <- ifelse(clev$inMCD, 'In MCD', 'Not In MCD')
	}
	
	# The plot will have lines extending downward from outliers
	#  to the x-axis. 
	is_outlier <- d$outlier_level != 'not an outlier'
	any_outliers <- any(is_outlier)
	if(any_outliers){
		# Obtain the coordinates of the outliers' lines' vertices.
		drop_line <- d[is_outlier,]
		drop_line$xmin <- drop_line$index - .5
		drop_line$xmax <- drop_line$index + .5
		drop_line$ymin <- 0
		drop_line$ymax <- drop_line$measure
		# ggplot will draw rows from top to bottom.
		# Ordering by increasing outlier level ensures lines for the
		#  most outlying observations are drawn last, i.e. on the top layer.
		drop_line <- drop_line[order(drop_line$outlier_level),]
	}
	
	if(log_measure){
		# Add 1 before log transforming to ensure a positive range.
		method_formatted <- paste0('log10(', method_formatted, ' + 1)')
		d$measure <- log(d$measure + 1, base = 10)
		if(any_outliers){
			drop_line$ymax <- log(drop_line$ymax + 1, base = 10)
		}
		cutoffs <- log(cutoffs + 1, base = 10)
	}
	
	# The lowest, middle, and highest outlier levels are colored
	#  yellow, orange, and red, respectively. 
	cols <- c(hsv(h=c(.1,.05,1), s=c(.6,.8,1)), '#000000')
	if(any_outliers){
		cols <- cols[sort(unique(
			outlier_level_num[outlier_level_num!=0]))]
	}

	main <- ifelse('main' %in% names(args), args$main, 
		paste0('Outlier Distribution', 
			ifelse(any_outliers, '', ' (None Identified)')))
	sub <- ifelse('sub' %in% names(args), args$sub,
		paste0(choosePCs_formatted,', ',method_formatted))
	xlab <- ifelse('xlab' %in% names(args), args$xlab, 'Index (Time Point)')
	ylab <- ifelse('ylab' %in% names(args), args$ylab, method_formatted)
	if(method=='leverage'){ ylim_max <- 1 } 
	else { ylim_max <- max(d$measure) * 1.01 }
	
	plt <- ggplot(d, aes(x=index,y=measure, color=outlier_level))
	if(any_outliers){
		plt <- plt + geom_rect(data=drop_line, inherit.aes=FALSE,
			aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
				fill=outlier_level), alpha=.9)
	}
	plt <- plt + geom_point(show.legend = FALSE) +
	scale_color_manual(values=c('grey','black','black','black')) +
	scale_fill_manual(values=cols) + 
	geom_hline(yintercept=cutoffs, linetype='dashed') +
	labs(x=xlab, y=ylab, fill='Outlier Level') +
	coord_cartesian(xlim = c(0, max(d$index)), ylim = c(0, ylim_max)) +
	theme_classic() +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	ggtitle(main, subtitle=sub)

	if(method %in% c('robdist','robdist_subset')){
		plt <- plt + facet_grid(inMCD~.)
	}
	
	if('type' %in% names(args)){
		if(args$type == 'n'){ return(plt) }
	}

	print(plt)
	return(plt)
}