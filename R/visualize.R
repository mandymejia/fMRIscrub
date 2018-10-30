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
#'  separately from those outside the MCD.
#'
#' @param clever A clever object.
#' @param log_measure If TRUE (default), will log10-scale the y-axis (the outlyingness
#' measure). Vales are increased by 1 before log-transforming to ensure a positive range.
#'
#' @import ggplot2
#' @export
#'
#' @examples
plot.clever <- function(clever, log_measure = FALSE){
	# Identify the outlier measurement.
	choosePCs_formatted <- switch(clever$params$choosePCs,
		kurtosis='Kurtosis',
		mean='Mean')
	method <- clever$params$method
	method_formatted <- switch(method, 
		leverage='Leverage',
		robdist='Robust Distance',
		robdist_subset='Robust Distance Subset')
	measure <- switch(method,
		leverage=clever$leverage,
		robdist=clever$robdist,
		robdist_subset=clever$robdist)

	# Identify outliers and their levels of outlyingness.
	cutoffs <- clever$cutoffs
	index <- 1:length(measure)
	outliers <- clever$outliers 
	outlier_level_num <- apply(outliers, 1, sum)  # get outlier levels as a single factor
	outlier_level_names <- c('not an outlier', colnames(outliers))
	outlier_level <- factor(outlier_level_names, 
		levels=outlier_level_names)[outlier_level_num + 1]
	d <- data.frame(index, measure, outlier_level)
	if(method %in% c('robdist','robdist_subset')){
	  d$inMCD <- ifelse(clever$inMCD, 'In MCD', 'Not In MCD')
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
	labs(x='Index (Time Point)', 
		y=method_formatted, fill='Outlier Level') +
	coord_cartesian(xlim = c(0, max(d$index)), ylim = c(0, ylim_max)) +
	theme_classic() +
	scale_x_continuous(expand=c(0,0)) +
	scale_y_continuous(expand=c(0,0)) +
	ggtitle(paste0('Outlier Distribution', 
		ifelse(any_outliers, '', ' (None Identified)')),
		subtitle=paste0(choosePCs_formatted,', ',method_formatted))
	
	if(method %in% c('robdist','robdist_subset')){
		plt <- plt + facet_grid(inMCD~.)
	}
	
	print(plt)
}