#' \code{"scrub"} plot sub-function
#'
#' Plot outlyingness measure(s) with the corresponding threshold(s). Requires
#'  the \code{cowplot} and \code{ggplot2} packages
#'
#' @param meas A \eqn{T} by \code{m} numeric data.frame with each column being the timecourse for an
#'  outlyingness measure. The names of the columns will be used to label the plot.
#' @param cut A length \eqn{m} numeric vector with each value being the cutoff for an
#'  outlyingness measure (each column in \code{meas}).
#' @param flag_intersect Only flag timepoints at which all measures are outliers?
#'  Default: \code{FALSE}.
# @param robdist_info A list containing information related to the robust
#  distance measure: inMCD, outMCD_scale, and Fparam. Not used if robust
#  distance-based measures are not being plottted.
#' @param colors A length \eqn{m} character vector giving the colors of each
#'  measure (each column in \code{meas})
#' @param geom "point" (default) or "line"
#' @param log_y Use log scale for y-axis? Default: \code{FALSE}
#' @param ylim_min,ylim_max The range of the y-axis. 
#' @param ... Additional arguments to ggplot: main, sub, xlab, ...
#'
#' @return A ggplot
#'
#' @importFrom utils stack
#'
#' @keywords internal
IDvols_plot <- function(
  meas, cut=NULL, flag_intersect=FALSE, 
  colors=NULL, log_y=FALSE, geom="point", 
  ylim_min=0, ylim_max= max(meas$measure),
  ...){

  # Load required packages.
  need_cow <- !requireNamespace("cowplot", quietly = TRUE)
  need_gg <- !requireNamespace("ggplot2", quietly = TRUE)
  if (need_cow && need_gg) {
    stop("Packages \"cowplot\" and \"ggplot2\" needed to plot `scrub` results. Please install them.", call. = FALSE)
  } else if (need_cow) {
    stop("Package \"cowplot\" needed to plot `scrub` results. Please install it.", call. = FALSE)
  } else if (need_gg) {
    stop("Package \"ggplot2\" needed to plot `scrub` results. Please install it.", call. = FALSE)
  }
  rm(need_cow, need_gg)

  # Format arguments.
  meas <- as.data.frame(meas)
  T_ <- nrow(meas)
  nMeas <- ncol(meas)
  id_outs <- !is.null(cut)
  flag_intersect <- as.logical(flag_intersect)
  gg_args <- list(...)

  # Flag outlying timepoints.
  if (id_outs) {
    cut <- as.numeric(cut)
    if (length(cut) != nMeas) {
      if (length(cut) == 1) { cut <- rep(cut, nMeas) } else { 
        stop("`cut` should be the same length as the number of columns in `meas`.") 
      }
    }
    if (flag_intersect) {
      flag <- apply(t(as.matrix(meas)) > cut, 2, all)
    } else {
      flag <- meas
      for (mm in seq(nMeas)) { flag[,mm] <- flag[,mm] > cut[mm] }
    }
  } else {
    flag <- rep(FALSE, T_)
  }

  # Get names.
  if (is.null(colnames(meas))) {
    colnames(meas) <- paste0("Measure ", seq(nMeas))
  }
  name <- colnames(meas)
  if (length(name) != length(unique(name))) { stop("All measure names should be unique.") }

  # Get colors.
  if (is.null(colors)) {
    # scales::brewer_pal("qual", "Set2")(8)
    colors <- c(
      "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
      "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"
    )[seq(0, nMeas-1) %% 8 + 1]
    if (nMeas > 8) { warning("More measures than default color palette (8). To fix, specify `colors`.") }
  }
  names(colors) <- name

  if (ncol(meas)==1) {
    meas <- data.frame(
      measure=meas[,1],
      name=colnames(meas)
    )
  } else {
    meas <- stack(meas)
    names(meas)[names(meas)=="values"] <- "measure"
    names(meas)[names(meas)=="ind"] <- "name"
  }
  meas$idx <- seq(T_)

  # # MCD
  # if (mcd_meas) {
  #   meas$inMCD <- vector("logical", T_)
  #   for (ii in 1:length(meas_subnames)) {
  #     ii_row <- meas$name==meas_subnames[ii]
  #     meas[ii_row,"inMCD"] <- robdist_info[[meas_subnames[ii]]]$inMCD
  #     meas[ii_row,"measure"] <- meas[ii_row,"measure"] *
  #       ifelse(
  #         meas[ii_row,"inMCD"],
  #         1,
  #         robdist_info[[meas_subnames[ii]]]$outMCD_scale
  #       )
  #   }
  # }

  if (id_outs) {
    if (length(dim(flag)) < 2) {
      flag <- data.frame(
        isOutlier = flag,
        name = ifelse(all(name==c("DPD", "ZD")), "DVARS Dual Cutoff", "Cutoff")
      )
    } else {
      flag <- stack(flag)
      names(flag)[names(flag)=="values"] <- "isOutlier"
      names(flag)[names(flag)=="ind"] <- "name"
    }
    flag$idx <- rep(1:T_)
    flag_name <- unique(flag$name)

    if (flag_intersect) {
      flag_colors <- setNames("#B8B8B8", flag_name)
    } else {
      flag_colors <- colors
    }
  }

  # Log values if applicable.
  if(log_y){
    meas$measure <- log(meas$measure, base=10)
    if (id_outs) { cut <- log(cut, base=10) }
  }

  # # Get the upper y-axis limit.
  # ylim_max <- ifelse(
  #   grepl("Leverage", name) & (!("leverage__fusedPCA" %in% meas_subnames)),
  #   1,
  #   ifelse(length(cut) < 1, max(meas$measure), max(max(cut), max(meas$measure)))
  # )
  # ylim_max <- ylim_max*1.05

  # # Get the lower y-axis limit.
  # ylim_min <- ifelse(name %in% c("DVARS", "GSR"), min(meas$measure),
  #   ifelse(log_y, min(meas$measure), 0)
  # )

  # Check if any outliers were detected.
  if(id_outs){
    any_outs <- any(flag$isOutlier)
    drop_line <- vector("list", length(flag_name))
    for (ii in seq(length(flag_name))) {
      flag_ii <- flag[flag$isOutlier & flag$name==flag_name[ii], "idx"]
      if (length(flag_ii) < 1) { next }
      drop_line[[ii]] <- data.frame(
        xmin = flag_ii - 0.5,
        xmax = flag_ii + 0.5,
        ymin = ylim_min,
        ymax = ylim_max
      )
    }
  } else {
    drop_line <- NULL
    any_outs <- FALSE
  }

  # Get labels.
  main <- ifelse("main" %in% names(gg_args), as.character(args$main), "Leverage")
  sub <- ifelse(
    "sub" %in% names(gg_args),
    as.character(args$sub),
    ifelse(
      id_outs,
      ifelse(length(drop_line) < 1, "No outliers detected.", ""),
      "(No outlier thresholding performed)"
    )
  )
  xlab <- ifelse("xlab" %in% names(gg_args), as.character(gg_args$xlab), "Index (Time Point)")
  ylab <- ifelse("ylab" %in% names(gg_args), as.character(gg_args$ylab), paste0("Leverage", ifelse(log_y, " (log", "")))
  legend.position <- ifelse("legend.position" %in% names(gg_args), as.character(gg_args$legend.position), "right")
  alpha <- ifelse("alpha" %in% names(gg_args), as.numeric(gg_args$alpha), 1)

  # Make ggplot.
  xmin <- xmax <- ymin <- ymax <- idx <- measure <- inMCD <- NULL
  plt <- ggplot2::ggplot()

  # Draw drop-down lines for outliers.
  if(any_outs){
    for (ii in seq(length(drop_line))) {
      if (is.null(drop_line[[ii]])) { next }
      plt <- plt +
        ggplot2::geom_rect(
          data=drop_line[[ii]],
          ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), alpha=.5*alpha, fill=flag_colors[ii]
        )
      #Text label if any outlier is detected.
    }
  }

  # Outlyingness cutoff line.
  for (ii in seq_len(length(cut))) {
    plt <- plt +
      ggplot2::geom_hline(
        yintercept=cut[ii], linetype="dashed", color=ifelse(length(cut)==1, "black", colors[ii])
      )
  }

  # # Draw data points (after drop-down lines, so they are drawn on top).
  # if(mcd_meas){
  #   plt <- plt +
  #     ggplot2::geom_point(data=meas, ggplot2::aes(x=idx, y=measure, color=name, shape=inMCD)) +
  #     ggplot2::scale_shape_manual(values=c(16, 3))

  # # [TO DO]: Only show first 10 or so CompCor PCs, and say so in the subtitle
  # } else if (grepl("CompCor", name)) {
  #   max_nPC <- max(as.numeric(gsub("PC", "", unique(meas$name))))
  #   for (ii in seq(max_nPC, 1)) {
  #     plt <- plt +
  #       ggplot2::geom_line(
  #         data=subset(meas, name==paste0("PC", ii)),
  #         ggplot2::aes(x=idx, y=measure, group=name, color=name), size=1
  #       )
  #   }
  # } else if (name=="GSR") {
  #   plt <- plt +
  #     ggplot2::geom_line(data=meas, ggplot2::aes(x=idx, y=measure, group=name, color=name), size=1)
  # } else {
  if (geom == "point") {
    plt <- plt + ggplot2::geom_point(data=meas, ggplot2::aes(x=idx, y=measure, color=name), alpha=alpha)
  } else if (geom == "line") {
    plt <- plt + ggplot2::geom_line(data=meas, ggplot2::aes(x=idx, y=measure, color=name), alpha=alpha)
  } else { stop() }
  plt <- plt + ggplot2::scale_color_manual(values=colors, labels=name)

  # Use an optimal spacing between the x-ticks.
  xticks_width <- c(1, 2, 2.5, 3, 5)
  xticks_width <- c(xticks_width, xticks_width*10, xticks_width*100,
    xticks_width*1000, xticks_width*10000, xticks_width*100000)
  xticks_width <- max(xticks_width[xticks_width*2 < T_*.9])
  xticks <- c(seq(from=0, to=floor(T_*.9), by=xticks_width), T_)

  plt <- plt + ggplot2::labs(y=ylab, x=xlab, color="Method") +
    cowplot::theme_cowplot() +
    #coord_cartesian(xlim=c(0, floor(max(d$index)*1.02)), ylim=c(0, ylim_max*1.2)) + #fix this line
    ggplot2::theme(
      #axis.title.x=ggplot2::element_blank(),
      legend.position=legend.position,
      panel.spacing.y=ggplot2::unit(1.5, "lines")) +
    ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult = c(.01, .01)), breaks=xticks) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult = c(0, .01)))

  return(plt)
}

#' Plot scrubbing results
#' 
#' Plot a leverage, DVARS, or FD timeseries from a \code{"IDvols_projection"},
#'  \code{"IDvols_DVARS"}, or \code{"IDvols_FD"} object, respectively. Highlight 
#'  volumes flagged for outlier presence.
#'
#' @param x The \code{"IDvols_*"} object.
#' @param title (Optional) If provided, will add a title to the plot.
#' @param ... Additional arguments to ggplot, e.g. \code{main}, \code{sub}, 
#'  \code{xlab}, \code{ylab}, \code{legend.position}
#'
#' @return A ggplot
#'
#' @keywords internal
plot_IDvols_wrapper <- function(x, title=NULL, ...){
  gg_args <- list(...)
  mtype <- as.character(x$measure_info["type"])
  stopifnot(mtype %in% c("Leverage", "DVARS", "FD"))

  # Measure(s)
  meas <- x$measure
  if (!is.data.frame(meas)) {
    meas <- setNames(
      as.data.frame(meas), 
      as.character(ifelse("name" %in% names(x$measure_info), x$measure_info["name"], x$measure_info["type"]))
    )
  }
  if (mtype == "DVARS") { meas <- meas[,c("DPD", "ZD")] }

  # Cutoff
  if (all(is.na(x$outlier_cutoff))) {
    cut <- NULL
  } else {
    cut <- x$outlier_cutoff
  }

  # y-limits
  if (mtype=="Leverage") {
    ylim_min <- 0; ylim_max <- 1
  } else {
    ylim_min <- min(0, min(meas)); ylim_max <- max(meas)
  }

  # Make the plot
  if ("legend.position" %in% names(gg_args)) {
    plt <- IDvols_plot(
      meas, cut, flag_intersect=(mtype=="DVARS"), 
      ylab=mtype, ylim_min=ylim_min, ylim_max=ylim_max, ...
    )
  } else {
    plt <- IDvols_plot(
      meas, cut, legend.position="none", flag_intersect=(mtype=="DVARS"), 
      ylab=mtype, ylim_min=ylim_min, ylim_max=ylim_max, ...
    )
  }

  # Add title.
  if(!is.null(title)){
    plt <- cowplot::plot_grid(
      cowplot::ggdraw() +
        cowplot::draw_label(title, fontface='bold', x=0, hjust=0) +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 7)),
      plt,
      ncol=1,
      rel_heights=c(.15, 1)
    )
  }

  return(plt)
}

#' Plot a \code{"IDvols_projection"} object
#' 
#' @param x The \code{"IDvols_projection"} object
#' @param title (Optional) If provided, will add a title to the plot.
#' @param ... Additional arguments to ggplot, e.g. \code{main}, \code{sub}, 
#'  \code{xlab}, \code{ylab}, \code{legend.position}
#' 
#' @return A ggplot
#' 
#' @method plot IDvols_projection
#' @export
plot.IDvols_projection <- function(x, title=NULL, ...) {
  plot_IDvols_wrapper(x, title=title, ...)
}

#' Plot a \code{"IDvols_DVARS"} object
#' 
#' @param x The \code{"IDvols_DVARS"} object
#' @param title (Optional) If provided, will add a title to the plot.
#' @param ... Additional arguments to ggplot, e.g. \code{main}, \code{sub}, 
#'  \code{xlab}, \code{ylab}, \code{legend.position}
#' 
#' @return A ggplot
#' 
#' @method plot IDvols_DVARS
#' @export
plot.IDvols_DVARS <- function(x, title=NULL, ...) {
  plot_IDvols_wrapper(x, title=title, ...)
}

#' Plot a \code{"IDvols_FD"} object
#' 
#' @param x The \code{"IDvols_FD"} object
#' @param title (Optional) If provided, will add a title to the plot.
#' @param ... Additional arguments to ggplot, e.g. \code{main}, \code{sub}, 
#'  \code{xlab}, \code{ylab}, \code{legend.position}
#' 
#' @return A ggplot
#' 
#' @method plot IDvols_FD
#' @export
plot.IDvols_FD <- function(x, title=NULL, ...) {
  plot_IDvols_wrapper(x, title=title, ...)
}

#' Plot a \code{"IDvols_projection_multi"} object
#'
#' @param x The \code{"IDvols_projection_multi"} object.
#' @param title (Optional) If provided, will add a title to the plot.
#' @param ... Additional arguments to ggplot, e.g. \code{main}, \code{sub}, 
#'  \code{xlab}, \code{ylab}, \code{legend.position}
#'
#' @return A ggplot
#'
# @method plot IDvols_projection_multi
#' @keywords internal
plot.IDvols_projection_multi <- function(x, title=NULL, ...){
  gg_args <- list(...)
  
  if ("legend.position" %in% names(gg_args)) {
    plt <- IDvols_plot(x$measure, x$outlier_cutoff, ...)
  } else {
    plt <- IDvols_plot(x$measure, x$outlier_cutoff, legend.position="none", ...)
  }

  # Add title.
  if(!is.null(title)){
    return(cowplot::plot_grid(
      cowplot::ggdraw() +
        cowplot::draw_label(title, fontface='bold', x=0, hjust=0) +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 7)),
      plt,
      ncol=1,
      rel_heights=c(.15, 1)
    ))
  } else {
    return(plt)
  }
}
