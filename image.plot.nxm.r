image.plot.nxm <- function(x, y, z, n, m, dry=F, 
                           individual_zlim=F,
                           horizontal=F, top_bottom=F, add_title=T,
                           cex.znames=1,
                           xlab="xaxis", ylab="yaxis", zlab="Variable [unit]",
                           cex.axis=1.25,
                           bgcol="white", NAcol="gray", poly_border_col=NA, 
                           useRaster=T,
                           contour_only=F, add_contour=T, contour_include_zero=T,
                           contour_posneg_soliddashed=T, contour_posneg_redblue=F,
                           contour_smooth=F, contour_smooth_n_pixel_thr=5, contour_spar=0.5,
                           contour_labcex=1, contour_drawlabels=T, contour_vfont=NULL, #c("sans serif", "bold"), 
                           type="active", plotname="testplot",
                           cm_bottom=2, cm_left=2.5, cm_top=1,
                           cm_right=4, colorbar_width_cm=0.45, colorbar_dist_cm=0.2,
                           width_png=2000, height_png=1666, res=300, 
                           width_pdf=7, height_pdf=7,
                           axis.args=NULL, add_names_inset=F, add_names_topleft=T,
                           legend.args=NULL, legend.line=6, legend.cex=0.85,
                           colorbar.cex=1.25,
                           family="sans", lwd=0.5, lwd.ticks=0.5, 
                           verbose=F,
                           ...) {

    if (verbose) message("\n*********** start image.plot.nxm() with `verbose`=T and `dry`=", 
                         substr(dry, 1, 1), " **************")
    
    ## demo values
    if (missing(x) && missing(y) && missing(z)) {
        message("x,y,z not provided --> run demo ...")
        n <- 3; m <- 2
        x <- y <- z <- vector("list", l=n*m)
        for (i in 1:(n*m)) {
            x[[i]] <- 1:20
            y[[i]] <- 1:20
            z[[i]] <- array(rnorm(length(x[[i]])*length(y[[i]])),
                            c(length(x[[i]]), length(y[[i]])))
            z[[i]][8:13,8:13] <- NA
        }
    }
   
    # necessary input; in contrast to the demo case above
    if (!missing(x)) {
        if (missing(y)) stop("y is missing")
        if (missing(z)) stop("z is missing")
    }
    if (!missing(y)) {
        if (missing(x)) stop("x is missing")
        if (missing(z)) stop("z is missing")
    }
    if (!missing(z)) {
        if (missing(x)) stop("x is missing")
        if (missing(y)) stop("y is missing")
    }
    if (!is.list(z)) z <- list(z)
    if (!is.list(x)) {
        xl <- vector("list", l=length(z))
        for (i in seq_along(z)) xl[[i]] <- x
        x <- xl; rm(xl)
    }
    if (!is.list(y)) {
        yl <- vector("list", l=length(z))
        for (i in seq_along(z)) yl[[i]] <- y
        y <- yl; rm(yl)
    }
    if (length(x) != length(z)) stop("x and z must have same length")
    if (length(y) != length(z)) stop("y and z must have same length")
    
    # capture additional arguments (aka ellipsis, dots, ...)
    dot_list <- list(...) # todo: use base::chkDots(...)?
    ndots <- length(dot_list)
    dot_names <- names(dot_list)
    if (verbose && ndots > 0) {
        message("dots:")
        for (i in 1:length(dot_list)) {
            message("*******************\ndots[[", i, "]]: ", dot_names[i])
            cat(capture.output(str(dot_list[[i]])), sep="\n")
        }
    }

    # figure out nrow and ncol aready here that dry run is faster
    # just learned: grDevices::n2mfrow(nplots) is doing the trick
    nz <- length(z) 
    if (missing(n) || missing(m)) { # default
        if (missing(n) && missing(m)) {
            nm <- grDevices::n2mfrow(nz)
            # nz: nrow ncol
            # 1: 1 1
            # 2: 2 1
            # 3: 3 1
            # 4: 2 2
            # 5: 3 2
            # 6: 3 2
            # 7: 3 3
            # 8: 3 3
            if (nz == 8) nm <- c(4, 2)
            # 9: 3 3
            # 10: 4 3
            if (nz == 10) nm <- c(5, 2)
            n <- nm[1]; m <- nm[2]
            if (F && nz == 2) { # special case: should nrow,ncol = 2,1 (default) or 1,2?
                n <- 1; m <- 2
            }
        } else {
            if (missing(n)) {
                n <- ceiling(nz/m)
                if (verbose) {
                    message("provided m = ", m, " cols x automatic n = ", n, 
                            " rows = ", n*m)
                }
            } else if (missing(m)) {
                m <- ceiling(nz/n)
                if (verbose) {
                    message("provided n = ", n, " rows x automatic m = ", m, 
                            " cols = ", n*m)
                }
            }
        } # if n or m or both are missing
    } # if n or m are missing
    if (verbose) message("--> n x m = ", n, " x ", m, " ...")
    nplots <- n*m
    if (nplots < nz) {
        stop("the obtained n*m = ", n*m, " < nz = ", nz, ". re-run with proper n (nrow) or/and m (ncol)")
    }

    # decide which axes are drawn to which subplot
    # n=nrow, m=ncol
    left_axis_inds <- bottom_axis_inds <- title_inds <- rep(F, t=nplots)
    for (i in seq_len(nplots)) {
        
        # order plots from top to bottom and then from left to right (default: False)
        if (top_bottom) { 
            
            if (verbose) message("axes top_bottom")
            
            # titles in top row
            if (i == n*(m-1)+1) title_inds[i] <- T
            
            # left axes
            if (i <= n) { 
                if (verbose) message("axis top_bottom i=", i, " <= n (=", n, ")")
                left_axis_inds[i] <- T
            }
            
            # bottom axes
            if (i %% n == 0 # bottom row
                ) { # todo: or last column of (nrow-1)th row if n*m > nplots
                if (verbose) message("axis top_bottom i=", i, " %% n (=", n, ") = ", i %% n, " == 0")
                bottom_axis_inds[i] <- T
            }

        # else order plots from left to right and then from top to bottom (default: True)
        } else if (!top_bottom) { 
            
            # titles in top row
            if (i == m) title_inds[i] <- T

            # left axes
            if (verbose) message("i %% m = ", i, " %% ", m, " = ", i %% m)
            if (nplots == 1 || m == 1 ||
                (nplots > 1 && i %% m == 1)) {
                if (verbose) message("left axis !top_bottom i=", i, " %% m (=", m, ") = ", i %% m, " == 1")
                left_axis_inds[i] <- T
            }

            # bottom axes
            if (i >= (n*m - m + 1) || # last row
                (nplots > nz && (i >= n*m - m))) { # or last column of (nrow-1)th row if nplots > nz
                if (verbose) message("bottom axis !top_bottom i=", i, " >= (n*m - m + 1) = (", 
                                     n, "*", m, " - ", m, " + 1) = ", (n*m - m + 1))
                bottom_axis_inds[i] <- T
            }
        } # if top_bottom
    } # for i nplots

    # just return number of rows and columns
    return_list <- list()
    return_list$nrow <- n
    return_list$ncol <- m
    return_list$nplots <- nplots
    return_list$nz <- nz
    return_list$top_bottom <- top_bottom
    return_list$left_axis_inds <- left_axis_inds
    return_list$bottom_axis_inds <- bottom_axis_inds
    return_list$title_inds <- title_inds
    if (dry) {
        return(return_list)
    } else {
        if (verbose) message("`dry`=F --> run function with `dry`=T do stop here and return nrow and ncol")
    }

    # derive nplots based on derived number of rows (n) and columns (m)
    if (add_names_inset && add_names_topleft) {
        stop("decide where to put legend: either `add_names_inset` or `add_names_topleft` can be true")
    }
   
    # check additional objects if provided
    if (any(dot_names == "image_list")) {
        image_list <- dot_list$image_list
        if (!is.null(image_list)) {
            if (is.null(image_list$x)) stop("provided `image_list` but `image_list$x` is missing")
            if (is.null(image_list$y)) stop("provided `image_list` but `image_list$y` is missing")
            if (is.null(image_list$z)) stop("provided `image_list` but `image_list$z` is missing")
        }
    } else {
        image_list <- NULL
    }
    
    if (any(dot_names == "polygon_list")) {
        polygon_list <- dot_list$polygon_list
        if (!is.null(polygon_list)) {
            if (is.null(polygon_list$x)) stop("provided `polygon_list` but `polygon_list$x` is missing")
            if (is.null(polygon_list$y)) stop("provided `polygon_list` but `polygon_list$y` is missing")
            if (is.null(polygon_list$z)) stop("provided `polygon_list` but `polygon_list$z` is missing")
            if (is.null(polygon_list$levels)) stop("provided `polygon_list` but `polygon_list$levels` is missing")
        }
    } else {
        polygon_list <- NULL
    }
    
    if (any(dot_names == "contour_list")) {
        contour_list <- dot_list$contour_list
        if (!is.null(contour_list)) {
            if (is.null(contour_list$x)) stop("provided `contour_list` but `contour_list$x` is missing")
            if (is.null(contour_list$y)) stop("provided `contour_list` but `contour_list$y` is missing")
            if (is.null(contour_list$z)) stop("provided `contour_list` but `contour_list$z` is missing")
            if (is.null(contour_list$levels)) stop("provided `contour_list` but `contour_list$levels` is missing")
            if (!is.null(contour_list$drawlabels)) contour_drawlabels <- contour_list$drawlabels
        }
    } else {
        contour_list <- NULL
    }
    
    if (any(dot_names == "quiver_list")) {
        quiver_list <- dot_list$quiver_list
        if (!is.null(quiver_list)) {
            if (is.null(quiver_list$u)) stop("provided `quiver_list` but `quiver_list$u` is missing")
            if (is.null(quiver_list$v)) stop("provided `quiver_list` but `quiver_list$v` is missing")
            if (is.null(quiver_list$const)) quiver_list$const <- rep(F, t=length(quiver_list$u))
            if (is.null(quiver_list$thr)) quiver_list$thr <- rep(NULL, t=length(quiver_list$u))
            if (is.null(quiver_list$nx_fac)) quiver_list$nx_fac <- rep(1, t=length(quiver_list$u))
            if (is.null(quiver_list$ny_fac)) quiver_list$ny_fac <- rep(1, t=length(quiver_list$u))
            if (is.null(quiver_list$scale)) quiver_list$scale <- rep(1, t=length(quiver_list$u))
            if (is.null(quiver_list$col)) quiver_list$col <- rep("black", t=length(quiver_list$u))
            if (is.null(quiver_list$angle)) quiver_list$angle <- rep(40, t=length(quiver_list$u))
            if (is.null(quiver_list$length)) quiver_list$length <- rep(0.07, t=length(quiver_list$u))
        }
    } else {
        quiver_list <- NULL
    }

    if (any(dot_names == "addland_list")) {
        addland_list <- dot_list$addland_list
        if (!is.null(addland_list)) {
            if (length(addland_list) != length(z)) { 
                stop("provided addland_list is of length ", length(addland_list), " but nz = ", nz)
            }
            for (i in seq_along(addland_list)) {
                if (!is.na(addland_list[i])) {
                    if (!is.null(addland_list[[i]]$data)) {
                        if (is.character(addland_list[[i]]$data)) {
                            if (!any(addland_list[[i]]$data == c("world", "world2"))) {
                                stop("provided `addland_list[[", i, "]]$data` must be ",
                                     "\"world\" (for lons -180,...,180) or \"world2\" (for lons 0,...,360)")
                            }
                            if (!any(search() == "package:maps")) library(maps)
                            addland_list[[i]]$type <- "map"
                        } else {
                            if (length(addland_list[[i]]$data) == 4 && 
                                all(names(addland_list[[i]]$data) == c("x0", "y0", "x1", "y1"))) {
                                # add checks
                                addland_list[[i]]$type <- "segments"
                            } else {
                                stop("`addland_list[[", i, "]]$data` -structure not implemented yet")
                            }
                        }
                    } else {
                        stop("provided `addland_list[[", i, "]]$data` is null")
                    }
                }
            }
        }
    } else {
        addland_list <- NULL
    }
    
    if (any(dot_names == "point_list")) {
        point_list <- dot_list$point_list
        if (!is.null(point_list)) {
            if (length(point_list) != length(z)) { 
                stop("provided point_list is of length ", length(point_list), " but nz = ", nz)
            }
        }
    } else {
        point_list <- vector("list", l=nz)
    }
    
    if (any(dot_names == "segment_list")) {
        segment_list <- dot_list$segment_list
        if (!is.null(segment_list)) {
            if (length(segment_list) != length(z)) { 
                stop("provided segment_list is of length ", length(segment_list), " but nz = ", nz)
            }
            for (i in seq_along(segment_list)) {
                if (!is.na(segment_list[[i]])) {
                    if (is.null(segment_list[[i]]$x0)) stop("provided `segment_list[[", i, "]]$x0` is missing")
                    if (is.null(segment_list[[i]]$y0)) stop("provided `segment_list[[", i, "]]$y0` is missing")
                    if (is.null(segment_list[[i]]$x1)) stop("provided `segment_list[[", i, "]]$x1` is missing")
                    if (is.null(segment_list[[i]]$y1)) stop("provided `segment_list[[", i, "]]$y1` is missing")
                }
            }
        }
    } else {
        segment_list <- NULL
    }
    
    if (any(dot_names == "text_list")) {
        text_list <- dot_list$text_list
        if (!is.null(text_list)) {
            if (length(text_list) != length(z)) { 
                stop("provided text_list is of length ", length(text_list), " but nz = ", nz)
            }
            for (i in seq_along(text_list)) {
                if (!is.na(text_list[[i]])) {
                    if (is.null(text_list[[i]]$x)) stop("provided `text_list[[", i, "]]$x` is missing")
                    if (is.null(text_list[[i]]$y)) stop("provided `text_list[[", i, "]]$y` is missing")
                    if (is.null(text_list[[i]]$labels)) stop("provided `text_list[[", i, "]]$labels` is missing")
                    if (is.null(text_list[[i]]$col)) text_list[[i]]$col <- "black"
                }
            }
        }
    } else {
        text_list <- vector("list", l=nz)
    }

    if (any(dot_names == "cmd_list")) {
        cmd_list <- dot_list$cmd_list
        if (!is.null(cmd_list)) {
            for (i in seq_along(cmd_list)) {
                if (!is.na(cmd_list[[i]])) {
                    if (typeof(cmd_list[[i]]) != "character") {
                        stop("provided `cmd_list[[", i, "]]` = ", dput(cmd_list[[i]]), 
                             "\nmust be of type character")
                    }
                }
            }
        }
    } else {
        cmd_list <- NULL
    }
    
    if (any(dot_names == "subplot_list")) {
        subplot_list <- dot_list$subplot_list
        if (!is.null(subplot_list)) {
            stop("todo")
        }
    } else {
        subplot_list <- NULL
    }

    # get all function arguments
    #all_args <- c(as.list(environment()), list(...)) # all arguments _defined by the function_
    provided_args <- as.list(match.call(expand.dots=T)) # all _provided_ args by user with dot args having no name
    #provided_args <- as.list(match.call(expand.dots=F)) # all _provided_ args by user with dot args having name "..."[[1]]

    # checks
    if (any(names(provided_args) == "contour_only")) { # if contour_only was provided by user
        if (contour_only) { # user wants contour_only
            if (any(names(provided_args) == "add_contour")) {
                if (add_contour) { # user wants add_contour
                    message("user provided both `contour_only` and `add_contour` as true.",
                            " set `contour_only` to false (default) and continue ...")
                }
            } else { # user did not provide add_contour
                if (add_contour) { # default of function
                    message("user provided `contour_only` as true but `add_contour` is also true (default).",
                            " assume that the non-default user choice is more impportant and set ",
                            "`add_contour` to false and continue ...")
                    add_contour <- F
                }
            }
        } else { # user does not want contour_only
            # nothing to do
        }
    } else { # user did not provide contour_only
        # here, contour_only=F (default)
    }
    if (add_contour || contour_only || !is.null(contour_list)) {
        if (contour_posneg_soliddashed && contour_posneg_redblue) {
            message("both `contour_posneg_soliddashed` and `contour_posneg_redblue` cannot be true.",
                    " set the latter to false (default) and continue ...")
            contour_posneg_redblue <- F
        }
    }

    # levels and colors
    if (individual_zlim) {
        if (!contour_only) {
            message("if `individual_zlim`=T, `contour_only` must be T. set `contour_only` from F to T ...")
            contour_only <- T
        }
    } else if (!individual_zlim) { # one zlim and breaks for all data
        if (any(dot_names == "ip")) {
            ip <- dot_list$ip
        } else {
            if (!any(dot_names == "zlim")) zlim <- range(z, na.rm=T)
            message("`ip` argument not provided. try to run `image.plot.pre(zlim)` ...") 
            ip <- image.plot.pre(zlim)
        }
        zlim <- ip$zlim
        cols <- ip$cols
        breaks <- ip$levels
        nlevels <- ip$nlevels
        axis.at <- ip$axis.at
        axis.at.ind <- ip$axis.at.ind
        axis.labels <- ip$axis.labels
    
        return_list$zlim <- zlim
        return_list$breaks <- breaks
        return_list$cols <- cols
        return_list$nlevels <- nlevels
        return_list$axis.at <- axis.at
        return_list$axis.labels <- axis.labels
    }

    # make sure that x and y are regular that image(..., useRaster=T) can be used
    if (useRaster) {
        # todo: what is the thr for useRaster? dev.capabilities("rasterImage")$rasterImage must be "yes" or "non-missing"
        if (!contour_only) {
            if (verbose) message("`useRaster`=T --> check if x,y are regular for graphics::image(..., useRaster=T) usage ...")
            for (i in seq_along(z)) {
                #if (any(grepl("POSIX", class(x[[i]])))) { # does not help; unique(dt) are more than 1; 
                #    dt <- mean(difftime(x[[i]][2:length(x[[i]])], x[[1]][1:(length(x[[i]])-1)]))
                #    x[[i]] <- seq.POSIXt(min(x[[i]], na.rm=T), max(x[[i]], na.rm=T), b=dt))
                #} else {
                    x[[i]] <- seq(min(x[[i]], na.rm=T), max(x[[i]], na.rm=T), l=length(x[[i]]))
                    y[[i]] <- seq(min(y[[i]], na.rm=T), max(y[[i]], na.rm=T), l=length(y[[i]]))
                #}
            }
        } # only check if image() will be used

        if (!is.null(image_list)) {
            if (verbose) message("`useRaster`=T --> check provided `image_list` if x,y are regular for graphics::image(..., useRaster=T) usage ...")
            for (i in seq_along(image_list$x)) {
                image_list$x[[i]] <- seq(min(image_list$x[[i]], na.rm=T), max(image_list$x[[i]], na.rm=T), 
                                         l=length(image_list$x[[i]]))
                image_list$y[[i]] <- seq(min(image_list$y[[i]], na.rm=T), max(image_list$y[[i]], na.rm=T), 
                                         l=length(image_list$y[[i]]))
            }
        }
    } # if useRaster

    if (verbose) {
        message("x:")
        cat(capture.output(str(x)), sep="\n")
        message("y:")
        cat(capture.output(str(y)), sep="\n")
    }

    ## Prepare plot
    if (!any(dot_names == "xlim")) {
        xlim <- range(x, na.rm=T)
        if (verbose) {
            cat("automatic xlim =")
            dput(xlim)
        }
    } else if (any(dot_names == "xlim")) {
        xlim <- dot_list[["xlim"]]
        if (verbose) {
            cat("provided xlim =")
            dput(xlim)
        }
    }   
    if (!any(dot_names == "ylim")) {
        ylim <- range(y, na.rm=T)
        if (verbose) {
            cat("automatic ylim =")
            dput(ylim)
        }
    } else if (any(dot_names == "ylim")) {
        ylim <- dot_list[["ylim"]]
        if (verbose) {
            cat("provided ylim =")
            dput(ylim)
        }
    }
    l <- max(c(sapply(x, length), 
               sapply(y, length)))
    x_plot <- seq(xlim[1], xlim[2], l=l)
    y_plot <- seq(ylim[1], ylim[2], l=l)
    
    if (verbose) {
        cat("x_plot = ")
        cat(capture.output(str(x_plot)), sep="\n")
        cat("y_plot = ")
        cat(capture.output(str(y_plot)), sep="\n")
    }
	if (!any(dot_names == "x_at")) {
        x_at <- pretty(x_plot, n=10)
        if (verbose) {
            cat("automatic x_at step 1 =")
            dput(x_at)
        }
	    x_at <- x_at[x_at >= min(x_plot) & x_at <= max(x_plot)]
        if (verbose) {
            cat("automatic x_at step 2 =")
            dput(x_at)
        }
    } else if (any(dot_names == "x_at")) {
        x_at <- dot_list[["x_at"]]
    }
    if (!any(dot_names == "y_at") || 
        (any(dot_names == "y_at") && is.null(dot_list[["y_at"]]))) {
        y_at <- pretty(y_plot, n=10)
        if (verbose) {
            cat("automatic y_at step 1 =")
            dput(y_at)
        }
        y_at <- y_at[y_at >= min(y_plot) & y_at <= max(y_plot)]
        if (verbose) {
            cat("automatic y_at step 2 =")
            dput(y_at)
        }
    } else if (any(dot_names == "y_at")) {
        y_at <- dot_list[["y_at"]]
    }

    if (!any(dot_names == "x_labels")) {
        x_labels <- format(x_at, trim=T)
        if (verbose) {
            cat("automatic x_labels =")
            dput(x_labels)
        }
    } else if (any(dot_names == "x_labels")) {
        x_labels <- dot_list[["x_labels"]]
    }

    if (!any(dot_names == "y_labels")) {
        y_labels <- format(y_at, trim=T)
        if (verbose) {
            cat("automatic y_labels =")
            dput(y_labels)
        }
    } else if (any(dot_names == "y_labels")) {
        y_labels <- dot_list[["y_labels"]]
    }
                    
    if (any(dot_names == "znames")) {
        znames <- dot_list[["znames"]]
    } else { # if not provided: default: a) 1, b) 2, ...
        znames <- names(z)
        if (is.null(znames)) {
            znames <- rep("", t=n*m)
            for (i in 1:(n*m)) {
                znames <- paste0(letters[i], ") ", 1:(n*m))
            }   
        }
    }

    # construct layout mat based on n x m; nrow x ncol
    if (top_bottom) { # plot figures from top to bottom and left to right
	    layout_mat <- matrix(1:(n*m), nrow=n, ncol=m, byrow=F)
    } else { # plot figures from left to right and top to bottom
        layout_mat <- matrix(1:(n*m), nrow=n, ncol=m, byrow=T)
    }

	# vertical colorbar on the right
    if (!contour_only) {
        if (!horizontal) {

            # left region for axes in cm; columns for plots in relative units; right region for colorbar in cm  
            layout_mat2 <- cbind(rep(0, t=n), # left axis row
                                 layout_mat,
                                 rep(n*m + 1, t=n)) # right legend column
            layout_widths <- c(lcm(cm_left), rep(1/m, t=m), lcm(cm_right))
            
            # upper region for title in cm; rows for plots in relative units; lower region for axes in cm
            layout_mat2 <- rbind(rep(0, t=m + 2), # title row
                                 layout_mat2,
                                 rep(0, t=m + 2)) # bottom axis row
            layout_heights <- c(lcm(cm_top), rep(1/n, t=n), lcm(cm_bottom))

        # horizontal colorbar at the bottom
        } else {
            stop("not yet") 
        }

    } else if (contour_only) {
        
        # left region for axes in cm; columns for plots in relative units; right region for colorbar in cm  
        layout_mat2 <- cbind(rep(0, t=n), # left axis row
                             layout_mat)
        layout_widths <- c(lcm(cm_left), rep(1/m, t=m))
        # upper region for title in cm; rows for plots in relative units; lower region for axes in cm
        layout_mat2 <- rbind(rep(0, t=m + 1), # title row
                             layout_mat2,
                             rep(0, t=m + 1)) # bottom axis row
        layout_heights <- c(lcm(cm_top), rep(1/n, t=n), lcm(cm_bottom))
    
    } # if contour_only or not
    return_list$layout_mat <- layout_mat
    return_list$layout_mat2 <- layout_mat2
    return_list$layout_widths <- layout_widths
    return_list$layout_heights <- layout_heights
    
    if (verbose) {
        cat("layout_widths=")
        dput(layout_widths)
        cat("layout_heights=")
        dput(layout_heights)
        cat("layout_mat2=")
        print(layout_mat2)
    }

    ## Open new or use already open plot device
    if (type == "active") {
        # open new device if none is open
        if (is.null(dev.list())) {
            dev.new(family=family)
        } # else use already open device (may be opened before in parent script ...)

    } else if (type == "png") {
        if (n < m) {
            width_png <- 2*width_png
        }
        png(plotname,
            width=width_png, height=height_png, res=res,
            family=family)

    } else if (type == "pdf") {
        if (m > n) {
            stop("neeeed")
        }
        pdf(plotname,
            width=width_pdf, height=height_pdf,
            family=family)
    }
    
    if (verbose) message("run layout() ...")
    layout(layout_mat2, widths=layout_widths, heights=layout_heights)
    #layout.show(n=max(layout_mat2))
    par(mar=rep(0.5, t=4)) # distance between sub-figures [rows]
	if (add_names_topleft) { # increase vertical distance between sub figures
        par(mar=c(0.5, 0.5, 1.5, 0.5)) 
    }
    if (verbose) {
        cat("fig=")
        dput(par("fig"))
        cat("fin=")
        dput(par("fin"))
        cat("oma=")
        dput(par("oma"))
        cat("omi=")
        dput(par("omi"))
        cat("mar=")
        dput(par("mar"))
        cat("mai=")
        dput(par("mai"))
        cat("usr=")
        dput(par("usr"))
        cat("plt=")
        dput(par("plt"))
        cat("pty=")
        dput(par("pty"))
    }

    # for every plot 
	for (i in seq_len(nplots)) {

        if (verbose) message("\n**************************************\n",
                             "subplot ", i, "/", nplots, " (nz = ", nz, ") ...")
        
        # Open i-th subplot device
        plot(x_plot, y_plot, t="n",
			 #xlim=xlim, ylim=ylim, 
             axes=F, xlab=NA, ylab=NA,
			 xaxs="i", yaxs="i")
       
        # its possible that there are less data to plot than nrow*ncols (e.g. length(x) = 5, ncol=2, nrow=3)
        # --> do not plot anything if (length(x) == nplots - 1 && i == nplots)
        if (length(x) == nplots - 1 && i == nplots) { 
            # nothing to do
            if (verbose) {
                message("length(x) = ", length(x), " == nplots - 1 = ", nplots - 1, 
                        " && i == nplots = ", nplots, " --> do not plot anything")
            }

        } else { # if length(x) != nplots - 1 && i != nplots

            # Add data to subplot
            nx <- length(x[[i]]); ny <- length(y[[i]])

            if (!contour_only) {
                
                if (verbose) message("`contour_only`=F --> add data to subplot using graphics::image() ...")

                # add NA values
                if (any(is.na(z[[i]]))) {
                    if (verbose) message("`z[[", i, "]]` has missing values (NA) --> add missing values ",
                                         "to subplot with color `NAcol`=", NAcol, " using graphics::image() ...")
                    graphics::image(x[[i]], y[[i]], array(1, c(nx, ny)),
                                    add=T, col=NAcol,
                                    axes=F, xlab="n", ylab="n",
                                    useRaster=useRaster)
                }

                # add actual data
                if (individual_zlim) {
                    stop("combination `contour_only`=F and `individual_zlim`=T not implemented yet")
                }
                graphics::image(x[[i]], y[[i]], z[[i]], 
                                add=T, col=cols, breaks=breaks,
                                axes=F, xlab="n", ylab="n", 
                                useRaster=useRaster)

            } # if !contour_only

            # add contour to subplot
            if (contour_only || add_contour) {
                
                if (verbose) {
                    if (contour_only) message("`contour_only`", appendLF=F) 
                    if (add_contour) message("`add_contour`", appendLF=F)
                    message("=T --> add data to subplot using graphics::contour() ...") 
                }

                if (contour_only) {
                    if (individual_zlim) {
                        axis.at <- pretty(range(z[[i]], na.rm=T), n=10) # overwrites global axis.at
                        if (verbose) {
                            message("`contour_only`=T and `individual_zlim`=T --> use individual contour levels. axis.at =")
                            dput(axis.at)
                        }
                    }
                }

                # distinguish between positive and negative contours
                if (contour_posneg_soliddashed || contour_posneg_redblue) {
                    if (contour_posneg_soliddashed) {
                        if (verbose) message("`contour_posneg_soliddashed`=T --> use solid lines for pos and dashed lines for neg values ...")
                    } else if (contour_posneg_redblue) {
                        if (verbose) message("`contour_posneg_redblue`=T --> use red lines for pos and blue lines for neg values ...")
                    }
                    if (any(axis.at < 0)) { # add negative values
                        axis.at.neg <- axis.at[axis.at < 0]
                        if (contour_posneg_soliddashed) {
                            graphics::contour(x[[i]], y[[i]], z[[i]],
                                              add=T, levels=axis.at.neg, 
                                              labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                              lty=2, axes=F, xlab="n", lwd=lwd)
                        } else if (contour_posneg_redblue) {
                            graphics::contour(x[[i]], y[[i]], z[[i]],
                                              add=T, levels=axis.at.neg, 
                                              labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                              lty=1, col="blue", axes=F, xlab="n", lwd=lwd)
                        }
                    } # if any neg values
                    if (any(axis.at >= 0)) { # add positive values
                        axis.at.pos <- axis.at[axis.at >= 0]
                        if (!contour_include_zero) {
                            if (any(axis.at.pos == 0)) {
                                if (verbose) message("`contour_include_zero`=F --> do not add zero contour line to subplot")
                                axis.at.pos <- axis.at.pos[which(axis.at.pos == 0)] 
                            }
                        }
                        if (contour_posneg_soliddashed) {
                            graphics::contour(x[[i]], y[[i]], z[[i]],
                                              add=T, levels=axis.at.pos, 
                                              labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                              lty=1, axes=F, xlab="n", lwd=lwd)
                        } else if (contour_posneg_redblue) {
                            graphics::contour(x[[i]], y[[i]], z[[i]],
                                              add=T, levels=axis.at.pos, 
                                              labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                              lty=1, col="red", axes=F, xlab="n", lwd=lwd)
                        }
                    } # if any pos values
                
                # default: do not distinguish between positive and negative contours
                } else if (!contour_posneg_soliddashed && !contour_posneg_redblue) {
                    if (verbose) message("both `contour_distinguish_posneg` and `contour_distinguish_redblue` are false",
                                         " --> use solid black lines for both pos and neg values ...")
                    tmp <- axis.at
                    if (!contour_include_zero) {
                        if (any(tmp == 0)) {
                            if (verbose) message("`contour_include_zero`=F --> do not add zero contour line to subplot")
                            tmp <- tmp[which(tmp == 0)] 
                        }
                    }
                    graphics::contour(x[[i]], y[[i]], z[[i]],
                                      add=T, levels=tmp, 
                                      labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                      axes=F, xlab="n", lwd=lwd)
                } # if distinguish between positive and negative contours or not

            } # if contour_only or add_contour
            
            # add additional data as image if available
            if (!is.null(image_list)) {
                if (verbose) message("add provided `image_list` to subplot using graphics::image() ...")
                if (length(image_list[[i]]$levels) == 1) { # special case: only 1 level
                    graphics::image(x[[i]], y[[i]], image_list[[i]]$data, 
                                    col=image_list[[i]]$cols,  
                                    add=T, useRaster=useRaster)
                } else {
                    graphics::image(x[[i]], y[[i]], image_list[[i]]$data, 
                                    col=image_list[[i]]$cols, breaks=image_list[[i]]$levels, 
                                    add=T, useRaster=useRaster)
                }
            } # if (!is.null(image_list))

            # add additional data as polygons if available
            if (!is.null(polygon_list)) {
                if (verbose) message("add provided `poly_list` to subplot using graphics::polygon() ...")
                poly_col_vec <- base::findInterval(polygon_list$z[[i]], polygon_list$levels, all.inside=F)
                cur_dev_type <- names(dev.cur())
                if (cur_dev_type == "pdf" && !is.na(poly_border_col)) {
                    polygon(polygon_list$x[[i]], polygon_list$y[[i]], col=cols[poly_col_vec], border=cols[poly_col_vec])
                } else {
                    polygon(polygon_list$x[[i]], polygon_list$y[[i]], col=cols[poly_col_vec], border=poly_border_col)
                }
            } # if (!is.null(polygon_list))

            # add additional data as contours if available
            if (!is.null(contour_list)) {
                
                if (is.null(contour_list$z[[i]])) {
                    if (verbose) message("provided `contour_list$z[[", i, "]]` is NULL. do not add to this subplot")
                
                } else { 
                    if (verbose) {
                        message("add provided `contour_list$z[[", i, "]]` to subplot using graphics::contour() ...")
                        message("   contour_list$levels = ", paste(contour_list$levels, collapse=", "))
                    }
                
                    # distinguish between positive and negative contours
                    if (contour_posneg_soliddashed || contour_posneg_redblue) {
                        if (contour_posneg_soliddashed) {
                            if (verbose) message("`contour_posneg_soliddashed`=T --> use solid lines for pos and dashed lines for neg values ...")
                        } else if (contour_posneg_redblue) {
                            if (verbose) message("`contour_posneg_redblue`=T --> use red lines for pos and blue lines for neg values ...")
                        }
                        if (any(contour_list$levels < 0)) { # add negative values
                            
                            axis.at.neg <- contour_list$levels[contour_list$levels < 0]
                            if (contour_posneg_soliddashed) {
                                lty <- 2; col <- "black"
                            } else if (contour_posneg_redblue) {
                                lty <- 1; col <- "blue"
                            }
                            
                            # draw original or smooth contours
                            if (!contour_smooth) {
                                graphics::contour(contour_list$x[[i]], contour_list$y[[i]], contour_list$z[[i]],
                                                  add=T, levels=axis.at.neg,
                                                  labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                                  col=col, lty=lty, axes=F, xlab="n", lwd=lwd)
                            } else if (contour_smooth) {
                                if (verbose) message("`contour_smooth`=T --> smooth negative contours longer than ",
                                                     "`contour_smooth_n_pixel_thr`=", contour_smooth_n_pixel_thr, 
                                                     " pixels using stats::smooth.spline() ...")
                                if (length(contour_spar) != length(axis.at.neg)) {
                                    message("smooth parameter `contour_spar`=", paste(contour_spar, collapse=", "), 
                                            " (in (0,1]; the higher the number the stronger the smoothing) is of ",
                                            "different length (", length(contour_spar), ") than the negative contour levels ",
                                            "to add to subplot (", length(axis.at.neg), ").")
                                    contour_spar_save <- contour_spar
                                    if (length(contour_spar) == 1) {
                                        message("length(contour_spar) = 1 --> repeat smoothing parameter ", length(axis.at.neg), " times ...")
                                        contour_spar <- rep(contour_spar, t=length(axis.at.neg))
                                    } else {
                                        stop("dont know how to proceed")
                                    }
                                } # if given contour_spar and axis.at.neg are of different length
                                for (j in seq_along(axis.at.neg)) {
                                    cl <- contourLines(contour_list$x[[i]], contour_list$y[[i]], contour_list$z[[i]],
                                                       levels=axis.at.neg[j])
                                    if (length(cl) >= 1) {
                                        for (k in seq_along(cl)) {
                                            if (length(cl[[k]]$x) > contour_smooth_n_pixel_thr) {
                                                cl_x <- stats::smooth.spline(x=seq_along(cl[[k]]$x), y=cl[[k]]$x,
                                                                             spar=contour_spar[j])
                                                cl_y <- stats::smooth.spline(x=seq_along(cl[[k]]$y), y=cl[[k]]$y,
                                                                             spar=contour_spar[j])
                                                lines(cl_x$y, cl_y$y, col=col, lty=lty, lwd=lwd)
                                            } else {
                                                message("contour line ", k, "/", length(cl), " is of length ", 
                                                        length(cl[[k]]$x), " < `contour_smooth_n_pixel_thr` = ", 
                                                        contour_smooth_n_pixel_thr, " --> ignore this contour line")
                                            }
                                        }
                                    }
                                } # for j contour lines
                                contour_spar <- contour_spar_save
                            } # if contour_smooth
                            
                        } # if any neg values
                        
                        if (any(contour_list$levels >= 0)) { # add positive values
                            
                            axis.at.pos <- contour_list$levels[contour_list$levels >= 0]
                            if (!contour_include_zero) {
                                if (any(axis.at.pos == 0)) {
                                    if (verbose) message("`contour_include_zero`=F --> do not add zero contour line to subplot")
                                    axis.at.pos <- axis.at.pos[which(axis.at.pos == 0)] 
                                }
                            }
                            if (contour_posneg_soliddashed) {
                                lty <- 1; col <- "black"
                            } else if (contour_posneg_redblue) {
                                lty <- 1; col <- "red"
                            }
                            
                            # draw original or smooth contours
                            if (!contour_smooth) {
                                graphics::contour(contour_list$x[[i]], contour_list$y[[i]], contour_list$z[[i]],
                                                  add=T, levels=axis.at.pos,
                                                  labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                                  col=col, lty=lty, axes=F, xlab="n", lwd=lwd)
                            } else if (contour_smooth) {
                                if (verbose) message("`contour_smooth`=T --> smooth positive contours longer than ",
                                                     "`contour_smooth_n_pixel_thr`=", contour_smooth_n_pixel_thr, 
                                                     " pixels using stats::smooth.spline() ...")
                                if (length(contour_spar) != length(axis.at.pos)) {
                                    message("smooth parameter `contour_spar`=", paste(contour_spar, collapse=", "), 
                                            " (in (0,1]; the higher the number the stronger the smoothing) is of ",
                                            "different length (", length(contour_spar), ") than the positive contour levels ",
                                            "to add to subplot (", length(axis.at.pos), ").")
                                    contour_spar_save <- contour_spar
                                    if (length(contour_spar) == 1) {
                                        message("length(contour_spar) = 1 --> repeat smoothing parameter ", length(axis.at.pos), " times ...")
                                        contour_spar <- rep(contour_spar, t=length(axis.at.pos))
                                    } else {
                                        stop("dont know how to proceed")
                                    }
                                } # if given contour_spar and axis.at.pos are of different length
                                for (j in seq_along(axis.at.pos)) {
                                    cl <- contourLines(contour_list$x[[i]], contour_list$y[[i]], contour_list$z[[i]],
                                                       levels=axis.at.pos[j])
                                    if (length(cl) >= 1) {
                                        for (k in seq_along(cl)) {
                                            if (length(cl[[k]]$x) > contour_smooth_n_pixel_thr) {
                                                cl_x <- stats::smooth.spline(x=seq_along(cl[[k]]$x), y=cl[[k]]$x,
                                                                             spar=contour_spar[j])
                                                cl_y <- stats::smooth.spline(x=seq_along(cl[[k]]$y), y=cl[[k]]$y,
                                                                             spar=contour_spar[j])
                                                lines(cl_x$y, cl_y$y, col=col, lty=lty, lwd=lwd)
                                            } else {
                                                message("contour line ", k, "/", length(cl), " is of length ", 
                                                        length(cl[[k]]$x), " < `contour_smooth_n_pixel_thr` = ", 
                                                        contour_smooth_n_pixel_thr, " --> ignore this contour line")
                                            }
                                        }
                                    }
                                } # for j contour lines
                                contour_spar <- contour_spar_save
                            } # if contour_smooth
                        } # if any pos values
                    
                    # default: do not distinguish between positive and negative contours
                    } else if (!contour_posneg_soliddashed && !contour_posneg_redblue) {
                        if (verbose) message("both `contour_distinguish_posneg` and `contour_distinguish_redblue` are false",
                                             " --> use solid black lines for both pos and neg values ...")
                        tmp <- contour_list$levels
                        if (!contour_include_zero) {
                            if (any(tmp == 0)) {
                                if (verbose) message("`contour_include_zero`=F --> do not add zero contour line to subplot")
                                tmp <- tmp[which(tmp == 0)] 
                            }
                        }
                            
                        # draw original or smooth contours
                        if (!contour_smooth) {
                            graphics::contour(contour_list$x[[i]], contour_list$y[[i]], contour_list$z[[i]],
                                              add=T, levels=tmp, 
                                              labcex=contour_labcex, drawlabels=contour_drawlabels, vfont=contour_vfont,
                                              axes=F, xlab="n", lwd=lwd)
                        } else if (contour_smooth) {
                            if (verbose) message("`contour_smooth`=T --> smooth positive contours longer than ",
                                                 "`contour_smooth_n_pixel_thr`=", contour_smooth_n_pixel_thr, 
                                                 " pixels using stats::smooth.spline() ...")
                            if (length(contour_spar) != length(axis.at.pos)) {
                                message("smooth parameter `contour_spar`=", paste(contour_spar, collapse=", "), 
                                        " (in (0,1]; the higher the number the stronger the smoothing) is of ",
                                        "different length (", length(contour_spar), ") than the positive contour levels ",
                                        "to add to subplot (", length(axis.at.pos), ").")
                                contour_spar_save <- contour_spar
                                if (length(contour_spar) == 1) {
                                    message("length(contour_spar) = 1 --> repeat smoothing parameter ", length(axis.at.pos), " times ...")
                                    contour_spar <- rep(contour_spar, t=length(axis.at.pos))
                                } else {
                                    stop("dont know how to proceed")
                                }
                            } # if given contour_spar and axis.at.pos are of different length
                            for (j in seq_along(axis.at.pos)) {
                                cl <- contourLines(contour_list$x[[i]], contour_list$y[[i]], contour_list$z[[i]],
                                                   levels=axis.at.pos[j])
                                if (length(cl) >= 1) {
                                    for (k in seq_along(cl)) {
                                        if (length(cl[[k]]$x) > contour_smooth_n_pixel_thr) {
                                            cl_x <- stats::smooth.spline(x=seq_along(cl[[k]]$x), y=cl[[k]]$x,
                                                                         spar=contour_spar[j])
                                            cl_y <- stats::smooth.spline(x=seq_along(cl[[k]]$y), y=cl[[k]]$y,
                                                                         spar=contour_spar[j])
                                            lines(cl_x$y, cl_y$y, col=col, lty=lty, lwd=lwd)
                                        } else {
                                            message("contour line ", k, "/", length(cl), " is of length ", 
                                                    length(cl[[k]]$x), " < `contour_smooth_n_pixel_thr` = ", 
                                                    contour_smooth_n_pixel_thr, " --> ignore this contour line")
                                        }
                                    }
                                }
                            } # for j contour lines
                            contour_spar <- contour_spar_save
                        } # if contour_smooth
                    } # if distinguish between positive and negative contours or not
                } # if !is.null(contour_list$z[[i]])
            } # if (!is.null(contour_list))
            
            # add additional data as quivers if available
            if (!is.null(quiver_list)) {
                if (verbose) message("add provided `quiver_list` to subplot using pracma::quiver() ...")
                if (!any(search() == "package:abind")) library(pracma)
                xmat <- array(x[[i]], c(nx, ny))
                ymat <- t(array(y[[i]], c(ny, nx)))
                quiver_inds <- array(F, c(nx, ny))
                if (verbose) message("nx_fac = ", quiver_list$nx_fac[i], ", ny_fac = ", 
                                     quiver_list$ny_fac[i], " --> draw ", quiver_list$nx_fac[i]*100, 
                                     " and ", quiver_list$ny_fac[i]*100,  
                                     " % of all possible quivers in x- and y-direction")
                # first try: get rough dx, dy -> dx and dy not necessarily constant
                quiver_inds_x <- seq(1, nx, l=quiver_list$nx_fac[i]*nx)
                quiver_inds_y <- seq(1, ny, l=quiver_list$ny_fac[i]*ny)
                # second try: use constant dx, dy
                quiver_inds_x <- seq(1, nx, b=trunc(mean(diff(quiver_inds_x))))
                quiver_inds_y <- seq(1, ny, b=trunc(mean(diff(quiver_inds_y))))
                message("quiver_inds_x: ", paste(head(quiver_inds_x), collapse=","),
                        ",...,", paste(tail(quiver_inds_x), collapse=","))
                quiver_inds[quiver_inds_x,quiver_inds_y] <- T
                message("quiver_inds_y: ", paste(head(quiver_inds_y), collapse=","),
                        ",...,", paste(tail(quiver_inds_y), collapse=","))
                if (!is.null(quiver_list$thr[i])) {
                    if (verbose) message("thr = ", quiver_list$thr[i], 
                                         " --> draw quivers >= ", quiver_list$thr[i])
                    quiver_thr_inds <- z[[i]] >= quiver_list$thr[i]
                    if (length(which(quiver_thr_inds)) == 0) {
                        message("--> zero locations are >= ", quiver_list$thr[i], ". continue without threshold ...")
                        quiver_thr_inds <- array(T, dim=dim(z[[i]]))
                    }
                    quiver_plot_inds <- quiver_inds & quiver_thr_inds
                } else {
                    if (verbose) message("thr is not set --> do not apply any threshold for quivers")
                    quiver_plot_inds <- quiver_inds
                }
                if (quiver_list$const[i]) {
                    if (verbose) message("const = T --> draw quivers of constant length")
                    # this does not work; arrows are not of equal length:
                    u <- quiver_list$u[[i]]/sqrt(quiver_list$u[[i]]^2 + quiver_list$v[[i]]^2)
                    v <- quiver_list$v[[i]]/sqrt(quiver_list$u[[i]]^2 + quiver_list$v[[i]]^2)
                } else {
                    if (verbose) message("const = F --> length of quivers represents velocity magnitude")
                    u <- quiver_list$u[[i]]
                    v <- quiver_list$v[[i]]
                }
                if (verbose) message("scale = ", quiver_list$scale[i], " with respect to velocity")
                pracma::quiver(x=xmat[quiver_plot_inds], y=ymat[quiver_plot_inds],
                               u=u[quiver_plot_inds], v=v[quiver_plot_inds],
                               scale=quiver_list$scale[i],
                               length=quiver_list$length[i],
                               angle=quiver_list$angle[i],
                               col=quiver_list$col[i], lwd=lwd)
            } # if (!is.null(quiver_list))
            
            # add land
            if (!is.null(addland_list) && !is.na(addland_list[i])) {
                if (verbose) {
                    if (addland_list[[i]]$type == "map") {
                        message("add provided `addland_list[[", i, "]]` to subplot using ",
                                "maps::map() with `addland_list$data` = ", addland_list[[i]]$data, " ...")
                    } else if (addland_list[[i]]$type == "segments") {
                        message("add provided `addland_list[[", i, "]]` to subplot using graphics::segments() ...")
                    }
                }
                if (is.null(addland_list[[i]]$xlim)) {
                    if (addland_list[[i]]$type == "map") {
                        if (addland_list[[i]]$data == "world") {
                            addland_list[[i]]$xlim <- c(-183.77640, 194.04724)
                        } else if (addland_list[[i]]$data == "world2") {
                            addland_list[[i]]$xlim <- c(3.666292, 363.666292)
                        }
                    } else if (addland_list[[i]]$type == "segments") {
                        addland_list[[i]]$xlim <- range(addland_list[[i]]$data$x0, 
                                                   addland_list[[i]]$data$x1, na.rm=T)
                    }
                } else if (!is.null(addland_list[[i]]$xlim)) {
                    if (length(addland_list[[i]]$xlim) == 1) {
                        if (addland_list[[i]]$xlim == "xlim") {
                            addland_list[[i]]$xlim <- xlim
                        } else {
                            stop("`addland_list[[", i, "]]$xlim` must be missing or ",
                                 "\"xlim\" or numeric of length 2")
                        }
                    } else if (length(addland_list[[i]]$xlim) == 2) {
                        if (!is.numeric(addland_list[[i]]$xlim)) { 
                            stop("`addland_list[[", i, "]]$xlim` must be missing or ",
                                 "\"xlim\" or numeric of length 2")
                        }
                    } else {
                        stop("`addland_list[[i]]$xlim` must be missing or ",
                             "\"xlim\" or numeric of length 2")
                    }
                } # is.null(addland$xlim) or not
                if (is.null(addland_list[[i]]$ylim)) {
                    if (addland_list[[i]]$type == "map") {
                        if (addland_list[[i]]$data == "world") {
                            addland_list[[i]]$ylim <- c(-86.91386, 85.32129)
                        } else if (addland_list[[i]]$data == "world2") {
                            addland_list[[i]]$ylim <- c(-91.760620, 85.370223)
                        }
                    } else if (addland_list[[i]]$type == "segments") {
                    addland_list[[i]]$ylim <- range(addland_list[[i]]$data$y0, 
                                               addland_list[[i]]$data$y1, na.rm=T)
                    }
                } else if (!is.null(addland_list[[i]]$ylim)) {
                    if (length(addland_list[[i]]$ylim) == 1) {
                        if (addland_list[[i]]$ylim == "ylim") {
                            addland_list[[i]]$ylim <- ylim
                        } else {
                            stop("`addland_list[[", i, "]]$ylim` must be missing or ",
                                 "\"ylim\" or numeric of length 2")
                        }
                    } else if (length(addland_list[[i]]$ylim) == 2) {
                        if (!is.numeric(addland_list[[i]]$ylim)) { 
                            stop("`addland_list[[", i, "]]$ylim` must be missing or ",
                                 "\"ylim\" or numeric of length 2")
                        }
                    } else {
                        stop("`addland_list[[", i, "]]$ylim` must be missing or ",
                             "\"ylim\" or numeric of length 2")
                    }
                } # is.null(addland_list$ylim) or not
                if (verbose) {
                    message("`addland_list[[i]]$xlim` = ", addland_list[[i]]$xlim[1], ", ", 
                            addland_list[[i]]$xlim[2])
                    message("`addland_list[[i]]$ylim` = ", addland_list[[i]]$ylim[1], ", ", 
                            addland_list[[i]]$ylim[2])
                }
                
                # add land stuff to every plot
                op <- par(no.readonly=T) # switch back to main plot with 'par(op)'
                par(new=T)
                plot(addland_list[[i]]$xlim, addland_list[[i]]$ylim, t="n",
                     axes=F, xlab=NA, ylab=NA,
                     xaxs="i", yaxs="i")
                if (addland_list[[i]]$type == "map") {
                    maps::map(addland_list[[i]]$data, interior=F, add=T, lwd=lwd)
                } else if (addland_list[[i]]$type == "segments") {
                    graphics::segments(x0=addland_list[[i]]$data$x0, 
                                       y0=addland_list[[i]]$data$y0,
                                       x1=addland_list[[i]]$data$x1, 
                                       y1=addland_list[[i]]$data$y1, lwd=lwd)
                }
                
                # special:
                if (T && !is.null(cmd_list) && !is.na(cmd_list[[i]])) {
                    if (verbose) message("special: add provided `cmd_list` to subplot in addland ",
                                         "section using base::eval(base::parse()) ...")
                    for (j in seq_along(cmd_list[[i]])) {
                        if (verbose) message("   run `", cmd_list[[i]][[j]], "` ...")
                        eval(parse(text=cmd_list[[i]][[j]]))
                    }
                } # if !is.null(cmd_list) && !is.na(cmd_list[[i]])
                
                #par(op) # switch back to main plot; somehow this breaks layout()'s subplot counting
                par(usr=op$usr) # only restore coords; todo: is this enough?
            } # if !is.null(addland_list) && !is.na(addland_list[i])
            #message("par(\"usr\") = ")
            #dput(par("usr"))
            
            # add additional stuff as points if available
            if (!is.null(point_list[[i]])) {
                if (verbose) message("add provided `point_list[[", i, "]]` to subplot using graphics::points() ...")
                if (is.null(point_list[[i]]$x) || is.null(point_list[[i]]$y)) {
                    message("point_list[[", i, "]]$x` or point_list[[", i, "]]$y` are NULL. cannot add points") 
                } else {
                    if (is.null(point_list[[i]]$col)) point_list[[i]]$col <- "black"
                    if (is.null(point_list[[i]]$bg)) point_list[[i]]$bg <- "black"
                    if (is.null(point_list[[i]]$pch)) point_list[[i]]$pch <- 4
                    if (is.null(point_list[[i]]$lwd)) point_list[[i]]$lwd <- lwd
                    if (is.null(point_list[[i]]$cex)) point_list[[i]]$cex <- 1
                    graphics::points(x=point_list[[i]]$x, y=point_list[[i]]$y,
                                     col=point_list[[i]]$col, bg=point_list[[i]]$bg,
                                     pch=point_list[[i]]$pch, lwd=point_list[[i]]$lwd, 
                                     cex=point_list[[i]]$cex)
                }
            } # if !is.null(point_list[[i]])
           
            # add additional stuff as segments if available
            if (!is.null(segment_list[[i]])) {
                if (verbose) message("add provided `segment_list[[", i, "]]` to subplot using graphics::segments() ...")
                graphics::segments(x0=segment_list[[i]]$x0, y0=segment_list[[i]]$y0,
                                   x1=segment_list[[i]]$x1, y1=segment_list[[i]]$y1, lwd=lwd)
            } # if !is.null(segment_list[[i]])

            if (!is.null(text_list[[i]])) {
                if (verbose) message("add provided `text_list[[", i, "]]` to subplot using graphics::text() ...")
                graphics::text(x=text_list[[i]]$x, y=text_list[[i]]$y,
                               labels=text_list[[i]]$labels, col=text_list[[i]]$col)
            } # if !is.null(text_list[[i]])
            
            # add cmd stuff to every plot
            if (!is.null(cmd_list)) {
                if (verbose) message("add provided `cmd_list` to subplot using base::eval(base::parse()) ...")
                for (j in seq_along(cmd_list)) {
                    if (verbose) message("   run `", cmd_list[[j]], "` ...")
                    eval(parse(text=cmd_list[[j]]))
                }
            } # if !is.null(cmd_list)

            # add title
            if (add_title && any(dot_names == "title") && title_inds[i]) {
                if (verbose) message("add provided `title` to subplot using base::title() ...")
                text(x=line2user(line=mean(par('mar')[c(2, 4)]), side=2), 
                     y=line2user(line=2, side=3), 
                     labels=dot_list[["title"]], xpd=NA, 
                     cex=1.5, font=1) # font=2 for bold
            }

            # add axes and axes labels
            if (verbose) message("add axes to subplot using graphics::axis() ...")
            if (left_axis_inds[i]) {
                graphics::axis(2, at=y_at, labels=y_labels, las=2, cex.axis=cex.axis, 
                     lwd=0, lwd.ticks=lwd.ticks)
                mtext(ylab, side=2, line=5, cex=1)
            } else { # just ticks
                graphics::axis(2, at=y_at, labels=F, lwd=0, lwd.ticks=lwd.ticks)
            }
            if (bottom_axis_inds[i]) {
                graphics::axis(1, at=x_at, labels=x_labels, cex.axis=cex.axis, 
                     lwd=0, lwd.ticks=lwd.ticks)
                mtext(xlab, side=1, line=3, cex=1)
            } else { # just ticks
                graphics::axis(1, at=x_at, labels=F, lwd=0, lwd.ticks=lwd.ticks)
            }

            # add name to every plot
            if (add_names_inset || add_names_topleft) {

                if (add_names_inset) {
                    
                    if (verbose) message("add names to subplot using base::legend() (`add_names_inset`=T) ...")
                    # prepare legend
                    lepos <- "topleft"
                    letext <- znames[i]
                    leinset <- 0.025 # distance legend box from horizontal (and vertical plot margins 
                    lexintersp <- -1.8 #    

                    # get coordinates of legend first
                    if (length(lepos) == 1) {
                        myleg <- legend(lepos, legend=letext, plot=F, inset=leinset, 
                                        bty="n", x.intersp=lexintersp, cex=cex.axis,
                                        col="black", lty=NA, lwd=lwd, pch=NA)
                    } else if (length(myleg_pos) == 2) {
                        myleg <- legend(lepos[1], lepos[2], legend=letext, plot=F, iinset=leinset, 
                                        bty="n", x.intersp=lexintersp, cex=cex.axis,
                                        col="black", lty=NA, lwd=lwd, pch=NA)
                    }

                    # draw background of legend label
                    if (F) message(myleg)
                    rect(xleft=myleg$rect$left, ybottom=myleg$rect$top - myleg$rect$h,
                         xright=myleg$rect$left + myleg$rect$w, ytop=myleg$rect$top, 
                         col=bgcol, lwd=lwd)
                    
                    # add text
                    if (length(lepos) == 1) {
                        myleg <- legend(lepos, legend=letext, plot=T, inset=leinset, 
                                        bty="n", x.intersp=lexintersp, cex=cex.axis,
                                        col="black", lty=NA, lwd=lwd, pch=NA)
                    } else if (length(myleg_pos) == 2) {
                        myleg <- legend(myleg_pos[1], myleg_pos[2], legend=text_lab, plot=T, inset=leinset, 
                                        bty="n", x.intersp=lexintersp, cex=cex.axis,
                                        col="black", lty=NA, lwd=lwd, pch=NA)
                    }
                    
                    if (F) {
                        legend("topleft",
                               #"topright", 
                               #"bottomright",
                               #"bottomleft",
                               legend=znames[i],
                               #legend=as.expression(znames[i]),
                               #legend=as.expression(paste0(letters[i], ") ", znames[i])),
                               col="black", lty=NA, lwd=lwd, pch=NA,
                               x.intersp=lexintersp,
                               cex=cex.znames, bty="n")
                    }

                # if add_names_topleft finished
                } else if (add_names_topleft) {

                    if (verbose) message("add names to subplot in top left corner using base::text (`add_names_topleft`=T) ...")
                    text(par("usr")[1], par("usr")[4] + strheight("."), 
                         xpd=T, labels=znames[i], pos=4, cex=cex.znames)

                } # add_names_inset or add_names_topleft

            } # if (add_names_inset || add_names_topleft)
           
            # draw box around plot
            box(lwd=lwd)

            ## overlay a subplot
            if (!is.null(subplot_list)) {
                    
                if (!any(search() == "package:TeachingDemos")) library(TeachingDemos)
                if (verbose) message("add subplot_list to subplot in using TeachingDemos::subplot() ...")

                for (spi in seq_along(subplot_list)) {

                    if (top_bottom) {
                        # add subplot axis only in last column
                        draw_axis_labels_inds <- 2 # need to fix that
                    }

                    #op <- par(no.readonly=T) # switch back to main plot with 'par(op)'
                    sb <- TeachingDemos::subplot(fun=subplot_list[[spi]]$fun(sb=subplot_list[[spi]], 
                                                                        draw_axis_labels_inds=draw_axis_labels_inds, i=i, lwd=lwd),
                                          x=grconvertX(subplot_list[[spi]]$fig_x, from="npc"),
                                          y=grconvertY(subplot_list[[spi]]$fig_y, from="npc"),
                                          type="plt")
                    #par(op) # switch back to main plot

                } # for spi in subplot_list 

            } # if !is.null(subplot))

            ## add something special to subplot
            if (F) { # my special phd stuff
                message("add special phd stuff")

                # area averaging box
                if (F && znames[i] == "H5") {
                    # load subplot() function
                    if (!any(search() == "package:TeachingDemos")) library(TeachingDemos)
                    
                    if (verbose) message("model drift add location subsection ...")
                    # for using par(sb) later on
                    #op <- par(no.readonly=T) # switch back to main plot with 'par(op)'

                    # draw subplot
                    sb <- subplot(fun=map_sub(bathy=bathync, database="world",
                                              xlim=sub_xlim, ylim=sub_ylim,
                                              sub_area_box=sub_area_box,
                                              csec_depth_levels=sub_depth_levels),
                                  x=grconvertX(sub_xp, from="npc"),
                                  y=grconvertY(sub_yp, from="npc"),
                                  type="plt") #pars=list( mar=c(0,0,0,0)+0.1)
                    #par(op) # switch back to main plot
                } # if csec_depth_subset

                # rossby
                if (T) {
              
                    # WKB horizontal velocity lm fit
                    if (F && (regexpr("H5 480m", znames[i]) != -1)) {
                        if (znames[i] == "R1 H5 480m") {
                            abline(a=1950.3939810, b=-0.0913097, lwd=2)
                            speed_cm_s <- 3.33387291856387
                        } else if (znames[i] == "R2 H5 480m") {
                            abline(a=1950.31389155, b=-0.09277237, lwd=2)
                            speed_cm_s <- 3.28031067717784
                        } else if (znames[i] == "R3 H5 480m") {
                            abline(a=1950.29371255, b=-0.09317323, lwd=2)
                            speed_cm_s <- 3.26771507361546
                        } else if (znames[i] == "R4 H5 480m") {
                            abline(a=1950.39046811, b=-0.09156948, lwd=2)
                            speed_cm_s <- 3.31599787547902
                        } else if (znames[i] == "R5 H5 480m") {
                            abline(a=1950.8370024, b=-0.0843012, lwd=2)
                            speed_cm_s <- 3.5091557691597
                        }
                        if (T && exists("speed_cm_s")) {
                            text <- substitute(paste(bold(u)[LR], " = ", speed, " cm ", unit^-1),
                                               list(speed=round(speed_cm_s, 2), unit="s"))
                            text(x=mean(x[[i]]), y=y_at[length(y_at)], 
                                 labels=as.expression(text), adj=0.5, cex=1, font=2)
                        }
                    } # WKB horizontal velocity lm fit

                    # 19.5 °C isotherm lm fit
                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        
                        # 1st line
                        ab <- c(1952.11144132, -0.06849022)
                        abline(a=ab[1], b=ab[2], lwd=2)
                        speed_cm_s <- 4.390902
                        if (T) {
                            text <- substitute(paste(u[LR], " = ", speed, " cm ", unit^-1),
                                               list(speed=round(speed_cm_s, 2), unit="s"))
                            text(x=mean(x[[i]]), 
                                 y=y_at[length(y_at)],
                                 labels=as.expression(text), adj=0.5, cex=1, font=2)
                        }

                        # 2nd line: smooth_theta = c(1, 0.15)
                        if (F) {
                            ab <- c(1943.72485189, -0.09769246)
                            abline(a=ab[1], b=ab[2], lwd=2)
                            speed_cm_s <- 3.086168
                            if (T) {
                                text <- substitute(paste(#bold(u)[LM], 
                                                         u[LR],
                                                         " = ", speed, " cm ", unit^-1),
                                                   list(speed=round(speed_cm_s, 2), unit="s"))
                                text(x=mean(x[[i]]), 
                                     y=y_at[3],
                                     labels=as.expression(text), adj=0.5, cex=1, font=2)
                            }
                        }

                        # 3nd line: smooth_theta = c(1, 0.15)
                        ab <- c(1943.9474806, -0.1313065)
                        abline(a=ab[1], b=ab[2], lwd=2)
                        speed_cm_s <- 2.264085
                        if (T) {
                            text <- substitute(paste(u[LR], " = ", speed, " cm ", unit^-1),
                                               list(speed=round(speed_cm_s, 2), unit="s"))
                            text(x=mean(x[[i]]), 
                                 y=y_at[6],
                                 labels=as.expression(text), adj=0.5, cex=1, font=2)
                        }

                    } # 19.5 °C isotherm lm fit

                    # radon fit velocities
                    if (T && any(dot_names == "radon_speeds")) {
                        radon_speeds <- dot_list[["radon_speeds"]]
                        if (any(dot_names == "radon_box")) {
                            radon_box <- dot_list[["radon_box"]]
                        }
                        if (regexpr("H5", znames[i]) != -1) {
                        #if (!is.na(radon_speeds[i])) { 
                            #message("add radon speed = ", radon_speeds[i], " ...")
                            # indicate region of readon calculation
                            if (F && exists("radon_box")) {
                                rect(radon_box[[i]][1], radon_box[[i]][3], 
                                     radon_box[[i]][2], radon_box[[i]][4])
                            }
                            segments(xlim[2], ylim[1],
                                     xlim[2] - radon_speeds[i] * diff(ylim),       
                                     ylim[2])
                            if (T && any(dot_names == "radon_sd_speeds")) {
                                radon_sd_speeds <- dot_list[["radon_sd_speeds"]]
                                #message("add radon sd speed = ", radon_sd_speeds[i], " ...")
                                segments(xlim[2], ylim[1],
                                         xlim[2] - (radon_speeds[i] + radon_sd_speeds[i]) * diff(ylim), ylim[2],
                                         lty=2)
                                segments(xlim[2], ylim[1],
                                         xlim[2] - (radon_speeds[i] - radon_sd_speeds[i]) * diff(ylim), ylim[2],
                                         lty=2)
                            } # if radon_sd_speeds
                        } # if not NA
                    } # if radon_speeds

                    # ssh lm fit
                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(-48.36050, 1948.000)
                        to <- c(-80.91727, 1950.614)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_cm_s <- 3.78968
                        text <- substitute(paste("|", bold(u), "| = ", speed, " cm ", unit^-1),
                                           list(speed=round(speed_cm_s, 2), unit="s"))
                        if (F) {
                            text(x=mean(c(from[1], to[1])),
                                 y=to[2] + 0.05*diff(ylim),
                                 text, adj=0.5, cex=1)
                        }
                    } # ssh fit

                    # ssh
                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(-45.66910, 1951.561)
                        to <- c(-73.16634, 1954.129)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_cm_s <- 3.26146420
                        text <- substitute(paste("|", bold(u), "| = ", speed, " cm ", unit^-1),
                                           list(speed=round(speed_cm_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)

                        from <- c(-31.61495, 1953.786)
                        to <- c(-52.08512, 1956.273)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_cm_s <- 2.50969579
                        text <- substitute(paste("|", bold(u), "| = ", speed, " cm ", unit^-1),
                                           list(speed=round(speed_cm_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)
                    } # ssh

                    # horbarstreamf
                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(-35.12849, 1948.251)
                        to <- c(-59.41772, 1950.495)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_cm_s <- 3.29802776
                        text <- substitute(paste("|", bold(u), "| = ", speed, " cm ", unit^-1),
                                           list(speed=round(speed_cm_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)
                    } # horbarstreamf

                    # 19.5°C isotherme depth at 30° N in North Atlantic
                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(-57.89010, 1949.135)
                        to <- c(-67.05584, 1950.146)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_cm_s <- 2.76738650
                        text <- substitute(paste("|", bold(u), "| = ", speed, " cm ", unit^-1),
                                           list(speed=round(speed_cm_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)

                        from <- c(-48.5715861834847, 1955.54233853961)
                        to <- c(-62.778494441144, 1956.53842173192)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_cm_s <- 4.35244704
                        text <- substitute(paste("|", bold(u), "| = ", speed, " cm ", unit^-1),
                                           list(speed=round(speed_cm_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)

                        from <- c(-56.20971, 1953.875)
                        to <- c(-64.15336, 1954.878)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_cm_s <- 2.41680232
                        text <- substitute(paste("|", bold(u), "| = ", speed, " cm ", unit^-1),
                                           list(speed=round(speed_cm_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)
                    } # isotherme

                } # rossby

                # kelvin_EqPac
                if (F) {

                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(96.93963, 1949.963)
                        to <- c(125.13402, 1950.549)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_m_s <- 0.1694675
                        text <- substitute(paste("|", bold(u), "| = ", speed, " m ", unit^-1),
                                           list(speed=round(speed_m_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)

                        from <- c(138.8956, 1951.156)
                        to <- c(171.7890, 1951.277)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_m_s <- 0.9556082
                        text <- substitute(paste("|", bold(u), "| = ", speed, " m ", unit^-1),
                                           list(speed=round(speed_m_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)

                        from <- c(56.99759, 1952.714)
                        to <- c(86.19892, 1952.774)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_m_s <- 1.696692
                        text <- substitute(paste("|", bold(u), "| = ", speed, " m ", unit^-1),
                                           list(speed=round(speed_m_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)

                        from <- c(53.93897, 1948.799)
                        to <- c(83.57913, 1948.933)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_m_s <- 0.7779232
                        text <- substitute(paste("|", bold(u), "| = ", speed, " m ", unit^-1),
                                           list(speed=round(speed_m_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)

                    } # kelvin_EqPac

                    # kelvin_EqAtl
                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(-28.907494, 1949.316)
                        to <- c(-3.644403, 1949.761)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_m_s <- 0.2
                        text <- substitute(paste("|", bold(u), "| = ", speed, " m ", unit^-1),
                                           list(speed=round(speed_m_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)
                    } # kelvin_EqAtl

                    # kelvin_EastAtl2
                    if (F && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(5.301791, 1949.430)
                        to <- c(13.135110, 1949.677)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_m_s <- 0.1118192
                        text <- substitute(paste("|", bold(u), "| = ", speed, " m ", unit^-1),
                                           list(speed=round(speed_m_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)
                    } # kelvin_EastAtl2

                    # kelvin_WestAtl
                    if (T && (znames[i] == "H1" || znames[i] == "H5")) {
                        from <- c(9.577463, 1949.331)
                        to <- c(20.612959, 1949.781)
                        segments(from[1], from[2], to[1], to[2], lwd=3)
                        speed_m_s <- 0.08637931
                        text <- substitute(paste("|", bold(u), "| = ", speed, " m ", unit^-1),
                                           list(speed=round(speed_m_s, 2), unit="s"))
                        text(x=mean(c(from[1], to[1])),
                             y=to[2] + 0.05*diff(ylim),
                             text, adj=0.5, cex=1)
                    } # kelvin_WestAtl

                } # kelvin

            } # add something special to subplot
        
        } # if length(x) != nplots - 1 && i != nplots

    } # for i n*m subplots

    ## draw legend
	#savepar <- par(mar=c(4, 0.5, 4, 0.5),
    #               xaxs="i", yaxs="i", lwd=lwd)
    if (!contour_only) { # only if needed
        
        if (verbose) message("\nplot.new() for colorbar plot ...")
        plot.new()

        if (horizontal) {
            stop("not yet")
        
        } else if (!horizontal) {

            if (verbose) message("vertical colorbar")
            y <- 1:nlevels
            y_midpoints <- (y[1:(nlevels - 1)] + y[2:nlevels])/2
            if (length(unique(diff(y_midpoints))) != 1) {
                if (verbose) message("warning: steps in 'y_midpoints' are uneven. use the first one ...")
            }
            y_midpoints <- c(y_midpoints[1] - diff(y_midpoints)[1], y_midpoints)

            if (F) {
                plot.window(xlim=c(1, 2),  # (0, 1) can be anything here, just to have a dimension
                            ylim=c(1, nlevels)
                            )
            }
 
            if (T) {
                #par(mar=c(2,0.5,2,10))
                if (verbose) message("par(mai)")
                par(mai=c(1, # translate from cm to inches
                          colorbar_dist_cm, 
                          1,
                          cm_right - colorbar_width_cm - colorbar_dist_cm)/cm(1))
                if (verbose) {
                    cat("fin=")
                    dput(par("fin"))
                    cat(paste0("in cm = c(", paste0(cm(par("fin")), collapse=","), ")\n")) 
                    cat("oma=")
                    dput(par("oma"))
                    cat("omi=")
                    dput(par("omi"))
                    cat("mar=")
                    dput(par("mar"))
                    cat("mai=")
                    dput(par("mai"))
                    cat("usr=")
                    dput(par("usr"))
                    cat("    c(x1, x2, y1, y2)\n")
                    cat("plt=")
                    dput(par("plt"))
                    cat("    c(x1, x2, y1, y2)\n")
                    cat("pty=")
                    dput(par("pty"))
                    message("y_midpoints:")
                    cat(capture.output(str(y_midpoints)), sep="\n")
                    cat("range(y_midpoints)=")
                    dput(range(y_midpoints))
                    message("breaks:")
                    cat(capture.output(str(breaks)), sep="\n")
                    cat("range(breaks)=")
                    dput(range(breaks))
                    message("axis.at:")
                    print(axis.at)
                    message("axis.labels:")
                    cat(capture.output(str(axis.labels)), sep="\n")
                }
                temp <- imageplot.setup(add=T)
                plt <- temp$bigplot
                if (verbose) {
                    message("temp:")
                    cat(capture.output(str(temp)), sep="\n")
                }
            }

            if (F) {
                plot.window(xlim=c(1, 2),  # (0, 1) can be anything here, just to have a dimension
                            ylim=c(1, nlevels)
                            )
            }

            # open colorbar plot
            if (verbose) message("par(new=T)")
            par(new=T, pty="m", err=-1)
            ix <- 1:2
            if (F) { # as in fields::image.plot()
                if (verbose) message("iy = breaks !!!!!!!!!!!!!!!!!!!!")
                iy <- breaks # use levels as indices in colorbar
                # constant dy for useRaster=T usage
                # --> maybe not possible due to unequal zlevels, e.g. c(zlim[1], 2, 3, zlim[2])
                #iy <- seq(min(iy), max(iy), l=length(iy)) 
                colorbar_breaks <- breaks
                colorbar_at <- axis.at
            } else if (T) {
                #if (verbose) message("iy = axis.at.ind !!!!!!!!!!!!!!!!!!!!")
                #iy <- axis.at.ind # use indices as indices in colorbar
                if (verbose) message("iy = seq_len(nlevels) !!!!!!!!!!!!!!!!!!!!")
                iy <- seq_len(nlevels)
                colorbar_breaks <- seq_len(nlevels)
                colorbar_at <- axis.at.ind
            }
            midpoints <- (iy[1:(length(iy) - 1)] + iy[2:length(iy)])/2
            iz <- matrix(midpoints, nrow=1, ncol=length(midpoints))
            if (verbose) {
                message("ix:")
                cat(capture.output(str(ix)), sep="\n")
                cat("range(ix) = ")
                dput(range(ix))
                message("iy:")
                cat(capture.output(str(iy)), sep="\n")
                cat("range(iy) = ")
                dput(range(iy))
                message("iz:")
                cat(capture.output(str(iz)), sep="\n")
                cat("range(iz) = ")
                dput(range(iz))
            }

            # add colorbar
            image(ix, iy, iz,
                  breaks=colorbar_breaks,
                  col=cols,
                  axes=F, lwd=lwd, 
                  xlab=NA, ylab=NA
                  #, useRaster=T # does not work through iy
                  )
            
            # add colorbar axis labels
            axis.args <- c(list(side=ifelse(horizontal, 1, 4),
                                #, mgp=c(3, 1, 0), 
                                las=ifelse(horizontal, 0, 2),
                                at=colorbar_at,
                                labels=axis.labels,
                                lwd=0, lwd.ticks=lwd.ticks,
                                cex.axis=colorbar.cex),
                           axis.args)
            if (!is.null(axis.args)) {
                if (verbose) message("do.call(\"axis\", axis.args) ...")
                do.call("axis", axis.args)
            }
           
            # add colorbar text
            #axis(4, at=axis.at, labels=axis.labels, las=2, 
            #     lwd=0, lwd.ticks=lwd.ticks, cex.axis=1.5)
            legend.args <- list(text=as.expression(zlab), 
                                side=ifelse(horizontal, 1, 4), 
                                line=legend.line, cex=legend.cex)
            #mtext(side=4, line=legend.line, text=zlab, cex=cex.axis)
            if (!is.null(legend.args)) {
                if (verbose) message("do.call(mtext, legend.args) ...")
                do.call(mtext, legend.args)
            }

        } # if horizontal or not

        box(lwd=lwd)
        #par(savepar)
    } # if !contour_only
    
    ## Save plot
    if (type != "active") {
        if (verbose) message("save ", plotname, " ...")
        dev.off()
    }

    return(return_list)

} # function end

## https://stackoverflow.com/questions/30765866/get-margin-line-locations-in-log-space/30835971#30835971
line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         `1` = grconvertY(-line * y_off, 'npc', 'user'),
         `2` = grconvertX(-line * x_off, 'npc', 'user'),
         `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
         `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}

if (F) {
  ## from the image.plot help: 
  # figs: matrix with 4 columns: each row describes a screen with
  # values for the left, right, bottom, and top of the screen (in
  # that order) in NDC units, that is 0 at the lower left corner
  # of the device surface, and 1 at the upper right corner.
  figs <- rbind(c(0, 1, 0.15, 1), # 1st subplot on top: map and colorbar
                c(0, 1, 0, 0.15)) # 2nd subplot below: boxplot

  # open the 2 screens
  split.screen(figs=figs) 
  
  # activate 1st screen: map plot and colorbar
  screen(1)
  
  # increase lower margin of 1st. subplot for colorbar
  par(mar=c(8.1, 4.1, 4.1, 2.1)) # default: 5.1, 4.1, 4.1, 2.1

  plot(1)

  # activate 2nd screen: boxplot
  screen(2)
  
  # close screens 
  close.screen(all=T)
  
  # save plot
  dev.off()
  
  # get coordinates of colorbar in user coordinates (from image.plot)
  old.par <- par(no.readonly=T)
  bigplot <- old.par$plt
  ndc_coords <- imageplot.setup(add=T, legend.mar=distance_of_colorbar_from_mapplot, 
                                legend.width=legend.width, horizontal=T, bigplot=bigplot) 
  # -> ndc = normalized device coordinates
  # -> other parameters of the image.plot(legend.only=T) call may need to handed to imageplot.setup()
  usr_coords <- grconvertX(ndc_coords$bigplot[1:2], from="ndc", to="user")
  usr_coords[3:4] <- grconvertY(ndc_coords$bigplot[3:4], from="ndc", to="user")
  message("bigplot:")
  message(usr_coords)
  rect(usr_coords[1], usr_coords[3], usr_coords[2], usr_coords[4], border="red", xpd=T) 
  usr_coords <- grconvertX(ndc_coords$smallplot[1:2], from="ndc", to="user")
  usr_coords[3:4] <- grconvertY(ndc_coords$smallplot[3:4], from="ndc", to="user")
  rect(usr_coords[1], usr_coords[3], usr_coords[2], usr_coords[4], border="red", xpd=T) 
  message("smallplot:")
  message(usr_coords)
  
} # split.screen!!!
