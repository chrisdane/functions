image.plot.nxm <- function(x, y, z, n, m, dry=F, 
                           horizontal=F, top_bottom=F, add_title=T,
                           xlab="xaxis", ylab="yaxis", zlab="Variable [unit]",
                           cex.axis=1.25,
                           bgcol="white", NAcol="gray", useRaster=NULL,
                           add_contour=T, add_land=F, 
                           contour_only=F, contour_labcex=1,
                           image_list=NULL, contour_list=NULL, quiver_list=NULL, 
                           type="active", plotname="testplot",
                           cm_bottom=2, cm_left=2.5, cm_top=1, cm_right=4,
                           colorbar_width_cm=0.45, colorbar_dist_cm=0.2,
                           width_png=2000, height_png=1666, res=300, 
                           width_pdf=7, height_pdf=7,
                           axis.args=NULL, add_names_inset=F, add_names_topleft=T,
                           legend.args=NULL, legend.line=7, legend.cex=1.25,
                           family="sans", lwd=0.5, lwd.ticks=0.5, 
                           verbose=F,
                           ...) {

    ## Demo values
    if (missing(x) && missing(y) && missing(z)) {
        if (missing(n) || missing(m)) {
            n <- 3
            m <- 2
        }
        x <- y <- z <- vector("list", l=n*m)
        for (i in 1:(n*m)) {
            x[[i]] <- 1:20
            y[[i]] <- 1:20
            z[[i]] <- array(rnorm(length(x[[i]])*length(y[[i]])),
                            c(length(x[[i]]), length(y[[i]])))
            z[[i]][8:13,8:13] <- NA
        }
    } # demo
   
    # capture additional arguments (aka ellipsis, dots, ...)
    dot_list <- list(...)
    ndots <- length(dot_list)
    dot_names <- names(dot_list)
    if (ndots > 0) {
        if (verbose) {
            for (i in 1:length(dot_list)) {
                message("dots[[", i, "]]: ", dot_names[i])
                cat(capture.output(str(dot_list[[i]])), sep="\n")
            }
        }
    }
    
    # necessary input
    if (missing(x)) stop("x is missing")
    if (missing(y)) stop("y is missing")
    if (missing(z)) stop("z is missing")
    if (!is.list(x)) stop("x must be list")
    if (!is.list(y)) stop("y must be list")
    if (!is.list(z)) stop("z must be list")
    if (!is.null(useRaster)) { # if useRaster was given
        if (!is.logical(useRaster)) {
            stop("'useRaster' must be either T or F.")
        }
    }

    # figure out nrow and ncol aready here that dry run is faster
    if (missing(n) && missing(m)) { # default
        # just learned: grDevices::n2mfrow(nplots) is doing the trick
        if (length(z) == 1) {
            n <- 1 # 1 row
            m <- 1 # 1 col
        } else if (length(z) == 2) {
            # what should be default?
            if (F) { # 1 row, 2 cols
                n <- 1
                m <- 2
            } else if (T) { # 2 rows, 1 col
                n <- 2
                m <- 1
            }
        } else if (length(z) == 3) {
            n <- 3
            m <- 1
        } else {
            n <- ceiling(sqrt(length(z)))
            m <- length(z) - n
        }
        if (verbose) message("automatically derived (nrow,ncol) = (n,m) = (", n, ",", m, ")") 
    } # if n and m are missing
    if (verbose) {
        message("   n x m = ", n, " x ", m, " ...")
    }
    nplots <- n*m
    if (nplots < length(z)) {
        stop("nplots = n x m = ", n, " x ", m, " < length(z) = ", length(z))
    }

    # decide which axes are drawn to which subplot
    left_axis_inds <- bottom_axis_inds <- title_inds <- rep(F, t=nplots)
    for (i in seq_len(nplots)) {
        if (top_bottom) { # order plots from top to bottom and then from left to right (default: False)
            
            if (verbose) message("axis top_bottom")
            # title in top row
            if (i == n*(m-1)+1) title_inds[i] <- T
            
            # n=nrow, m=ncol
            if (i <= n) { # left axis
                if (verbose) message("axis top_bottom i=", i, " <= n (=", n, ")")
                left_axis_inds[i] <- T
            }
            if (i %% n == 0) { # bottom axis
                if (verbose) message("axis top_bottom i=", i, " %% n (=", n, ") = ", i %% n, " == 0")
                bottom_axis_inds[i] <- T
            }

        } else if (!top_bottom) { # order plots from left to right and then from top to bottom (default: True)
            
            # title in top row
            if (i == m) title_inds[i] <- T

            # n=nrow, m=ncol
            if (verbose) message("i %% m = ", i, " %% ", m, " = ", i %% m)
            if (nplots == 1 || m == 1 ||
                (nplots > 1 && i %% m == 1)) { # left axis
                if (verbose) message("axis !top_bottom i=", i, " %% m (=", m, ") = ", i %% m, " == 1")
                left_axis_inds[i] <- T
            }
            if (i >= (n*m - m + 1)) { # bottom axis
                if (verbose) message("axis !top_bottom i=", i, " >= (n*m - m + 1) = (", n, "*", m, " - ", m, " + 1) = ", (n*m - m + 1))
                bottom_axis_inds[i] <- T
            }
        } # if top_bottom
    } # for i nplots

    # just return number of rows and columns
    if (dry) {
        return(list(nrow=n, ncol=m, top_bottom=top_bottom,
                    left_axis_inds=left_axis_inds, bottom_axis_inds=bottom_axis_inds,
                    title_inds=title_inds))
    }

    # make sure that x and y are regular that image(..., useRaster=T) can be used
    if (F) {
        message("make regular x,y coords ...")
        for (i in 1:length(z)) {
            x[[i]] <- seq(min(x[[i]], na.rm=T), max(x[[i]], na.rm=T), l=length(x[[i]]))
            y[[i]] <- seq(min(y[[i]], na.rm=T), max(y[[i]], na.rm=T), l=length(y[[i]]))
        }
    }

    if (verbose) {
        message("x")
        cat(capture.output(str(x)), sep="\n")
        message("y")
        cat(capture.output(str(y)), sep="\n")
    }

    # derive nplots based on derived number of rows (n) and columns (m)
    if (add_names_inset && add_names_topleft) {
        stop("decide where to put legend: either `add_names_inset` or `add_names_topleft` can be true")
    }
   
    # check additional data lists if provided
    if (!is.null(image_list)) { # todo
    }
    
    if (!is.null(contour_list)) { # todo
    }

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

    ## checks finished

    # data names if existing
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

    # levels and colors
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

    # Prepare plot
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
    if (!any(dot_names == "y_at")) {
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

    # construct layout mat based on n x m; nrow x ncol
    if (top_bottom) { # plot figures from top to bottom and left to right
	    layout_mat <- matrix(1:(n*m), nrow=n, ncol=m, byrow=F)
    } else { # plot figures from left to right and topto bottom
        layout_mat <- matrix(1:(n*m), nrow=n, ncol=m, byrow=T)
    }

	# vertical legend bar on the right
    if (!horizontal) {
        layout_mat2 <- cbind(rep(0, t=n), # left axis row
                             layout_mat,
                             rep(n*m + 1, t=n)) # right legend column
        layout_mat2 <- rbind(rep(0, t=m + 2),
                             layout_mat2,
                             rep(0, t=m + 2)) # bottom axis row

        # left region for axes in cm; columns for plots in relative units; right region for colorbar in cm  
        layout_widths <- c(lcm(cm_left), rep(1/m, t=m), lcm(cm_right))
        # upper region for title in cm; rows for plots in relative units; lower region for axes in cm
        layout_heights <- c(lcm(cm_top), rep(1/n, t=n), lcm(cm_bottom))

    # horizontal legend bar on the bottom
    } else {
        stop("not yet") 
    }
    if (verbose) {
        cat("layout_widths=")
        dput(layout_widths)
        cat("layout_heights=")
        dput(layout_heights)
        cat("layout_mat2=")
        dput(layout_mat2)
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
    
    if (verbose) message("open image plot ...")
    
    layout(layout_mat2, widths=layout_widths, heights=layout_heights)
    #layout.show(n=max(layout_mat2))
    par(mar=rep(0.5, t=4)) # distance between sub-figures [rows]
	if (add_names_topleft) { # increase vertical distance between sub figures
        par(mar=c(1.5, 0.5, 1.5, 0.5)) 
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

        if (verbose) message("plot ", i, "/", length(x), " ...")
        
        # Open plot device
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

            # Add data to plot
            nx <- length(x[[i]]); ny <- length(y[[i]])

            if (!contour_only) {

                # check useRaster
                err <- tryCatch(image(x[[i]], y[[i]], 
                                      array(1, c(length(x[[i]]), length(y[[i]]))),
                                      add=T, col=NAcol, axes=F, xlab=NA, ylab=NA, 
                                      useRaster=T),
                                error=function(e) e, warning=function(w) w)
                if (is.null(err)) {
                    useRaster <- T
                } else {
                    if (verbose) {
                        if (i == 1) message("cannot use useRaster=T since:")
                        message(err)
                    }
                    interpolate_to_regular <- F
                    if (interpolate_to_regular) {
                        message("`interpolate_to_regular`=T ...")
                        # todo
                    } else {
                        useRaster <- F
                    }
                }
                
                # add NA values
                if (!useRaster) { 
                    if (verbose) message("image() 1")
                    image(x[[i]], y[[i]], array(1, c(nx, ny)),
                          add=T, col=NAcol,
                          axes=F, xlab="n", ylab="n",
                          useRaster=useRaster)
                }

                # add actual data
                if (verbose) message("image() 2")
                image(x[[i]], y[[i]], z[[i]], 
                      add=T, col=cols, breaks=breaks,
                      axes=F, xlab="n", ylab="n", 
                      useRaster=useRaster)

                # add contour to plot
                if (add_contour) {
                    tmp <- axis.at
                    if (F && any(tmp == 0)) { # do not show zero contour
                        if (i == 1) message("add_contour = T --> add data contour without zero ...")
                        tmp <- tmp[-which(tmp == 0)]
                    } else {
                        if (i == 1) message("add_contour = T --> add data contour with zero ...")
                    }
                    contour(x[[i]], y[[i]], z[[i]],
                            levels=tmp, lwd=lwd, add=T, labcex=contour_labcex)
                }
                
            } else if (contour_only) {

                contour(x[[i]], y[[i]], z[[i]],
                        levels=axis.at, labcex=contour_labcex)

            } # if contour_only or not
            
            # add additional data as image if available
            if (!is.null(image_list)) {
                if (i == 1) message("\n`contour_list` is not NULL -> add additional data as image() to plot ...")
                if (length(image_list[[i]]$levels) == 1) { # special case: only 1 level
                    image(x[[i]], y[[i]], image_list[[i]]$data, 
                          col=image_list[[i]]$cols,  
                          add=T, useRaster=useRaster)
                } else {
                    image(x[[i]], y[[i]], image_list[[i]]$data, 
                          col=image_list[[i]]$cols, breaks=image_list[[i]]$levels, 
                          add=T, useRaster=useRaster)
                }
            } # if (!is.null(image_list))

            # add additional data as contours if available
            if (!is.null(contour_list)) {
            if (i == 1) message("\n`contour_list` is not NULL -> add additional data as contours() to plot ...")
                contour(x[[i]], y[[i]], contour_list[[i]], 
                        add=T, levels=contour_list[[i]]$levels,
                        lwd=lwd, labcex=contour_labcex, drawlabels=T)
            } # if (!is.null(contour_list))
            
            # add additional data as quivers if available
            if (!is.null(quiver_list)) {
                message("\n`quiver_list` is not NULL -> add additional data as quivers via pracma::quiver() to plot ...")
                if (i == 1) {
                    message("quiver_list:")
                    cat(capture.output(str(quiver_list)), sep="\n")
                }
                if (!any(search() == "package:abind")) library(pracma)
                xmat <- array(x[[i]], c(nx, ny))
                ymat <- t(array(y[[i]], c(ny, nx)))
                quiver_inds <- array(F, c(nx, ny))
                if (verbose) message("nx_fac = ", quiver_list$nx_fac[i], ", ny_fac = ", 
                                     quiver_list$ny_fac[i], " --> draw ", quiver_list$nx_fac[i]*100, 
                                     " and ", quiver_list$ny_fac[i]*100,  
                                     " % of all possible quivers in x- and y-direction ...")
                quiver_inds_x <- seq(1, nx, l=quiver_list$nx_fac[i]*nx)
                quiver_inds_y <- seq(1, ny, l=quiver_list$ny_fac[i]*ny)
                quiver_inds[quiver_inds_x,quiver_inds_y] <- T
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
                               col=quiver_list$col[i])
            } # if (!is.null(quiver_list))
            
            # add text to every plot
            if (verbose) message("check dots for \"addtext_list\"")
            if (any(dot_names == "addtext_list")) {
                for (j in 1:length(addtext_list)) {
                    if (typeof(addtext_list[[j]]) == "character") {
                        if (verbose) message("add \"", addtext_list[[j]], "\" ...")
                        eval(parse(text=addtext_list[[j]]))
                    }
                }
            }

            # add title
            if (add_title && any(dot_names == "title") && title_inds[i]) {
                if (verbose) message("title()")
                text(x=line2user(line=mean(par('mar')[c(2, 4)]), side=2), 
                     y=line2user(line=2, side=3), 
                     labels=dot_list[["title"]], xpd=NA, 
                     cex=1.5, font=1) # font=2 for bold
            }

            # add axes and axes labels
            if (verbose) message("axis()")
            if (left_axis_inds[i]) {
                axis(2, at=y_at, labels=y_labels, las=2, cex.axis=cex.axis, 
                     lwd=0, lwd.ticks=lwd.ticks)
                mtext(ylab, side=2, line=5, cex=1)
            } else { # just ticks
                axis(2, at=y_at, labels=F, lwd=0, lwd.ticks=lwd.ticks)
            }
            if (bottom_axis_inds[i]) {
                axis(1, at=x_at, labels=x_labels, cex.axis=cex.axis, 
                     lwd=0, lwd.ticks=lwd.ticks)
                mtext(xlab, side=1, line=3, cex=1)
            } else { # just ticks
                axis(1, at=x_at, labels=F, lwd=0, lwd.ticks=lwd.ticks)
            }

            # add land
            if (add_land != F) {
                if (i == 1) message("maps::map(", add_land, ")")
                if (!any(search() == "package:maps")) library(maps)
                maps::map(add_land, interior=F, add=T)
            }

            # add name to every plot
            if (add_names_inset || add_names_topleft) {

                if (add_names_inset) {
                    
                    if (i == 1) message("`add_names_inset` = T --> legend()")
                    # prepare leend
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
                    
                    # ad text
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
                               cex=cex.axis, bty="n")
                    }

                # if add_names_topleft finished
                } else if (add_names_topleft) {

                    if (i == 1) message("`add_names_topleft` = T --> add `znames` on top left corner of each plot")
                    text(par("usr")[1], par("usr")[4] + strheight("."), 
                         xpd=T, labels=znames[i], pos=4, cex=legend.cex)

                } # add_names_inset or add_names_topleft

            } # if (add_names_inset || add_names_topleft)
           
            # draw box around plot
            box(lwd=lwd)

            ## overlay a subplot
            if (verbose) message("check dots for 'subplot' ...")
            if (any(dot_names == "subplot")) {

                for (spi in 1:length(subplot)) {

                    if (!any(search() == "package:TeachingDemos")) library(TeachingDemos)

                    if (top_bottom) {
                        # add subplot axis only in last column
                        draw_axis_labels_inds <- 2 # need to fix that
                    }

                    #op <- par(no.readonly=T) # switch back to main plot with 'par(op)'
                    sb <- subplot(fun=subplot[[spi]]$fun(sb=subplot[[spi]], 
                                                         draw_axis_labels_inds=draw_axis_labels_inds, i=i, lwd=lwd),
                                  x=grconvertX(subplot[[spi]]$fig_x, from="npc"),
                                  y=grconvertY(subplot[[spi]]$fig_y, from="npc"),
                                  type="plt")
                    #par(op) # switch back to main plot

                } # for spi in subplot 

            } # if any(dot_names == "subplot")

            ## add something special to plot
            if (F) {
                message("add special stuff ... todo: give as argument list")

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

            } # add something special to plot
        
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
            if (T) { # as in fields::image.plot()
                message("iy = breaks !!!!!!!!!!!!!!!!!!!!")
                iy <- breaks # use levels as indices in colorbar
            } else if (F) {
                message("iy = axis.at.ind !!!!!!!!!!!!!!!!!!!!")
                iy <- axis.at.ind # use indices as indices in colorbar
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
            if (verbose) message("image(breaks=breaks)")
            #if (verbose) message("image(breaks=1:nlevels)")
            image(ix, iy, iz,
                  #y=y_midpoints,
                  #z=array(breaks, c(1, nlevels)),
                  breaks=breaks, 
                  #breaks=1:nlevels,
                  col=cols,
                  axes=F, lwd=lwd,
                  #, add=T
                  )
            
            # add my axis formats to user provided 'axis.args'
            axis.args <- c(list(side=ifelse(horizontal, 1, 4),
                                #, mgp=c(3, 1, 0), 
                                las=ifelse(horizontal, 0, 2),
                                at=axis.at,
                                #at=axis.at.ind, 
                                labels=axis.labels,
                                lwd=0, lwd.ticks=lwd.ticks,
                                cex.axis=1.5),
                           axis.args)
            if (!is.null(axis.args)) {
                if (verbose) message("do.call(\"axis\", axis.args) ...")
                do.call("axis", axis.args)
            }
           
            # add colorbar label
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

    return(list(zlim=zlim, breaks=breaks, col=cols, nlevels=nlevels,
                axis.at=axis.at, axis.labels=axis.labels,
                layout_mat=layout_mat, layout_mat2=layout_mat2,
                layout_widths=layout_widths, layout_heights=layout_heights))

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
