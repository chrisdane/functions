plot.nxm <- function(x, y, n, m,
                     dry=F, 
                     types, cols, ltys, lwds, pchs, cexs, 
                     one_xlim=F, one_ylim=F,
                     add_zerolines=T, zeroline_col="gray", zeroline_lty=1, zeroline_lwd=0.5,
                     plot_list=NULL,
                     top_bottom=F, add_title=T,
                     x_lab, y_lab,
                     axis_cex=1, axis_lwd=1, axis_tick_lwd=1,
                     type="active", plotname="testplot",
                     width_png=2666, height_png=2000, res=400, 
                     width_pdf=7, height_pdf=7,
                     axis.args=NULL, add_names_inset=T,
                     legend.args=NULL, legend.line=7, legend.cex=1,
                     family="sans", 
                     verbose=F,
                     ...) {
    ## Description
    # ip is result of image.plot.pre() 

    ## Demo values
    if (missing(x) && missing(y)) {
        if (missing(n) || missing(m)) {
            n <- 3
            m <- 2
        }
        x <- y <- vector("list", length.out=nplots)
        for (i in seq_len(nplots)) {
            x[[i]] <- 1:20
            y[[i]] <- rnorm(20)
        }
    } # demo
   
    # necessary input for dry run
    if (missing(x)) stop("x is missing")
    if (missing(y)) stop("y is missing")
    if (!is.list(x)) stop("x must be list")
    if (!is.list(y)) stop("y must be list")
    
    if (verbose) {
        message("x")
        message(str(x))
        message("y")
        message(str(y))
    }
    
    # figure out nrow and ncol aready here that dry run is faster
    if (missing(n) && missing(m)) { # default
        if (length(y) == 1) {
            n <- m <- 1 # row and col
        } else if (length(y) == 2) { # what should be default?
            if (F) { 
                n <- 1 # row
                m <- 2 # cols
            } else if (T) { 
                n <- 2 # rows
                m <- 1 # col
            }
        } else if (length(y) == 3) { 
            n <- 3 # rows
            m <- 1 # col
        } else {
            # https://stackoverflow.com/questions/4106935/determine-rows-columns-needed-given-a-number
            #cols = Math.floor( Math.sqrt(totalTiles) );
            #rows = Math.floor(  Math.ceil( totalTiles / cols ) );
            m <- floor(sqrt(length(y))) # cols
            n <- floor(ceiling(length(y)/m)) # rows
            if (verbose) {
                message("length(y) = ", length(y), "\n", 
                        " -> ncols m = floor(sqrt(", length(y), ")) = ", m, "\n",
                        " -> nrows n = floor(ceiling(", length(y), "/m)) = ", n)
            }
        }
        if (verbose) message("automatically derived (nrow,ncol) = (n,m) = (", n, ",", m, ")") 
    } # if n and m are missing
    
    # derive nplots based on derived number of rows (n) and columns (m)
    nplots <- n*m
    if (nplots < length(y)) {
        stop("this should not happen")
    }
    
    # construct layout mat based on n x m; nrow x ncol
    if (top_bottom) { # plot figures from top to bottom and left to right
        # 1 4
        # 2 5
        # 3 6
	    layout_mat <- matrix(seq_len(nplots), nrow=n, ncol=m, byrow=F)
    } else { # plot figures from left to right and topto bottom
        # 1 2
        # 3 4
        # 5 6
        layout_mat <- matrix(seq_len(nplots), nrow=n, ncol=m, byrow=T)
    }

    # columns for plots in relative units
    layout_widths <- rep(1/m, times=m)
    # rows for plots in relative units
    layout_heights <- rep(1/n, times=n)

    if (verbose) {
        cat("nrow x ncol = n x m = ", n, " x ", m, "\n")
        cat("layout_widths=")
        dput(layout_widths)
        cat("layout_heights=")
        dput(layout_heights)
        cat("layout_mat=")
        dput(layout_mat)
    }

    # just return number of rows and columns
    if (dry) {
        return(list(nrow=n, ncol=m, nplots=nplots, 
                    layout_widths=layout_widths, layout_heights=layout_heights, layout_mat=layout_mat))
    }

    ## start 
    
    # capture additional arguments (aka ellipsis, dots, ...)
    dot_list <- list(...)
    ndots <- length(dot_list)
    dot_names <- names(dot_list)
    if (ndots > 0) {
        if (verbose) {
            for (i in 1:length(dot_list)) {
                print(paste0("   dots[", i, "]: ", dot_names[i]))
                ## Note: 
                # print(str(dot_list[[i]]))
                # returns the value AND NULL
                str(dot_list[[i]]) 
            }
        }
    }

    ## data names if existing
    if (any(dot_names == "ynames")) {
        ynames <- dot_list[["ynames"]]
    } else { # default: a) 1, b) 2, ...
        ynames <- names(y)
        if (is.null(ynames)) {
            ynames <- rep("", times=nplots)
            for (i in seq_len(nplots)) {
                ynames <- paste0(letters[i], ") ", seq_len(nplots))
            }   
        }
    }

    ## Prepare plot
    if (missing(cols)) stop("cols is missing")
    if (missing(ltys)) stop("ltys is missing")
    if (missing(lwds)) stop("lwds is missing")
    if (missing(pchs)) stop("pchs is missing")
    if (missing(types)) types <- rep("l", times=nplots)
    if (missing(cexs)) cexs <- rep(1, times=nplots)
    if (missing(x_lab)) {
        x_lab <- vector("list", length.out=nplots)
        x_lab[] <- "x_lab"
    }
    if (missing(y_lab)) {
        y_lab <- vector("list", length.out=nplots)
        y_lab[] <- "y_lab"
    }

    if (!any(dot_names == "xlim")) {
        xlim <- lapply(x, range, na.rm=T)
        if (verbose) cat("automatic xlim =\n"); print(sapply(xlim, "["))
    } else if (any(dot_names == "xlim")) {
        xlim <- dot_list[["xlim"]]
        if (!is.list(xlim)) stop("xlim needs to be a list")
        if (verbose) cat("provided xlim =\n"); print(sapply(xlim, "["))
    } # if xlim is provided or not
    
    if (!any(dot_names == "ylim")) {
        ylim <- lapply(y, range, na.rm=T)
        if (verbose) cat("automatic ylim =\n"); print(sapply(ylim, "["))
    } else if (any(dot_names == "ylim")) {
        ylim <- dot_list[["ylim"]]
        if (!is.list(ylim)) stop("ylim needs to be a list")
        if (verbose) cat("provided ylim =\n"); print(sapply(ylim, "["))
    } # if ylim is provided or not

    if (!any(dot_names == "x_at")) {
        x_at <- vector("list", length.out=nplots)
    } else {
        x_at <- dot_list[["x_at"]]
        if (!is.list(x_at)) stop("x_at needs to be a list")
        if (verbose) cat("provided x_at = "); sapply(x_at, "[")
    }
    if (!any(dot_names == "y_at")) {
        y_at <- vector("list", length.out=nplots)
    } else {
        y_at <- dot_list[["y_at"]]
        if (!is.list(y_at)) stop("y_at needs to be a list")
        if (verbose) cat("provided y_at = "); sapply(y_at, "[")
    }
    if (!any(dot_names == "x_labels")) {
        x_labels <- vector("list", length.out=nplots)
    } else {
        x_labels <- dot_list[["x_labels"]]
        if (!is.list(x_labels)) stop("x_labels needs to be a list")
        if (verbose) cat("provided x_labels = "); sapply(x_labels, "[")
    }
    if (!any(dot_names == "y_labels")) {
        y_labels <- vector("list", length.out=nplots)
    } else {
        y_labels <- dot_list[["y_labels"]]
        if (!is.list(y_labels)) stop("y_labels needs to be a list")
        if (verbose) cat("provided y_labels = "); sapply(y_labels, "[")
    }
    
    x_plot <- y_plot <- vector("list", length.out=nplots)
    for (i in seq_len(nplots)) { 
        if (verbose) message("prepare plot ", i, "/", nplots)
        l <- max(length(x[[i]]), length(y[[i]]))
        x_plot[[i]] <- seq(xlim[[i]][1], xlim[[i]][2], length.out=l)
        y_plot[[i]] <- seq(ylim[[i]][1], ylim[[i]][2], length.out=l)
        if (verbose) {
            cat("x_plot[[", i, "]] = ")
            message(str(x_plot[[i]]))
            cat("y_plot[[", i, "]] = ")
            message(str(y_plot[[i]]))
        }

        if (!any(dot_names == "x_at")) {
            x_at[[i]] <- pretty(x_plot[[i]], n=10)
            if (verbose) {
                cat("automatic x_at step 1 =")
                dput(x_at[[i]])
            }
            x_at[[i]] <- x_at[[i]][x_at[[i]] >= min(x_plot[[i]]) & x_at[[i]] <= max(x_plot[[i]])]
            if (verbose) {
                cat("automatic x_at step 2 =")
                dput(x_at[[i]])
            }
        }
        if (!any(dot_names == "y_at")) {
            y_at[[i]] <- pretty(y_plot[[i]], n=10)
            if (verbose) {
                cat("automatic y_at step 1 =")
                dput(y_at[[i]])
            }
            y_at[[i]] <- y_at[[i]][y_at[[i]] >= min(y_plot[[i]]) & y_at[[i]] <= max(y_plot[[i]])]
            if (verbose) {
                cat("automatic y_at step 2 =")
                dput(y_at[[i]])
            }
        }
        if (!any(dot_names == "x_labels")) {
            x_labels[[i]] <- format(x_at[[i]], trim=T)
            if (verbose) {
                cat("automatic x_labels =")
                dput(x_labels[[i]])
            }
        }
        if (!any(dot_names == "y_labels")) {
            y_labels[[i]] <- format(y_at[[i]], trim=T)
            if (verbose) {
                cat("automatic y_labels =")
                dput(y_labels[[i]])
            }
        }
    } # for i nplots
     
    # todo: mix of autmatic and user provided x_at/x_labels may be of different lengths

    # if class(xlim[[1]]) = c("POSIXct" "POSIXt"), make numeric out of it
    if (any(sapply(xlim, class) == "POSIXct")) {
        xlim <- lapply(xlim, unclass)
        if (verbose) cat("final xlim =\n"); print(sapply(xlim, "["))
    }
    if (any(sapply(ylim, class) == "POSIXct")) {
        ylim <- lapply(ylim, unclass)
        if (verbose) cat("final ylim =\n"); print(sapply(xlim, "["))
    }
   
    # maybe needed
    xlim_all <- range(xlim)
    ylim_all <- range(ylim)
    if (verbose) {
        cat("xlim_all = "); dput(xlim_all)
        cat("ylim_all = "); dput(ylim_all)
    }

    ## Open new or use already open plot device
    if (type == "active") {
        # open new device if none is open
        if (is.null(dev.list())) {
            dev.new(family=family)
        } # else use already open device (may be opened before in parent script ...)

    } else if (type == "png") {
        if (n < m) { # nrow < ncol
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
    
    # open big plot that will contain all subplots
    if (verbose) print("open plot ...")
    layout(layout_mat, widths=layout_widths, heights=layout_heights)
    #layout.show(n=max(layout_mat))
    par(mar=c(5.1, 4.1, 4.1, 2.1) + 0.1) # distance between sub-figures [rows]
    #par(mar=c(2, 2, 2, 2))
    if (F) {
        mypar()
        stop("asd")
    }
        
    if (verbose) {
        cat("fig="); dput(par("fig"))
        cat("fin="); dput(par("fin"))
        cat("oma="); dput(par("oma"))
        cat("omi="); dput(par("omi"))
        cat("mar="); dput(par("mar"))
        cat("mai="); dput(par("mai"))
        cat("usr="); dput(par("usr"))
        cat("plt="); dput(par("plt"))
        cat("pty="); dput(par("pty"))
    }

    # plot every subplot
	for (i in seq_len(nplots)) {

        if (verbose) message("plot() i ", i, "/", nplots)
        
        # xlim ylim
        xlimi <- xlim[[i]] # default
        ylimi <- ylim[[i]]
        if (one_xlim) xlimi <- xlim_all
        if (one_ylim) ylimi <- ylim_all
        
        # Open plot device
        plot(x_plot[[i]], y_plot[[i]], times="n",
			 xlim=xlimi, ylim=ylimi,
             xaxt="n", yaxt="n", 
             xlab=NA, ylab=NA)
      
        # add zero lines
        if (add_zerolines) {
            abline(h=0, col=zeroline_col, lty=zeroline_lty, lwd=zeroline_lwd)
            abline(v=0, col=zeroline_col, lty=zeroline_lty, lwd=zeroline_lwd)
        }

        # add data
        points(x[[i]], y[[i]], times=types[i], 
               lty=ltys[i], lwd=lwds[i],
               pch=pchs[i], cex=cexs[i],
               col=cols[i])

        # add axes and axes labels to every plot
        axis(1, at=x_at[[i]], labels=x_labels[[i]], cex.axis=axis_cex, 
             lwd=axis_lwd, lwd.ticks=axis_tick_lwd)
        mtext(x_lab[[i]], side=1, line=3, cex=1)
        axis(2, at=y_at[[i]], labels=y_labels[[i]], las=2, cex.axis=axis_cex,
             lwd=axis_lwd, lwd.ticks=axis_tick_lwd)
        mtext(y_lab[[i]], side=2, line=5, cex=1)

        # add title to every plot
        if (add_title && any(dot_names == "title")) {
            if (verbose) message("add title")
            title(dot_list[["title"]])
        } # add title
        
        # add text to every plot
        if (verbose) message("check dots for 'addtext_list'")
        if (any(dot_names == "addtext_list")) {
            for (j in 1:length(addtext_list)) {
                if (typeof(addtext_list[[j]]) == "character") {
                    if (verbose) print(paste0("add '", addtext_list[[j]], "' ..."))
                    eval(parse(text=addtext_list[[j]]))
                }
            }
        }

        # add name to every plot
        if (add_names_inset) {
                
            if (verbose) message("`add_names_inset` = T --> legend()")
            # prepare legend
            message("\n", "add default stuff to ", mode, " right_data legend ...")
            le <- list()
            le$pos <- "topright" 
            le$ncol <- 1
            le$text <- ynames[i]
            le$col <- cols[i]
            le$lty <- ltys[i]
            le$lwds <- lwds[i]
            le$pchs <- pchs[i]
            le$cex <- legend.cex
             
            # reorder reading direction from R's default top->bottom to left->right
            if (length(le$pos) == 1) {
                legend(le$pos, legend=le$text, lty=le$lty, lwd=le$lwd,
                       pch=le$pch, col=le$col, ncol=le$ncol,
                       x.intersp=0.2, cex=le$cex, bty="n")
            } else if (length(le$pos) == 2) {
                legend(x=le$pos[1], y=le$pos[2],
                       legend=le$text, lty=le$lty, lwd=le$lwd,
                       pch=le$pch, col=le$col, ncol=le$ncol,
                       x.intersp=0.2, cex=le$cex, bty="n")
            }

        } # if add_names_inset
       
        # draw box around plot
        box(lwd=axis_lwd)

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
                                                     draw_axis_labels_inds=draw_axis_labels_inds, 
                                                     i=i, lwd=axis_lwd),
                              x=grconvertX(subplot[[spi]]$fig_x, from="npc"),
                              y=grconvertY(subplot[[spi]]$fig_y, from="npc"),
                              type="plt")
                #par(op) # switch back to main plot

            } # for spi in subplot 

        } # if any(dot_names == "subplot")

        ## add something special to plot
        if (F) {
            message("add special stuff ... todo: give as argument list")

        } # add something special to plot

    } # for i nplots

    ## Save plot
    if (type != "active") {
        if (verbose) {
            print(paste0("image.plot.nxm(): Save ", plotname, " ..."))
        }
        dev.off()
    }

    return(list(nrow=n, ncol=m, nplots=nplots, 
                layout_widths=layout_widths, layout_heights=layout_heights, layout_mat=layout_mat))

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

