image.plot.pre <- function(zlim, 
                           nlevels=NULL, max_labels=NULL, zlevels=NULL,
                           method="pretty",
                           axis.at=NULL, axis.at.ind=NULL, axis.at.small=NULL, 
                           axis.labels=NULL, axis.round=NULL,
                           axis.zoom=F, axis.addzlims=T, power_min=NULL,
                           cols=NULL, pos_cols=NULL, neg_cols=NULL,
                           palname=NULL, colors_script,
                           anom_colorbar=NULL, 
                           center_around=0, center_col="white", center_include=F,
                           verbose=F) {

    if (verbose) message("********* start image.plot.pre() with verbose = `T` *********")

    ## Check input
    if (missing(zlim)) stop("error: 'zlim' is missing")
    if (any(is.na(zlim))) stop("zlim must not be NA")
    if (is.null(nlevels)) nlevels <- 11
    if (is.null(max_labels)) max_labels <- 15
    if (is.null(axis.zoom)) axis.zoom <- F
    if (is.null(axis.addzlims)) axis.addzlims <- T
    if (missing(colors_script)) {
        if (verbose) message("find colors_script ... ", appendLF=F)
        #colors_script <- paste0(getSrcDirectory(sys.function(sys.nframe())), "/colors/color_function.r")
        colors_script <- "~/scripts/r/functions/colors/color_function.r"
        if (verbose) message(colors_script)
    }
    if (verbose) cat("given zlim =", zlim, "\n")
   
    ## check if both positive and negative numbers
    if (is.null(anom_colorbar)) {
        if (any(zlim < center_around) && any(zlim > center_around)) {
            anom_colorbar <- T
        } else {
            anom_colorbar <- F
        }
    } else if (!is.null(anom_colorbar)) {
        if (verbose) message("******\ncheck given anom_colorbar = ", anom_colorbar, " ...")
        if (anom_colorbar &&
            (!any(zlim < center_around) || !any(zlim > center_around))) {
            if (verbose) {
                message("you specified 'anom_colorbar=T' although your 'zlim' = ",
                        zlim[1], ", ", zlim[2], " is not of different signs around 'center_around' = ",
                        center_around, ". set 'anom_colorbar' to false ...")
                anom_colorbar <- F
            }
        }
    }

    if (axis.zoom) {
        if (verbose) message("******\n check given axis.zoom = ", axis.zoom, " ...")
        if (!is.null(cols) && # user colors
            axis.addzlims && # user wants to add zlims to colorbar
            length(cols) < 3) { # but provides less than 3 colors
            stop("axis.addzlims=T but provided colors are of length ", length(cols), 
                 ". there must be at least 3 colors for the zoom")
        }
        if (!is.null(cols)) nlevels <- length(cols)
        if (is.null(zlevels) && is.null(axis.labels)) {
            stop("provide 'zlevels' (length=2) and/or 'axis.labels' for zoom")
        }
    }

    ## zlevels including zlim
    if (is.null(zlevels)) { # not given by user
        if (verbose) message("******\nfind zlevels with method \"", method, "\" ...")
        
        if (method == "pretty") {
            zlevels <- pretty(zlim, n=nlevels)
            if (verbose) cat("zlevels =", zlevels, "\n")

            ## remove zlevels outside of zlim (due to pretty)
            if (zlevels[1] < zlim[1]) {
                zlevels <- zlevels[-1]
            }
            if (zlevels[length(zlevels)] > zlim[2]) {
                zlevels <- zlevels[-length(zlevels)]
            }

            ## append zlims if necessary
            if (zlevels[1] > zlim[1]) {
                zlevels <- c(zlim[1], zlevels)
                if (verbose) cat("zlevels case1a =", zlevels, "\n")
            }

            if (zlevels[length(zlevels)] < zlim[2]) {
                zlevels <- c(zlevels, zlim[2])
                if (verbose) cat("zlevels case2a =", zlevels, "\n")
            }

        } else if (method == "exact") {
            zlevels <- seq(zlim[1], zlim[2], l=nlevels)
        
        } else if (method == "exp") {

            power_lims <- c(0, 0) # -20.34705  29.78989 --> 1 (i.e. 10^1)
            # as.integer(log10(abs(zlim)))
            if (zlim[1] != 0) { 
                power_lims[1] <- as.integer(log10(abs(zlim[1])))
            }
            if (zlim[2] != 0) {
                power_lims[2] <- as.integer(log10(abs(zlim[2])))
            } 

            # provide power_min
            if (is.null(power_min)) { # default
                if (all(power_lims == 0)) {
                    power_min <- -2 # power 0 --> powers -2, -1, 0
                } else {
                    if (min(power_lims) < 0) {
                        power_min <- min(power_lims) - 1 # include 0.1 if zlim = 1.x
                    } else {
                        power_min <- min(power_lims)
                    }
                }
            }
            if (verbose) { 
                cat("power_lims =", power_lims, "\n")
                cat("power_min =", power_min, "\n")
            }

            if (anom_colorbar) {

                #  zlevels = c(zlim[1], -1e1, -1e0, -1e-1, -1e-2, 0, 1e-2, 1e-1, 1e0, 1e1, zlim[2])
                if (min(power_lims) == 0) {
                    powers <- c(0:power_min, 0, power_min:0)
                    signs <- c(rep(-1, t=floor(length(powers)/2)), 0, rep(1, t=floor(length(powers)/2))) 

                } else if (min(power_lims) > 0) { # e.g. zlim=c(-20.34705, 29.78989)
                    powers <- c(power_lims[1]:power_min, 0, power_min:power_lims[2])
                    signs <- c(rep(-1, t=length(power_lims[1]:power_min)), 0, rep(1, t=length(power_min:power_lims[2])))
                
                } else if (min(power_lims) < 0) { # e.g. zlim=c(-0.0002034705, 0.0002978989)
                    powers <- c((power_lims[1] - 1):power_min, 0, power_min:(power_lims[2] - 1))
                    signs <- c(rep(-1, t=length((power_lims[1] - 1):power_min)), 0, rep(1, t=length(power_min:(power_lims[2] - 1))))    
                }
                
            } else if (!anom_colorbar) {

                if (all(zlim >= 0)) { # both zlim positive
                    powers <- power_min:(power_lims[2] + 1)
                    signs <- rep(1, t=length(powers)) 
                
                } else if (all(zlim < 0)) { # both zlim negative
                    stop("not yettttt")
                }
            
            }

            if (verbose) {
                cat("signs =", signs, "\n")
                cat("powers =", powers, "\n")
            }

            zlevels <- as.numeric(paste0(signs, "e", powers))
            
            # append zlims if necessary
            if (zlevels[1] > zlim[1]) {
                zlevels <- c(zlim[1], zlevels)
                if (verbose) cat("zlevels case1b =", zlevels, "\n")
            }
            if (zlevels[length(zlevels)] < zlim[2]) {
                zlevels <- c(zlevels, zlim[2])
                if (verbose) cat("zlevels case2b =", zlevels, "\n")
            }

        } # which method
   
    # zlevels are given by user
    } else if (!is.null(zlevels)) {
        
        if (verbose) cat("******\ncheck provided zlevels (n=", 
                         length(zlevels), ") = ", zlevels, "\n")

        ## apply zoom (in level space)
        if (axis.zoom) {

            if (verbose) message("with axis.zoom")
            if (length(zlevels) != 2) {
                stop("error: if 'axis.zoom' = T, your provided 'zlevels' ",
                     "must be 2-length vector withe range to zoom in")
            }

            zoom <- zlevels
            zoomlevels <- pretty(zoom, n=nlevels - 2)
            zoom.l <- length(zoomlevels)
            #zoom.b <- diff(zoom)/zoom.l

            # Fill space between zlim and zoom at beginning and end of colorbar
            nlevplab <- max(zoom.l/max_labels, 1)
            if (zoom[1] > zlim[1] && zoom[2] < zlim[2]) {
                zlevels <- c(seq(zlim[1], zoom[1], l=nlevplab),
                             zoomlevels[2:(zoom.l - 1)],
                             seq(zoom[2], zlim[2], l=nlevplab))

            } else if (zoom[1] > zlim[1] && !(zoom[2] < zlim[2])) {
                zlevels <- c(seq(zlim[1], zoom[1], l=nlevplab),
                             zoomlevels[2:zoom.l])

            } else if (!(zoom[1] > zlim[1]) && zoom[2] < zlim[2]) {
                zlevels <- c(zoomlevels[1:(zoom.l - 1)],
                             seq(zoom[2], zlim[2], l=nlevplab))

            } else if (!(zoom[1] > zlim[1]) && !(zoom[2] < zlim[2])) {
                zlevels <- zoomlevels

            }
            if (zlevels[length(zlevels)] != zlim[2]) { # may happen because b= is used and not l=
                zlevels[length(zlevels)] <- zlim[2]
            }

        } # if axis.zoom
    
    } # is zlevels are given by user or not 
    if (verbose) cat("final zlevels (n=", length(zlevels), ") = ", 
                     paste(zlevels, collapse=", "), "\n", sep="")

    ## Number of z levels
    nlevels <- length(zlevels)
    if (F && nlevels < max_labels) {
        if (F) {
            message("'nlevels' < 'max_labels' (",  nlevels, " < ", max_labels,
                    "). set 'max_labels' to ", nlevels)
        }
        max_labels <- nlevels
    }

    ## axis labels without zlims as numeric (converted to character/expression later)
    if (is.null(axis.labels)) {

        if (verbose) message("******\nfind axis.labels with method \"", method, "\" ...")
 
        if (method == "exp") {

            # !!! paste0() not allowed here !!!
            # zlims are added later to axis.labels
            axis.labels <- vector("list", l=length(powers))

            for (i in 1:length(axis.labels)) {
                if (signs[i] == 0 && powers[i] == 0) {
                    axis.labels[[i]] <- substitute(paste(0))

                } else if (signs[i] != 0 && powers[i] == 0) {
                    if (signs[i] < 0) {
                        axis.labels[i] <- -1 #substitute(-1)
                    } else if (signs[i] > 0) {
                        axis.labels[i] <- substitute(1)
                    }
                } else if (signs[i] != 0 && powers[i] == 1) {
                    if (signs[i] < 0) {
                        axis.labels[i] <- substitute(-10)
                    } else if (signs[i] > 0) {
                        axis.labels[i] <- substitute(10)
                    }
                
                } else {
                    if (signs[i] < 0) {
                        axis.labels[[i]] <- substitute(paste(sign^power),
                                                       list(sign=signs[i]*10, power=powers[i]))
                    } else if (signs[i] > 0) {
                        axis.labels[[i]] <- substitute(paste(sign^power),
                                                       list(sign=signs[i]*10, power=powers[i]))
                    }
                    #axis.labels[[i]] <- substitute(paste(#pm, 
                    #                                     sign^power),
                    #                               list(#pm=ifelse(sign > 0, "", "-"),
                    #                                    sign=signs[i]*10, power=powers[i]))
                }
            } # for i labels
            if (verbose) cat("case exp; axis.labels (n=", length(axis.labels), " =", 
                             axis.labels, "\n")

        } else { # if method != "exp"

            if (nlevels <= max_labels) {

                # nicer labels without zlims
                if (length(zlevels) > 2) {
                    #axis.labels <- as.numeric(formatC(zlevels[-c(1, nlevels)]))
                    axis.labels <- as.numeric(format(zlevels[-c(1, nlevels)]))
                } else {
                    axis.labels <- as.numeric(formatC(zlevels))
                }
                if (verbose) cat("case non-exp & nlev <= max_lab; axis.labels (n=", 
                                 length(axis.labels), ") =", axis.labels, "\n")

            } else { # nlevels > max_labels
                
                if (method == "pretty") {

                    if (axis.zoom) {
                       
                        if (verbose) message("with axis.zoom ...")

                        if (zoom.l <= max_labels) {
                            if (verbose) cat("case pretty & nlev > max_lab & with zoom; zoomlevels =", zoomlevels, "\n")
                            axis.labels <- zoomlevels
                        
                        } else {
                            if (axis.addzlims) {
                                axis.labels <- pretty(zoom, n=max_labels - 2)
                            } else {
                                axis.labels <- pretty(zoom, n=max_labels)
                            }
                            
                            ## remove axis.labels outside of zlim (due to pretty)
                            if (axis.labels[1] < zoom[1]) {
                                #axis.labels[1] <- zlim[1]
                                axis.labels <- axis.labels[-1]
                            }
                            if (axis.labels[length(axis.labels)] > zoom[2]) {
                                #axis.labels[length(axis.labels)] <- zlim[2] 
                                axis.labels <- axis.labels[-length(axis.labels)]
                            }
                        }
                        if (verbose) cat("case pretty & nlev > max_lab & with zoom; axis.labels (n=", 
                                         length(axis.labels), ") =", axis.labels, "\n")

                    } else if (!axis.zoom) {

                        # nicer labels without zlims
                        axis.labels <- pretty(zlevels[-c(1, nlevels)], n=max_labels) 

                        if (F) { # why did i put this?!
                            if (length(zlevels) > 2) {
                                axis.labels <- as.numeric(formatC(zlevels[-c(1, nlevels)]))
                            } else {
                                axis.labels <- as.numeric(formatC(zlevels))
                            }
                        }
                        if (verbose) cat("case pretty & nlev > max_lab & without zoom; axis.labels (n=", 
                                         length(axis.labels), ") =", axis.labels, "\n")

                    } # if axis.zoom
                    
                } else if (method == "exact") {

                    if (axis.zoom) {
                        axis.labels <- c(zlim[1], 
                                         seq(zoom[1], zoom[2], l=max_labels - 2),
                                         zlim[2])
                        if (verbose) cat("case exact & with zoom; axis.labels (n=", 
                                         length(axis.labels), ") =", axis.labels, "\n")
                    } else if (!axis.zoom) {
                        axis.labels <- seq(zlim[1], zlim[2], l=max_labels)
                        if (verbose) cat("case exact & without zoom; axis.labels (n=", 
                                         length(axis.labels), ") =", axis.labels, "\n")
                    }

                } # which method

            } # if nlevels <= max_labels

            ## remove axis.labels outside of zlim (due to pretty)
            if (axis.labels[1] < zlim[1]) {
                if (verbose) message("remove axis.labels[1] = ", axis.labels[1], 
                                     " < zlim[1] = ", zlim[1], " ...")
                axis.labels <- axis.labels[-1]
            }
            if (axis.labels[length(axis.labels)] > zlim[2]) {
                if (verbose) message("remove axis.labels[", length(axis.labels), "] = ", 
                                     axis.labels[length(axis.labels)], " > zlim[2] = ", 
                                     zlim[2], " ...")
                axis.labels <- axis.labels[-length(axis.labels)]
            }

            # all values were removed
            if (length(axis.labels) == 0) {
                axis.labels <- mean(zlim, na.rm=T)
                if (verbose) message("all automatic axis.labels are out of zlim. ",
                                     "set axis.labels as mean(zlim) = ", axis.labels)
            }

            ## need to take 1e numbers into account here!!!

        } # if method == "exp" or not
    
    } else if (!is.null(axis.labels)) {
        if (verbose) cat("******\ncheck provided axis.labels (n=", 
                         length(axis.labels), " =", axis.labels, "\n")
        # no checks here?

    } # if is.null(axis.labels) or not 
    if (verbose) cat("final axis.labels before format (n=", 
                     length(axis.labels), ") =", axis.labels, "\n")

    ## round axis.labels with precision axis.round
    if (is.null(axis.round)) {
        if (verbose) message("******\nfind axis.round ...")

        if (!is.numeric(axis.labels)) { # if axis.labels was provided by user as character
            axis.round <- NULL
        
        } else {

            if (method == "exp") {
                axis.round <- NULL # do not round

            } else {

                if (any(regexpr("e", axis.labels) != -1)) { # different orders of magnitude
                    axis.round <- NULL # do not round

                } else {
                    if (any(regexpr("\\.", axis.labels) != -1)) { # there are decimals
                        
                        # find the necessary decimal place
                        pos <- regexpr("\\.", axis.labels)
                        inds <- which(pos != -1)
                        pos <- pos[inds]
                        axis.round <- max(nchar(substr(axis.labels[inds], 
                                                       pos + 1, 
                                                       nchar(axis.labels[inds]))))
                    } else { # no decimals
                        axis.round <- 0
                    }

                } # there are decimal in labels
            } # if method == "exp" or not
        } # if is.numeric(axis.labels)
    
    } else if (!is.null(axis.round)) {
        if (verbose) cat("******\ncheck provided axis.round =", axis.round, "\n")
        # no checks here?
    
    } # if is.null(axis.round) or not
    if (verbose) cat("final axis.round =", axis.round, "\n")

    ## position of labels: axis.at
    if (is.null(axis.at)) {
    
        if (verbose) message("******\nfind axis.at with method \"", method, "\" ...")

        if (method != "exp") {
            axis.at <- axis.labels
            if (verbose) cat("case non-exp; axis.at (n=", length(axis.at), ") =", axis.at, "\n")
        } else if (method == "exp") {
            axis.at <- sapply(axis.labels, function(x) eval(parse(text=x)))
            if (verbose) cat("case exp; axis.at (n=", length(axis.at), ") =", axis.at, "\n")
        }

        # position in colorbar
        if (is.null(axis.at.ind)) {
            
            if (verbose) message("******\nfind axis.at.ind with method \"", method, "\" ...")

            if (method == "exp") {
                # class(axis.labels[[1]]) = "call" !!
                # the following does not yield precise inds
                #axis.at.ind <- apply(matrix(as.numeric(sapply(axis.labels, eval)), nrow=length(axis.labels)),
                #                     1, function(x) {
                #                         which(abs(zlevels - x) == min(abs(zlevels - x)))[1] })
                axis.at.ind <- 2:(nlevels-1)
                if (verbose) cat("case exp; axis.at.ind (n=", length(axis.at.ind), 
                                 ") =", axis.at.ind, "\n")
            
            } else {
                axis.at.ind <- apply(matrix(as.numeric(axis.labels), nrow=length(axis.labels)),
                                     1, function(x) {
                                         which(abs(zlevels - x) == min(abs(zlevels - x)))[1] })
                if (verbose) cat("case non-exp; axis.at.ind (n=", 
                                 length(axis.at.ind), ") =", axis.at.ind, "\n")
            }
        } else {
            stop("not happened yet")
        }

    } else if (!is.null(axis.at)) {
        if (verbose) cat("******\ncheck provided axis.at (n=", 
                         length(axis.at), " =", axis.at, "\n")
        if (!is.numeric(axis.at)) stop("your axis.at must be numeric")
        if (is.null(axis.at.ind)) {
            axis.at.ind <- axis.at
            if (verbose) cat("case non-provided axis.at; axis.at.ind (n=", 
                             length(axis.at.ind), ") =", axis.at.ind, "\n")
        } 
    
    } # if is.null(axis.at)
    
    # further axis.at.ind checks 
    if (method == "exp") { # small ticks between 1, 10, 100, ...
        if (is.null(axis.at.small)) {
            axis.at.small <- 2:9 * rep(axis.at, e=length(2:9))
        }
    } else { # if method != "exp"
        # no success: some nlevels and max_labels are too close 
        # and so there are several axis.at.ind values doubled
        if ((!axis.zoom && length(unique(diff(axis.at.ind))) > 1) ||
            (axis.zoom && length(unique(diff(axis.at.ind[2:(length(axis.at.ind) - 1)]))) > 1)) {
            if (verbose) cat("case non-exp & non-constant diff(axis.at.ind) =", diff(axis.at.ind), "\n")
        }
    } # if method == "exp"
    if (verbose) {
        message("******\naxis.at after first check:")
        message("nlevels = ", nlevels)
        cat("axis.at (n=", length(axis.at), ") =", axis.at, "\n")
        cat("axis.at.ind (n=", length(axis.at.ind), ") =", axis.at.ind, "\n")
        cat("axis.at.small (n=", length(axis.at.small), ") =", axis.at.small, "\n")
        cat("axis.round =", axis.round, "\n")
    }

    # apply axis.round to axis.labels
    # --> "5.0" instead of "5" depending on 'axis.round'
    if (verbose) message("******\napply axis.round to axis.labels ...")
    if (method == "exp") {
        axis.labels <- as.expression(axis.labels)
        if (verbose) cat("case exp; axis.labels =", axis.labels, "\n")

    } else {

        if (is.numeric(axis.labels)) {
            axis.labels <- format(axis.labels, trim=T)
            if (verbose) cat("case nonexp & numeric; axis.labels =", axis.labels, "\n")
        } # if is.numeric(axis.labels)
        
    } # which method
    if (verbose) cat("final axis.labels after format =", axis.labels, "\n")

    # add zlimits to legend
    if (axis.addzlims) {
        if (verbose) message("******\naxis.addzlims=T --> add zlim to labels ...")

        if (method != "exp") {

            ## it is possible that round(zlim, axis.round) equals axis.labels[1] 
            ## and/or axis.labels[n]. then the labels would be e.g. c(-50, -50, -40, ...)
            if (as.numeric(sprintf(paste0("%.", axis.round, "f"), zlim[1])) < as.numeric(axis.labels[1])) {
                message("zlim[1] = ", zlim[1], " < axis.labels[1] = ", axis.labels[1])
                axis.labels <- c(sprintf(paste0("%.", axis.round, "f"), zlim[1]), axis.labels)
                axis.at <- c(as.numeric(sprintf(paste0("%.", axis.round, "f"), zlim[1])), axis.at)
                axis.at.ind <- c(1, axis.at.ind)
            }
            if (as.numeric(sprintf(paste0("%.", axis.round, "f"), zlim[2])) > as.numeric(axis.labels[length(axis.labels)])) {
                axis.labels <- c(axis.labels, sprintf(paste0("%.", axis.round, "f"), zlim[2]))
                axis.at <- c(axis.at, as.numeric(sprintf(paste0("%.", axis.round, "f"), zlim[2])))
                axis.at.ind <- c(axis.at.ind, nlevels)
            }
            if (verbose) {
                cat("case non-exp; axis.labels (n=", length(axis.labels), ") =", axis.labels, "\n")
                cat("case non-exp; axis.at (n=", length(axis.at), ") =", axis.at, "\n")
                cat("case non-exp; axis.at.ind (n=", length(axis.at.ind), ") =", axis.at.ind, "\n")
            }
        
        } else {
            stop("not yettttttt")
        }

    } # if axis.addzlims

    if (verbose) {
        message("******\nzlim,levels,axis checks finished:")
        cat("zlim =", zlim, "\n")
        message("nlevels = ", nlevels)
        cat("zlevels (n=", length(zlevels), ") =", zlevels, "\n")
        cat("diff(zlevels) =", diff(zlevels), "\n")
        cat("length(unique(diff(zlevels))) =", length(unique(diff(zlevels))), "\n")
        cat("axis.labels (n=", length(axis.labels), ") =", axis.labels, "\n")
        cat("axis.at (n=", length(axis.at), ") =", axis.at, "\n")
        cat("diff(axis.at) =", diff(axis.at), "\n")
        cat("axis.at.ind (n=", length(axis.at.ind), ") =", axis.at.ind, "\n")
        cat("diff(axis.at.ind) =", diff(axis.at.ind), "\n")
    }

    if (F) { # add special stuff
        if (axis.addzlims && axis.zoom) {
            axis.at.ind <- c(axis.at.ind[1], 
                         mean(axis.at.ind[1:2]), 
                         axis.at.ind[2:(length(axis.at.ind) - 1)],
                         mean(axis.at.ind[(length(axis.at.ind) - 1):length(axis.at.ind)]),
                         axis.at.ind[length(axis.at.ind)])
            axis.labels <- c(axis.labels[1], 
                             "⁄⁄",
                             axis.labels[2:(length(axis.labels) - 1)],
                             "⁄⁄",
                             axis.labels[length(axis.labels)])
        }
    }


    # cols (in level space)
    if (is.null(cols)) {
        if (verbose) message("******\nfind cols ...")

        # anomaly cols (in level space)
        if (anom_colorbar) {
            if (verbose) message("case anom_colorbar ...")
            
            # centerind (in level space) 
            centerind <- which(abs(zlevels - center_around) == min(abs(zlevels - center_around)))
            if (length(centerind) > 1) {
                if (verbose) {
                    print(paste0("warning: found ", length(centerind), " positions to center around ",
                                 center_around, ". take the first one: zlevels[", centerind[1], "] = ",
                                 zlevels[centerind[1]], " ..."))
                }
                centerind <- centerind[1]
            }

            ncolors_neg <- centerind - 1
            ncolors_pos <- nlevels - centerind
            # both zoom and equal spacing in both neg and pos colors
            ncolors_oneside <- max(ncolors_neg, ncolors_pos) 
            if (verbose) message("ncolors_neg = ", ncolors_neg, ", ncolors_pos = ", 
                                 ncolors_pos, ", ncolors_oneside = ", ncolors_oneside)

            if (is.null(pos_cols) || is.null(neg_cols)) {
                if (is.null(palname)) { # default colors for anomaly colorbar
                    palname <- "grads_anomaly"
                    if (verbose) message("palname not provided; use default ", palname, " ...")
                }
                if (file.exists(colors_script)) {
                    source(colors_script)
                } else {
                    stop("file colors_script=", colors_script, " does not exist.")
                }
                # use even colors here, include 1 additional for zero later
                if (verbose) message("run color_function() with palname = ", palname)
                cols <- color_function(palname, 
                                       n=ifelse(nlevels %% 2 == 0, 
                                                nlevels, nlevels - 1),
                                       rgb_path=dirname(colors_script)) 
                #cat("pal=")
                #dput(pal)
                neg_cols <- cols[1:(length(cols)/2)]
                pos_cols <- cols[(length(cols)/2 + 1):length(cols)]
            } # is.null(pos_cols) || is.null(neg_cols)

            if (center_include) {
                neg_cols <- c(neg_cols[2:length(neg_cols)], center_col)
                pos_cols <- c(center_col, pos_cols[1:(length(pos_cols) - 1)])
            }
           
            # make anomaly color vector
            neg_cols_rgb <- colorRampPalette(neg_cols)(ncolors_oneside)
            pos_cols_rgb <- colorRampPalette(pos_cols)(ncolors_oneside)
            cols <- c(neg_cols_rgb[(length(pos_cols_rgb) - ncolors_neg + 1):length(pos_cols_rgb)],
                      pos_cols_rgb[1:ncolors_pos])

        } else if (!anom_colorbar) {
            if (verbose) message("case not anom_colorbar ...")
            
            if (file.exists(colors_script)) {
                source(colors_script)
            } else {
                stop("file colors_script=", colors_script, " does not exist.")
            }

            # default colors for non-anomaly-colorbar
            if (is.null(palname)) { 
                #palname <- "grads_anomaly"
                #palname <- "colormaps_3gauss" # ncview's default
                palname <- "Spectral" # from RColorBrewer
                if (verbose) message("palname not provided; use default ", palname, " ...")
            }
            if (verbose) message("run color_function() with palname = ", palname)
            cols <- color_function(palname, rgb_path=dirname(colors_script))

            if (axis.zoom) {
                message("case axis.zoom ...") 
                if (zoom[1] > zlim[1] && zoom[2] < zlim[2]) {
                    cols <- c(colorRampPalette(cols[1])(nlevplab - 1),
                              colorRampPalette(cols[2:(length(cols) - 1)])(zoom.l - 1),
                              colorRampPalette(cols[length(cols)])(nlevplab - 1))

                } else if (zoom[1] > zlim[1] && !(zoom[2] < zlim[2])) {
                    cols <- c(colorRampPalette(cols[1])(nlevplab - 1),
                              colorRampPalette(cols[2:length(cols)])(zoom.l - 1))

                } else if (!(zoom[1] > zlim[1]) && zoom[2] < zlim[2]) {
                    cols <- c(colorRampPalette(cols[1:(length(cols) - 1)])(zoom.l - 1),
                              colorRampPalette(cols[length(cols)])(nlevplab - 1))

                } else if (!(zoom[1] > zlim[1]) && !(zoom[2] < zlim[2])) {
                    cols <- c(colorRampPalette(cols)(zoom.l - 1))
                }
            
            } else {
                if (verbose) message("case not axis.zoom ...") 
                cols <- colorRampPalette(cols)(nlevels - 1)
            
            } # if axis.zoom or not

        } # if anom_colorbar
    
    } else if (!is.null(cols)) {
        if (verbose) message("******\ncheck provided cols (n=", length(cols), ") ...")

        # check if provided cols have correct length
        if (length(cols) != nlevels - 1) {
            if (verbose) message("reorganize cols (n=", length(cols), 
                                 " to correct length (nlevels-1=", nlevels-1,
                                 ") using grDevices::colorRampPalette() ...")
            cols <- grDevices::colorRampPalette(cols)(nlevels - 1)
        }
        
    } # if is.null(cols) or not
    if (verbose) message("final length(cols) = ", length(cols))
    
    if (verbose) message("********* finished image.plot.pre() with verbose = `T` *********")

    return(list(zlim=zlim, levels=zlevels, nlevels=nlevels, cols=cols, 
                axis.at=axis.at, axis.at.ind=axis.at.ind, axis.at.small=axis.at.small,
                axis.labels=axis.labels, axis.round=axis.round,
                axis.zoom=axis.zoom, axis.addzlims=axis.addzlims,
                method=method))

} # image.plot.pre function
