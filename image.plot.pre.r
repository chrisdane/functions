image.plot.pre <- function(zlim, 
                           nlevels=11, max_labels=15, zlevels=NULL,
                           method="pretty",
                           axis.at=NULL, axis.at.ind=NULL, axis.at.small=NULL, 
                           axis.labels=NULL, axis.round=NULL,
                           axis.zoom=F, axis.addzlims=F, power_min=NULL,
                           cols=NULL, pos_cols=NULL, neg_cols=NULL,
                           palname=NULL, colors_script,
                           anom_colorbar=NULL, 
                           center_around=0, center_col="white", center_include=F,
                           verbose=F) {

    ## Check input
    if (missing(zlim)) {
        stop("error: 'zlim' is missing")
    }
    if (missing(colors_script)) {
        colors_script <- paste0(getSrcDirectory(sys.function(sys.nframe())), "/colors/color_function.r")
        if (F) message("colors_script: ", colors_script)
    }
    if (verbose) {
        cat("zlim=")
        dput(zlim)
    }

    ## check if both positive and negative numbers
    if (is.null(anom_colorbar)) {
        if (any(zlim < center_around) && any(zlim > center_around)) {
            anom_colorbar <- T
        } else {
            anom_colorbar <- F
        }
    } else if (!is.null(anom_colorbar)) {
        if (anom_colorbar &&
            (!any(zlim < center_around) || !any(zlim > center_around))) {
            if (verbose) {
                print(paste0("note: you specified 'anom_colorbar=T' although your 'zlim' = ",
                             zlim[1], ", ", zlim[2], " is not of different signs around 'center_around' = ",
                             center_around, ". 'anom_colorbar'=F from now on ..."))
                anom_colorbar <- F
            }
        }
    }

    if (axis.zoom) {
        if (!is.null(cols) && # user colors
            axis.addzlims && # user wants to add zlims to colorbar
            length(cols) < 3) { # but provides less than 3 colors
            stop("error: provide at least 3 colors (needed for zoom)")
        }
        if (!is.null(cols)) {
            nlevels <- length(cols)
        }
        if (is.null(zlevels) && is.null(axis.labels)) {
            stop("error: provide 'zlevels' (length=2) and/or 'axis.labels' for zoom")
        }
    }

    ## zlevels including zlim
    if (is.null(zlevels)) { # not given by user
        
        if (method == "pretty") {
            zlevels <- pretty(zlim, n=nlevels)
            if (verbose) {
                cat("hereeeeee\n")
                cat("zlevels=")
                dput(zlevels)
            }

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
                if (verbose) {
                    message("case1a")
                    message("zlevels=", appendLF=F)
                    dput(zlevels)
                }
            }
            if (zlevels[length(zlevels)] < zlim[2]) {
                zlevels <- c(zlevels, zlim[2])
                if (verbose) {
                    message("case2a")
                    message("zlevels=", appendLF=F)
                    dput(zlevels)
                }
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
                cat("power_lims=")
                dput(power_lims)
                cat("power_min=")
                dput(power_min)
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
                message("signs=", appendLF=F)
                dput(signs)
                message("powers=", appendLF=F)
                dput(powers)
            }

            zlevels <- as.numeric(paste0(signs, "e", powers))
            
            # append zlims if necessary
            if (zlevels[1] > zlim[1]) {
                zlevels <- c(zlim[1], zlevels)
                if (verbose) {
                    message("case1b")
                    message("zlevels=", appendLF=F)
                    dput(zlevels)
                }
            }
            if (zlevels[length(zlevels)] < zlim[2]) {
                zlevels <- c(zlevels, zlim[2])
                if (verbose) {
                    message("case2b")
                    message("zlevels=", appendLF=F)
                    dput(zlevels)
                }
            }

        } # which method
   
    # zlevels are given by user
    } else {

        ## apply zoom (in level space)
        if (axis.zoom) {

            if (length(zlevels) != 2) {
                stop("error: if 'axis.zoom' = T, your provided 'zlevels' must be 2-length vector withe range to zoom in")
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
    if (verbose) {
        cat("here 1\n")
        message("zlevels=", appendLF=F)
        dput(zlevels)
    }

    ## Number of z levels
    nlevels <- length(zlevels)
    if (F && nlevels < max_labels) {
        if (F) {
            message("note: your 'nlevels' < 'max_labels' (",  nlevels, " < ", max_labels,
                         "). set 'max_labels' to ", nlevels)
        }
        max_labels <- nlevels
    }

    ## axis labels without zlims as numeric (converted to character/expression later)
    if (is.null(axis.labels)) {
 
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
            }

        } else { # not exp

            if (nlevels <= max_labels) {

                # nicer labels without zlims
                if (length(zlevels) > 2) {
                    axis.labels <- as.numeric(formatC(zlevels[-c(1, nlevels)]))
                } else {
                    axis.labels <- as.numeric(formatC(zlevels))
                }
                if (verbose) {
                    cat("h3r3\n")
                    cat("axis.labels=")
                    dput(axis.labels)
                }

            } else { # nlevels > max_labels

                if (verbose) {
                    message(nlevels, " nlevels > ", max_labels, " max_labels ...")
                }

                if (method == "pretty") {

                    if (axis.zoom) {
                        
                        if (zoom.l <= max_labels) {
                            if (verbose) {
                                message("zoomlevels=", appendLF=F)
                                dput(zoomlevels)
                            }
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

                    } else if (!axis.zoom) {

                        # nicer labels without zlims
                        axis.labels <- pretty(zlevels[-c(1, nlevels)], n=max_labels) 
                        if (verbose) {
                            print("fooo")
                            cat("zlim<-")
                            dput(zlim)
                            cat("zlevels<-")
                            dput(zlevels)
                            print(max_labels)
                            cat("axis.labels=")
                            dput(axis.labels)
                        }

                        if (F) { # why did i put this?!
                            if (length(zlevels) > 2) {
                                axis.labels <- as.numeric(formatC(zlevels[-c(1, nlevels)]))
                            } else {
                                axis.labels <- as.numeric(formatC(zlevels))
                            }
                        }
                        if (verbose) {
                            cat("h3r3\n")
                            cat("axis.labels=")
                            dput(axis.labels)
                        }

                        #stop("asdasdasds")

                    } # if axis.zoom
                    
                } else if (method == "exact") {

                    if (axis.zoom) {
                        axis.labels <- c(zlim[1], 
                                         seq(zoom[1], zoom[2], l=max_labels - 2),
                                         zlim[2])
                    } else {
                        axis.labels <- seq(zlim[1], zlim[2], l=max_labels)
                    }

                } # which method

            } # if nlevels <= max_labels

            ## remove axis.labels outside of zlim (due to pretty)
            if (axis.labels[1] < zlim[1]) {
                #print("hi3")
                #axis.labels[1] <- zlim[1]
                axis.labels <- axis.labels[-1]
            }
            if (axis.labels[length(axis.labels)] > zlim[2]) {
                #print("hi4")
                #axis.labels[length(axis.labels)] <- zlim[2] 
                axis.labels <- axis.labels[-length(axis.labels)]
            }

            ## need to take 1e numbers into account here!!!

        } # if method == "exp" or not
    
    } # if is.null(axis.labels)   
    if (verbose) {
        cat("here 2\n")
        cat("axis.labels=")
        dput(axis.labels)
    }

    ## round axis.labels with precision axis.round
    if (is.null(axis.round)) {

        if (!is.numeric(axis.labels)) {
            axis.round <- NULL
        
        } else {

            if (method == "exp") {
                axis.round <- NULL # do not round

            } else {

                # different orders of magnitude
                if (any(regexpr("e", axis.labels) != -1)) {
                    
                    axis.round <- NULL # do not round

                } else {

                    # there are decimals
                    if (any(regexpr("\\.", axis.labels) != -1)) {
                        
                        # find the necessary decimal place
                        pos <- regexpr("\\.", axis.labels)
                        inds <- which(pos != -1)
                        pos <- pos[inds]
                        axis.round <- max(nchar(substr(axis.labels[inds], 
                                                       pos + 1, 
                                                       nchar(axis.labels[inds]))))
                   
                    # no decimals
                    } else {
                        axis.round <- 0
                    }

                } # there are decimal in labels
            } # if method == "exp" or not
        } # if is.numeric(axis.labels)
    } # if is.null(axis.round)
    if (verbose) {
        cat("here 3\n")
        cat("axis.round=")
        dput(axis.round)
    }

    ## position of labels: axis.at
    if (is.null(axis.at)) {
        
        if (method != "exp") {
            axis.at <- axis.labels
        } else if (method == "exp") {
            axis.at <- sapply(axis.labels, function(x) eval(parse(text=x)))
        }

        # position in colorbar
        if (is.null(axis.at.ind)) {
            if (method == "exp") {
                # class(axis.labels[[1]]) = "call" !!
                # the following does not yield precise inds
                #axis.at.ind <- apply(matrix(as.numeric(sapply(axis.labels, eval)), nrow=length(axis.labels)),
                #                     1, function(x) {
                #                         which(abs(zlevels - x) == min(abs(zlevels - x)))[1] })
                axis.at.ind <- 2:(nlevels-1)
            
            } else {
                if (is.numeric(axis.labels)) {
                    axis.at.ind <- apply(matrix(axis.labels, nrow=length(axis.labels)),
                                         1, function(x) {
                                             which(abs(zlevels - x) == min(abs(zlevels - x)))[1] })
                } else {
                    stop("this is not allowed")
                }
            }
        }

    } else { # if !is.null(axis.at)
        if (!is.numeric(axis.at)) {
            stop("your axis.at must be numeric")
        }
        if (is.null(axis.at.ind)) {
            axis.at.ind <- axis.at
        }
    } # if is.null(axis.at)
    if (method == "exp") {
        ## small ticks 
        if (is.null(axis.at.small)) {
            axis.at.small <- 2:9 * rep(axis.at, e=length(2:9))
        }

    } else {

        ## no success: some nlevels and max_labels are too close 
        ## and so there are several axis.at.ind values doubled
        if ((!axis.zoom && length(unique(diff(axis.at.ind))) > 1) ||
            (axis.zoom && length(unique(diff(axis.at.ind[2:(length(axis.at.ind) - 1)]))) > 1)) {
            if (verbose) {
                cat("note: different diffs of axis.at.ind=")
                dput(axis.at.ind)
                cat(" --> diff(axis.at.ind)=")
                cat(paste0(diff(axis.at.ind), collapse=", "), "\n")
            }
        }
    } # if method == "exp"
    if (verbose) {
        cat("axis.at=")
        dput(axis.at)
        cat("axis.at.ind=")
        dput(axis.at.ind)
        cat("axis.at.small=")
        dput(axis.at.small)
    }

    # apply axis.round to axis.labels
    # --> "5.0" instead of "5" depending on 'axis.round'
    if (method == "exp") {
        axis.labels <- as.expression(axis.labels)
    
    } else {

        if (is.numeric(axis.labels)) {
        
            if (!is.null(axis.round)) {
                
                if (T) {
                    if (verbose) {
                        cat(paste0("axis.round=", axis.round, "\n"))
                    }
                    axis.labels <- sprintf(paste0("%.", axis.round, "f"), axis.labels)
                
                } else {
                    axis.labels <- format(axis.labels) # format() better than as.character()
                    if (verbose) {
                        cat("here1 axis.labels=")
                        dput(axis.labels)
                    }

                    # decimals
                    if (any(regexpr("\\.", axis.labels) != -1)) {
                        pos <- regexpr("\\.", axis.labels)
                        inds <- which(pos != -1)
                        pos <- pos[inds]
                        axis.round <- max(nchar(substr(axis.labels[inds],
                                                       pos + 1,
                                                       nchar(axis.labels[inds]))))
                    
                    # no decimals
                    } else {
                        axis.round <- 0
                    }
                }
            } # if !is.null(axis.round)
     
        } # if is.numeric(axis.labels)
        
    } # which method
    
    if (verbose) {
        cat("here 4\n")
        cat("axis.labels=")
        dput(axis.labels)
        cat("axis.round=")
        dput(axis.round)
    }

    ## add zlimits to legend
    if (axis.addzlims) {

        if (method != "exp") {

            ## it is possible that round(zlim, axis.round) equals axis.labels[1] 
            ## and/or axis.labels[n]. then the labels would be e.g. c(-50, -50, -40, ...)
            if (as.numeric(sprintf(paste0("%.", axis.round, "f"), zlim[1])) < as.numeric(axis.labels[1])) {
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
                cat("here 5\n")
                cat("axis.at.ind=")
                dput(axis.at.ind)
                cat("axis.labels=")
                dput(axis.labels)
            }
        
        } else {
            stop("not yettttttt")
        }

    } # if axis.addzlims

    if (verbose) {
        print("zlim")
        print(zlim)
        print("zlevels")
        print(zlevels)
        print(paste0("nlevels = ", nlevels))
        print("diff(zlevels)")
        print(diff(zlevels))
        print("axis.labels")
        print(axis.labels)
        print(paste0("length(axis.labels) = ", length(axis.labels)))
        print("axis.at")
        print(axis.at)
        print("axis.at.ind")
        print(axis.at.ind)
        print(paste0("length(axis.at.ind) = ", length(axis.at.ind)))
        print("diff(axis.at.ind)")
        print(diff(axis.at.ind))
        #print(paste0("axis.round = ", axis.round))
    }
            

    if (F) {
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


    ## cols (in level space)
    if (is.null(cols)) {

        ## anomaly cols (in level space)
        if (anom_colorbar) {

            ## centerind (in level space) 
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
            if (verbose) {
                message("ncolors_neg=", ncolors_neg)
                message("ncolors_pos=", ncolors_pos)
                message("ncolors_oneside=", ncolors_oneside)
            }

            if (is.null(pos_cols) || is.null(neg_cols)) {
                if (is.null(palname)) { # default colors for anomaly colorbar
                    palname <- "grads_anomaly"
                }
                if (file.exists(colors_script)) {
                    source(colors_script)
                } else {
                    stop("file colors_script=", colors_script, " is not readable.")
                }
                cols <- color_function(palname, 
                                       n=ifelse(nlevels %% 2 == 0, 
                                                nlevels, nlevels - 1),
                                       rgb_path=dirname(colors_script)) 
                             # use even colors here, include 1 additional for zero later
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
            
            if (file.exists(colors_script)) {
                source(colors_script)
            } else {
                stop("file colors_script=", colors_script, " is not readable.")
            }

            # default colors for non-anomaly-colorbar
            if (is.null(palname)) { 
                if (F) {
                    palname <- "grads_anomaly"
                    cols <- color_function(palname, rgb_path=dirname(colors_script))
                } else if (T) {
                    palname <- "mpl_gist_ncar"
                    cols <- rev(color_function(palname, rgb_path=dirname(colors_script)))
                }
            } else { # user provided palname
                cols <- color_function(palname, rgb_path=dirname(colors_script))
            }

            if (axis.zoom) {
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
                cols <- colorRampPalette(cols)(nlevels - 1)
            
            }

        } # if anom_colorbar
    
    } else {

        # check if provided cols have correct length
        if (length(cols) != nlevels - 1) {
            if (verbose) message("reorganize cols to correct length ...")
            cols <- colorRampPalette(cols)(nlevels - 1)
        }
        
    } # if is.null(cols) or not

    if (verbose) {
        print(paste0("length(cols) = ", length(cols)))
    }

    return(list(zlim=zlim, levels=zlevels, nlevels=nlevels, cols=cols, 
                axis.at=axis.at, axis.at.ind=axis.at.ind, axis.at.small=axis.at.small,
                axis.labels=axis.labels, axis.round=axis.round,
                axis.zoom=axis.zoom, axis.addzlims=axis.addzlims,
                method=method))

} # image.plot.pre function
