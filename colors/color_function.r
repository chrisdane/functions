## R

color_function <- function(palname="demo", n=64, alpha=1, 
                           rgb_path, rgb_mat=NULL) {
    
    ## rgb: 3 column matrix (R, G, B)
    if (F) {
        message("getwd(): ", getwd())
        cat("current frame is", sys.nframe(), "\n")
        cat("parents are", sys.parents(), "\n")
        cat("sys.function(", sys.nframe(), ") =\n")
        print(getSrcDirectory(sys.function(sys.nframe())))
        #cat("sys.frame(", sys.nframe(), ")$ofile = ", sys.frame(i)$ofile)
        cat("names(sys.frame(", sys.nframe(), ") = ", names(sys.frame(sys.nframe())), "\n")
    }

    if (missing(rgb_path)) {
        rgb_path <- getSrcDirectory(sys.function(sys.nframe()))
    }
    rgb_path <- paste0(rgb_path, "/")
    if (F) message("rgb_path: ", rgb_path)

    ## R built-in colormaps
    rs <- c("heat", "rainbow", "topo", "cm", "terrain")

    ## matlabs
    matlabs <- c("jet")#, "parula")

    ## python
    pythons <- c("viridis", "magma", "plasma", "inferno") # needs package viridis

    ## ncview 
    ## https://www.myroms.org/forum/viewtopic.php?t=1930
    ncviews <- c("ncview_blue_red", "ncview_jaisnb", "ncview_jaison", 
                 "ncview_rainbow",  "ncview_banded",  "ncview_bright",   
                 "ncview_jaisnc", "ncview_jet", "ncview_roullet", 
                 "ncview_blu_red", "ncview_jaisn2", "ncview_jaisnd", 
                 "ncview_manga", "ncview_wheel")

    ## grads 
    grads <- "grads_anomaly"

    ## colorbrewer
    colorbrewers_seq <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", 
                          "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", 
                          "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")
    colorbrewers_div <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", 
                          "RdYlBu", "RdYlGn", "Spectral")
    colorbrewers_qual <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", 
                           "Set1", "Set2", "Set3")
    colorbrewers <- c(colorbrewers_seq, colorbrewers_div, colorbrewers_qual)

    ## add further names here for demo:
    all <- c(rs, matlabs, pythons,
             "mpl_gist_ncar", 
             ncviews,
             grads)

    ##########################################################

    if (palname == "demo") {
        names <- all
    } else {
        names <- palname
    }
    nnames <- length(names)

    ## Prepare 3-column rgb matrix
    if (missing(rgb_mat)) {

        rgb_list <- vector("list", l=nnames) # every item in rgb_list needs to be n x 3

        for (i in 1:nnames) {

            if (names[i] %in% rs) {

                if (names[i] == "heat") {
                    # red -> white
                    rgb <- t(col2rgb(heat.colors(n)))
                    if (T) { # white -> red
                        rgb <- rgb[n:1,]
                    }
                
                } else if (names[i] == "rainbow") {
                    rgb <- t(col2rgb(rainbow(n)))

                } else if (names[i] == "topo") {
                    rgb <- t(col2rgb(topo.colors(n)))

                } else if (names[i] == "cm") {
                    rgb <- t(col2rgb(cm.colors(n)))
                
                } else if (names[i] == "terrain") {
                    rgb <- t(col2rgb(terrain.colors(n)))
                }

            } else if (names[i] %in% matlabs) {

                if (names[i] == "jet") {
                    ## adapted from fields::jet.colors()
                    orig <- c("#00008F", "#00009F", "#0000AF", "#0000BF", "#0000CF",
                              "#0000DF", "#0000EF", "#0000FF", "#0010FF", "#0020FF",
                              "#0030FF", "#0040FF", "#0050FF", "#0060FF", "#0070FF",
                              "#0080FF", "#008FFF", "#009FFF", "#00AFFF", "#00BFFF",
                              "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF", "#10FFEF",
                              "#20FFDF", "#30FFCF", "#40FFBF", "#50FFAF", "#60FF9F",
                              "#70FF8F", "#80FF80", "#8FFF70", "#9FFF60", "#AFFF50",
                              "#BFFF40", "#CFFF30", "#DFFF20", "#EFFF10", "#FFFF00",
                              "#FFEF00", "#FFDF00", "#FFCF00", "#FFBF00", "#FFAF00",
                              "#FF9F00", "#FF8F00", "#FF8000", "#FF7000", "#FF6000",
                              "#FF5000", "#FF4000", "#FF3000", "#FF2000", "#FF1000",
                              "#FF0000", "#EF0000", "#DF0000", "#CF0000", "#BF0000",
                              "#AF0000", "#9F0000", "#8F0000", "#800000")
                    #if (n == 64 & alpha == 1) 
                    #    return(orig)
                    rgb <- t(col2rgb(orig))

                } else if (names[i] == "parula") {
                    print("not implemented")
                } 

            } else if (names[i] %in% pythons) {
                
                library(viridis)

                if (names[i] == "viridis") {
                    rgb <- t(col2rgb(viridis(n=n, alpha=alpha)))

                } else if (names[i] == "magma") {
                    rgb <- t(col2rgb(magma(n=n, alpha=alpha))) 

                } else if (names[i] == "inferno") {
                    rgb <- t(col2rgb(inferno(n=n, alpha=alpha)))

                } else if (names[i] == "plasma") {
                    rgb <- t(col2rgb(plasma(n=n, alpha=alpha)))
                }

            } else if (names[i] %in% ncviews) {
                
                rgb <- readLines(paste0(rgb_path, names[i], ".h"))
                # remove possible header (not always there)
                if (gregexpr("/", rgb[1])[[1]][1] == 1) {
                    j <- 2
                    while (gregexpr("/", rgb[j])[[1]][1] == -1) {
                        j <- j + 1
                    }
                    rgb <- rgb[-(1:j)]
                }
                # remove empty lines
                if (any(rgb == "")) {
                    rgb <- rgb[-which(rgb == "")]
                }
                # find beginning of rgb values in 1st line
                start <- regexpr("\\{", rgb[1])
                inds <- gregexpr("[0-9]", rgb[1])[[1]]
                start <- inds[inds > start][1] - 1
                # reorder 1st line
                rgb[1] <- substr(rgb[1], start=start, stop=nchar(rgb[1]))
                # remove symbols at the end
                rgb[length(rgb)] <- gsub("};", ",", rgb[length(rgb)])
                # character -> numeric
                rgb <- sapply(strsplit(rgb, ","), as.numeric)
                # reorder to 3 column matrix
                rgb <- t(matrix(unlist(rgb), nrow=3))

            } else if (names[i] == "mpl_gist_ncar") {

                rgb <- as.matrix(read.table(paste0(rgb_path, "MPL_gist_ncar.rgb"), skip=2))

            } else if (names[i] == "grads_anomaly") {
                neg_col <- c("#0000a3", "#0000cc", "#0000fe", "#0041fe", "#0083fe", "#00bcfe")
                pos_col <- c("#fefe00", "#febc00", "#fe8300", "#fe0000", "#bc0000", "#830000")
                rgb <- t(col2rgb(c(neg_col, "white", pos_col)))

            } else if (names[i] %in% colorbrewers) {

                library(RColorBrewer)
                rgb <- t(col2rgb(brewer.pal(n=min(n, 11), name=names[i]))) # 11 is maximum n of the package

            }

            if (class(rgb) == "function") { # built-in function was not overwritten
                stop("palette name '", names[i], "' not known. choose one of\n", 
                     paste0(all, collapse=","))
            }
            rgb_list[[i]] <- rgb

        } # for i names

    } else { # rgb_mat is given

        ## check rgb_mat input
        if (length(dim(rgb_mat)) != 2 || dim(rgb_mat)[2] != 3) {
            stop("error: 'rgb_mat' must be a 3-column matrix (R, G, B)")
        }

        rgb_list <- list(rgb_mat)

    } # rgb_mat is given or not
    names(rgb_list) <- names

    ##########################################################

    ## Make colorbar
    ncol <- length(rgb_list)
    cols_list <- vector("list", l=ncol)
    names(cols_list) <- names

    for (i in 1:ncol) {

        rgb <- rgb_list[[i]]

        if (range(rgb)[2] <= 1) {
            maxColorValue <- 1
        } else {
            maxColorValue <- 255
        }
        if (F) {
            print(names[i])
            print(maxColorValue)
            print(str(rgb))
        }

        ## adapted from fields::jet.colors()
        temp <- matrix(NA, ncol = 3, nrow = n)
        x <- seq(0, maxColorValue, l=dim(rgb)[1])
        xg <- seq(0, maxColorValue, l=n)
        for (k in 1:3) {
            hold <- fields::splint(x=x, y=rgb[, k], xgrid=xg)
            hold[hold < 0] <- 0
            hold[hold > maxColorValue] <- maxColorValue
            if (maxColorValue == 255) {
                temp[, k] <- round(hold)
            } else if (maxColorValue == 1) {
                temp[, k] <- hold
            }
        }
        #print(str(temp))

        if (alpha == 1) {    
            cols <- rgb(temp, maxColorValue=maxColorValue)
        } else {
            cols <- rgb(temp, maxColorValue=maxColorValue, alpha=alpha)
        }

        cols_list[[i]] <- cols

    } # for i nnames

    if (palname == "demo") {

        ## plot demo
        #graphics.off()
        dev.new()
        par(mar=c(5, 4, 4, 8)) # space for names
        x <- 1:n
        y <- 1:ncol
        z <- array(1:(n*ncol), c(n, ncol))
        xp <- seq(0.5, dim(z)[1] + 0.5, l=100)
        yp <- seq(0.5, dim(z)[2] + 0.5, l=100)
        plot(xp, yp, t="n", axes=F,
             xlab="Number of levels", ylab=NA, 
             xaxs="i", yaxs="i")
        axis(1, at=pretty(x, n=10), labels=pretty(x, n=10))
        axis(2, at=y, labels=y, las=2)
        title("this is the 'color_function()' demo")
        image(x, y, z, col=unlist(cols_list), breaks=0:max(z),
              add=T, axes=F, useRaster=T,
              xlab=NA, ylab=NA)
        abline(h=seq(1.5, ncol - 0.5, b=1))
        box()

        # add names
        lex <- rep(n, t=nnames)
        text(lex, y, names, pos=4, xpd=T)

    }

    if (length(cols_list) == 1) {
        return(cols_list[[1]])
    } else {
        return(cols_list)
    }

} # end function
