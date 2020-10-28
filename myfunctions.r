## R

## my collection of small R functions

# return R currently running executable
Rexe <- function() {
    return(paste0(R.home(), "/bin/exec/R"))
}

tryCatch.W.E <- function(expr) { # from `demo(error.catching)`
    W <- NULL
    w.handler <- function(w) { # warning handler
        W <<- w
        invokeRestart("muffleWarning")
    }
    list(value=withCallingHandlers(tryCatch(expr, error=function(e) e),
                                   warning=w.handler), 
         warning=W)
} # tryCatch.W.E

checkfun <- function() {
    message("myfunctions.r: sys.parent() = ", sys.parent(), 
            ", current frame sys.nframe() = ", sys.nframe())
    # copypaste/Rscript: pa0,fr0; checkfun(): pa0,fr1; source("file.r") = pa3,fr4
    f1 <- function() message("f1(): sys.parent() = ", sys.parent(), 
                             ", current frame sys.nframe() = ", sys.nframe())
    f2 <- function() f1(); f3 <- function() f2()
    message("run f1"); f1() # copypaste/Rscript: pa0,fr1; checkfun() = pa1,fr2; source("file.r") = pa0,fr5
    message("run f2"); f2() # copypaste/Rscript: pa1,fr2; checkfun() = pa2,fr3; source("file.r") = pa5,fr6
    message("run f3"); f3() # copyüaste/Rscript: pa2,fr3; checkfun() = pa3,fr4; source("file.r") = pa6,fr7
    # --> test if run from file or as function:
    #if (sys.parent() == 3L && sys.nframe() == 4L) { # run `source(file.r")`
    #    me <- "file.r"
    #} else { # run `fun()`
    #    me <- normalizePath(getSrcFilename(fun, full.names=T))
    #}
} # checkfun

# check if all elements of a list are identical
# https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
identical_list <- function(x) {
    if (length(x) == 1) {
        return(T)
    } else if (length(x) == 0) {
        return(F)
    } else {
        check <- vapply(1:(length(x)-1),
                    function(n) identical(x[[n]], x[[n+1]]),
                    logical(1))
        if (all(check)) T else F
    }
}

# check of graphics::image()'s argument `useRaster` from graphics::image.default 
check_irregular <- function(x, y) {
    dx <- diff(x)
    dy <- diff(y)
    # all.equal(target, current, ...) returns
    #   TRUE            if d <= tolereance with
    #                       tolerance <- sqrt(.Machine$double.eps)
    #                       d <- (sum(abs(target - current))/length(target))
    #   a character     otherwise, giving the mean relative difference, 
    #                   e.g. "Mean relative difference: 0.2"; or other 
    #                   information like "Numeric: lengths (18, 1) differ"
    #                   if the lengths of target and current differ
    # isTRUE(x) returns
    #   TRUE if is.logical(x) && length(x) == 1L && !is.na(x) && x
    (length(dx) && !isTRUE(all.equal(dx, rep(dx[1], length(dx))))) ||
    (length(dy) && !isTRUE(all.equal(dy, rep(dy[1], length(dy)))))
}

# minute/second degree to decimal degree longitude/latitude
deg2dec <- function(deg=0.0, min=0.0, sec=0.0) {
    if (length(deg) == 1 && length(min) == 1 && length(sec) == 1) {
        message("deg2dec(): dec = deg + min/60 + sec/3600 = ", deg, " + ", min, "/60 + ", sec, "/3600")
    } else {
        message("deg2dec(): dec = deg + min/60 + sec/3600")
    }
    dec <- deg + min/60 + sec/3600
    return(dec)
} # deg2dec

# leap years
is.leap <- function(years) {
    return(((years %% 4 == 0) & (years %% 100 != 0)) | (years %% 400 == 0))
}

reorder_legend <- function(le) {
    # check input
    if (le$ncol < 1) warning("reorder_legend(): le$ncol=", ncol, ". set to 1")
    le$ncol <- max(1, le$ncol)
    n <- length(le$text)
    nrow <- ceiling(n/le$ncol)
    # https://stackoverflow.com/questions/39552682/base-r-horizontal-legend-with-multiple-rows
    MyOrder <- as.vector(matrix(1:(nrow*le$ncol), nrow=nrow, ncol=le$ncol, byrow=T))
    
    # reorder every list element of length n
    for (i in 1:length(le)) {
        if (length(le[[i]]) == n) {
            le[[i]] <- le[[i]][MyOrder]
        }
    }
    return(le)
} # reorder_legend

myma <- function(x, order, verbose=F, ...) {
    if (verbose) {
        message("yields the same result as\n,
                forecast::ma(x, order=order, centre=ifelse(order %% 2 == 0, F, T))\n
                monthly ts --> order=36 --> 3a ma\n
                daily   ts --> order=\n")
    }
    y <- stats::filter(x, filter=rep(1/order, t=order))
}

ht <- function(d, nmax_per_side=15) {
    if (!is.null(dim(d)) && length(dim(d)) > 2) stop("ht(): input d needs to be 1- or 2-dimensional")
    if (is.null(dim(d))) nd <- length(d)
    if (!is.null(dim(d))) nd <- dim(d)[1]
    if (nd <= 2*nmax_per_side) {
        print(d)
    } else {
        print(head(d, n=nmax_per_side))
        #message(system('bold=`tput bold`; printf "   ${bold}\u22ee"', intern=T))
        message(system('printf "   \u22ee"', intern=T))
        print(tail(d, n=nmax_per_side))
    }
} # ht()

# make POSIX time with negative years
make_posixlt_origin_function <- function(years, origin_in=0, origin_out=0, verbose=0) {

    # input: 
    #   years (numeric; negative for before `origin_in`)
    #   origin_in (numeric; origin of input years; default: 0 BC/AD)
    #   origin_out (numeric; origin of output years; default: 0 BC/AD)
    # output:
    #   dates (POSIXlt; date values with respect to `origin_out`)
    if (missing(years)) stop("must provide years")
    if (!is.numeric(years)) stop("years must be numeric")

    if (verbose > 0) {
        message("************** todo: check numeric year.dec values ***************")
        message("years:")
        print(head(years))
        print(tail(years))
        message("origin_in = ", origin_in, "\n",
                "origin_out = ", origin_out)
    }

    # capture different cases
    years_ge_zero_lt_10000 <- which(years >= 0 & years < 10000) # case 1
    years_ge_zero_ge_10000 <- which(years >= 0 & years >= 10000) # case 2
    years_lt_zero <- which(years < 0) # case 3
    
    # capture NA
    nainds <- which(is.na(years))
        
    # case 1
    if (length(years_ge_zero_lt_10000) > 0) { # as.POSIXlt("0-1-1") to as.POSIXlt("9999-12-31") -> ok
        if (verbose > 0) {
            message("case 1 years[years_ge_zero_lt_10000]:")
            cat(capture.output(str(years[years_ge_zero_lt_10000])), sep="\n")
        }
        lt_ge_zero_lt_10000 <- as.POSIXlt(paste0(years[years_ge_zero_lt_10000], "-06-30"), tz="UTC") 
        if (verbose > 0) {
            message("case 1 lt_ge_zero_lt_10000:")
            cat(capture.output(str(lt_ge_zero_lt_10000)), sep="\n")
        }
    } # case 1

    # case 2
    if (length(years_ge_zero_ge_10000) > 0) { # as.POSIXlt("10000-1-1") and above -> error "not in a standard unambiguous format"
        lt_ge_zero_ge_10000 <- as.POSIXlt(paste0("1337-06-30"), tz="UTC") # placeholder for saving results
        for (yeari in seq_along(years_ge_zero_ge_10000)) {
            by <- paste0(years[years_ge_zero_ge_10000][yeari], " years")
            tmp <- seq.POSIXt(from=as.POSIXlt("0000-06-30", tz="UTC"), l=2, b=by, tz="UTC")[2]
            lt_ge_zero_ge_10000[yeari] <- as.POSIXlt(tmp)
            if (verbose > 1) message("case 2 year ", years[years_ge_zero_ge_10000][yeari], ": from 0 by ", by, " --> ", 
                                     lt_ge_zero_ge_10000[yeari], " since ", origin_in)
        } # for yeari
        if (verbose > 0) {
            message("case 2 lt_ge_zero_ge_10000:")
            cat(capture.output(str(lt_ge_zero_ge_10000)), sep="\n")
        }
    } # case 2

    # case 3
    if (length(years_lt_zero) > 0) { # below or equal as.POSIXlt("-1-1-1") --> negative year gives error "not in a standard unambiguous format" 
        lt_lt_zero <- as.POSIXlt(paste0("1337-06-30"), tz="UTC") # placeholder for saving results
        for (yeari in seq_along(years[years_lt_zero])) {
            by <- paste0(years[years_lt_zero][yeari], " years")
            tmp <- seq.POSIXt(from=as.POSIXlt("0000-06-30", tz="UTC"), l=2, b=by, tz="UTC")[2]
            lt_lt_zero[yeari] <- as.POSIXlt(tmp)
            if (verbose > 1) message("case 3 year ", years[years_lt_zero][yeari], ": from 0 by ", by, " --> ", 
                                     lt_lt_zero[yeari], " since ", origin_in)
        } # for yeari
        if (verbose > 0) {
            message("case 3 lt_lt_zero:")
            cat(capture.output(str(lt_lt_zero)), sep="\n")
        }

    } # if any negative years since origin_in

    # combine all cases
    posixlt <- as.POSIXlt(seq.POSIXt(as.POSIXlt("0-1-1", tz="UTC"), b="1 day", l=length(years))) # placeholder
    if (length(nainds) > 0) {
        posixlt[nainds] <- NA
    }
    if (length(years_ge_zero_lt_10000) > 0) {
        posixlt[years_ge_zero_lt_10000] <- lt_ge_zero_lt_10000
    }
    if (length(years_ge_zero_ge_10000) > 0) {
        posixlt[years_ge_zero_ge_10000] <- lt_ge_zero_ge_10000
    }
    if (length(years_lt_zero) > 0) {
        posixlt[years_lt_zero] <- lt_lt_zero
    }
    if (verbose > 0) {
        message("posixlt:")
        cat(capture.output(str(posixlt)), sep="\n")
    }

    # shift to new origin
    if (origin_out != origin_in) {
        
        shift_by <- origin_in - origin_out
        if (verbose > 0) message("shift\n   ", min(posixlt), " to ", max(posixlt), " since ", origin_in, "\n", 
                             "by ", shift_by, " years:")
        posixlt$year <- posixlt$year + shift_by
        if (verbose > 0) {
            message("   ", min(posixlt), " to ", max(posixlt), " since ", origin_out)
            message("class(posixlt) = ", class(posixlt))
        }

    } # if origin_out != origin_in or not

    # append origin
    attributes(posixlt)$origin <- origin_out
    if (verbose > 0) message("class(posixlt) = ", paste(class(posixlt), collapse=", "))
    
    # fix time zone
    #posixlt$zone <- rep("UTC", t=length(posixlt)) # todo: this breaks if input `years` is of length 1
    #if (verbose > 0) message("6 class(posixlt) = ", class(posixlt))

    # sort
    #posixlt <- sort(posixlt)

    return(posixlt)

} # make_posixlt_origin_function

# Get the proportion variation explained. See this website for more details: http://goo.gl/jte8X
# http://www.gettinggeneticsdone.com/2011/08/sync-your-rprofile-across-multiple-r.html
get_rsq <- function(predicted, actual) {
    # "goodness of fit" = 
    # "coefficient of determination" = 
    # "fraction of variance explained by the model" = 
    # "Multiple R-squared:"-result of lm()
    1-sum((predicted-actual)^2)/sum((actual-mean(actual))^2)
}

# get p-value of linear model; from 3.2.1 of faraways book
get_pval <- function(lm) {
    if (class(lm) != "lm") stop("input must be of class \"lm\"")
    # model[[1]] is actual data that was modeled by predictors model[[2..]]
    a <- sum((lm$model[[1]]-mean(lm$model[[1]]))^2) 
    b <- sum(lm$res^2)
    n_predictors <- lm$rank-1
    n_df <- lm$df.residual
    c <- (a-b)/n_predictors/(b/n_df)
    1-pf(c, n_predictors, n_df)
}

grl_nfigs2nwords <- function(nfigs=1:12, ntabs) {
    # in GRL, 1 paper consists of 12 "publication units" (PU) max.
    pu_max <- 12
    # 1 PU = 500 words = 1 fig = 1 tab
    nwords_per_pu <- 500; nfigs_per_pu <- ntabs_per_pu <- 1
    # -> there is a maximum number of words per paper as a function of the number of figures and tables
    if (missing(ntabs)) ntabs <- rep(0, t=length(nfigs))
    if (length(ntabs) != length(nfigs)) stop("nfigs and ntabs are of different length")
    nwords <- pu_max*nwords_per_pu - (nfigs + ntabs)*nwords_per_pu
    df <- data.frame(nfig=nfigs, ntab=ntabs, nwords=nwords)
    print(df, row.names=F)
    return(df)
}

# color2rgb
col2rgba <- function(x, alpha) {
    if (missing(x)) stop("must provide colors")
    if (missing(alpha)) {
        apply(col2rgb(x)/255, 2, function(x) rgb(matrix(x, ncol=3)))
    } else {
        apply(col2rgb(x)/255, 2, function(x) rgb(matrix(x, ncol=3), alpha=alpha))
    }
}

# my colors
mycols <- function(n) {
    if (n == 1) { # mycols my colors nicer than R defaults
        cols <- "black"
    } else if (n == 2) {
        cols <- c("black", "#E41A1C") # black, myred
    } else if (n >= 3) {
        # black, myred, myblue instead of R default (black, red, blue)
        cols <- c("black", "#E41A1C", "#377EB8") 
        if (n > 3) {
            if (F) {
                cols <- c(cols, 4:n)
            } else if (T) {
                library(RColorBrewer) # https://www.r-bloggers.com/palettes-in-r/
                cols <- c(cols, brewer.pal(max(3, n), "Dark2")[1:(n-3)])
            }
        }
    }
    return(cols)
} # mycols

# convert velocities with units package
speeds <- function(x=1, unit="cm/s") {
    library(units) # valid_udunits()
    x <- set_units(x=x, value=unit, mode="standard")
    lengths <- c("0.1mm", "mm", "cm", "10cm", "m", "10m", "100m", "km", "3km", "10km", "100km", "1000km")
    times <- c("s", "min", "h", "day", "month", "year", "kyear", "Myear")
    vec <- as.vector(outer(lengths, times, paste, sep="/"))
    for (i in 1:length(vec)) vec[i] <- set_units(x, value=vec[i], mode="standard")
    mat <- matrix(as.numeric(vec), nrow=length(lengths))
    rownames(mat) <- lengths; colnames(mat) <- times
    oo <- getOption("scipen") # oldoption
    options("scipen"=100); message("scipen=", getOption("scipen"))
    print(mat)
    options("scipen"=oo); message("scipen=", getOption("scipen"))
    invisible(mat)

} # speeds function

# get file format
cdo_get_filetype <- function(fin, cdo="cdo", ncdump="ncdump", verbose=T) {

    if (verbose) message("cdo_get_filetype() start with `verbose`=T ...")
    cmd <- paste0(cdo, " showformat ", fin)
    if (verbose) message("run `", cmd, "`")
    input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
    if (!is.null(input_format$warning)) { # `cdo showformat` yields warn/error
        # `cdo showformat` on fesom data yields error:
        # Warning (cdf_read_xcoord): Unsupported array structure, skipped variable tosga!
        # Warning (cdfInqContents): No data arrays found!
        # Unsupported file structure
        message(input_format$warning$message)
        message("-> try to run `", ncdump, " -k` instead ...")
        cmd <- paste0(ncdump, " -k ", fin)
        if (verbose) message("run `", cmd, "`")
        input_format <- tryCatch.W.E(expr=eval(parse(text=paste0("system(cmd, intern=T)"))))
        if (!is.null(input_format$warning)) { # `ncdump -k` yields also warn/error
            stop(input_format$warning$message)
        } else { # no warning on `ncdump -k` 
            if (verbose) {
                for (i in seq_along(input_format$value)) {
                    message("--> \"", input_format$value[i], "\"")
                }
                if (length(input_format$value) > 1) {
                    message("take last entry")
                    input_format$value <- input_format$value[length(input_format$value)]
                }
                message("--> ", appendLF=F)
            }
        }
    } else { # no warning on `cdo showformat`
        if (verbose) {
            for (i in seq_along(input_format$value)) {
                message("--> \"", input_format$value[i], "\"")
            }
            if (length(input_format$value) > 1) {
                message("take last entry")
                input_format$value <- input_format$value[length(input_format$value)]
            }
        }
    }
    if (any(input_format$value == c(# from cdo showformat:
                                    "netCDF", "NetCDF", "NetCDF2", 
                                    "NetCDF4 classic zip", "netCDF-4 classic model",
                                    # from ncdump -k:
                                    "64-bit offset"))) {
        if (verbose) message("--> input is of type netcdf  ...")
        file_type <- "nc"
    } else { # e.g. "GRIB", "EXTRA  BIGENDIAN", "EXTRA  LITTLEENDIAN"
        if (verbose) message("--> input is not of type netcdf or not known")
        file_type <- "non-nc"
    }
    
    if (verbose) message("cdo_get_filetype() finished")
    return(list(file_type=file_type, exact_format=input_format$value))

} # cdo_get_filetype

# get coordinates of selected ('clicked') point on plot
get_coords <- function(...) {

    # open plot with world map if no plot is already open 
    if (is.null(dev.list())) {
        library(maps)
        message("run maps::map(\"world\", interior=F) ...")
        map("world", interior=F)
        message("par(\"usr\") = ", appendLF=F)
        dput(par("usr"))
    } 
    options(locatorBell=F) # turn off system beep
    message("run `graphics::locator()` ... (to exit the locator, hit any mouse ",
            "button but the first while the mouse is over the plot device and looks like a cross) ...")
    graphics::locator(type="o", ...)
} # get_coords

# set my default plot options
setDefaultPlotOptions <- function(plist=list(plot_type="png", bg_col="white", NA_col="gray65", 
                                             contour_labcex=1,
                                             a4_width_in=8.26, a4_height_in=11.68,
                                             ts_width=2666, ts_height=1600, 
                                             ts_width_m=2000, ts_height_m=1600,
                                             depth_width=2666, depth_weight=1600,
                                             map_width=2666, map_height=2000,
                                             scatter_width=2666, scatter_height=2666,
                                             useRaster=T, ppi=400, inch=7, pointsize=12, 
                                             family_png="sans", family_pdf="sans"), ...) {
    dot_list <- list(...)
    dot_names <- names(dot_list)
    if (length(dot_list) > 0) {
        for (i in 1:length(dot_list)) {
            if (i == 1) message("*** setDefaultPlotOptions() start ***")
            if (dot_names[i] == "plot_type") {
                if (!any(dot_list[[i]] == c("png", "pdf"))) {
                    stop("setDefaultPlotOptions(): given plot_type ", 
                         dot_list[[i]], " not defined.")
                }
                if (dot_list[[i]] == plist$plot_type) {
                    next # argument
                }
            }
            if (any(dot_names[i] == c("family_png", "family_pdf"))) {
                if (any(search() == "package:extrafont")) {
                    if (!any(fonts() == dot_list[[i]])) { # do not update font
                        next # argument 
                    }
                }
            }
            if (T) message("argument \"", dot_names[i], "\" provided. overwrite default \"", 
                           plist[[dot_names[i]]], "\" with \"", dot_list[[i]], "\"")
            plist[[dot_names[i]]] <- dot_list[[i]]
            if (i == length(dot_list)) message("*** setDefaultPlotOptions() finished ***")
            ## Note: 
            # print(str(dot_list[[i]]))
            # returns the value AND NULL
            #str(dot_list[[i]]) 
        } # for all arguments in dots `...`
    } # if any dots arguments given
    return(plist)
}

# nicer default pars
# attention: this overwrites the default par()
#par <- function(las=1, ...) {
#    graphics::par(las=las, ..., no.readonly = FALSE)
#} # usage: par(); plot(...)

# paste my relevant plot options
mypar <- function() {
    if (is.null(dev.list())) {
        y <- askYesNo("mypar(): no plot open. would you like to run 'dev.new()'?", 
                      default=F, prompts=c("Yes", "No", "Cancel"))
        if (y) {    
            dev.new()
        }
    }
    if (!is.null(dev.list())) {
        op <- par()
        message("current device no. ", dev.cur(), " is of type ", 
                names(dev.cur()), " with family='", op$family, "'")
        message("character width, height")
        message("   par(\"cin\") = c(", paste0(op$cin, collapse=", "), ") # inch")
        message("   par(\"cra\") = c(", paste0(op$cra, collapse=", "), ") # px; default = 1/72 = 0.01388889 inch") 
        message("   --> par(\"cra\")/par(\"cin\") = c(", paste0(op$cra/op$cin, collapse=", "), ") # ppi if device is png")
        message("device width, height (whole plot):")
        message("   par(\"fin\") = c(", paste0(par("fin"), collapse=", "), ") # inch")
        message("   dev.size(\"in\") = c(", paste0(dev.size("in"), collapse=", "), ")")
        message("   dev.size(\"px\") = c(", paste0(dev.size("px"), collapse=", "), ")")
        message("   dev.size(\"cm\") = c(", paste0(dev.size("cm"), collapse=", "), ")")
        message("plot width, height (where data is shown):")
        message("   par(\"pin\") = c(", paste0(op$pin, collapse=", "), ") # inch")
        message("   --> par(\"pin\")*par(\"cra\")/par(\"cin\") = c(", 
                paste0(op$pin*op$cra/op$cin, collapse=", "), ") # px")
        message("   --> par(\"pin\")*2.54 = c(", 
                paste0(op$pin*2.54, collapse=", "), ") # cm")
        message("plot coordinates:")
        message("   par(\"usr\") = c(", paste0(op$usr, collapse=", "), ") # l r b t")
        message("plot coordinates relative:")
        message("   par(\"plt\") = c(", paste0(op$plt, collapse=", "), ") # l r b t")
        message("margin size:")
        message("   par(\"omi\") = c(", paste0(op$omi, collapse=", "), ") # inch; b l t r")
        message("   --> par(\"omi\")*par(\"cra\")/par(\"cin\") = c(", 
                paste0(op$omi*op$cra/op$cin, collapse=", "), ") # px; b l t r")
        message("   --> par(\"omi\")*2.54 = c(", 
                paste0(op$omi*2.54, collapse=", "), ") # cm; b l t r")
        message("   par(\"mai\") = c(", paste0(op$mai, collapse=", "), ") # inch; b l t r (default: c(1.02, 0.82, 0.82, 0.42))")
        message("   --> par(\"mai\")*par(\"cra\")/par(\"cin\") = c(", 
                paste0(op$mai*op$cra/op$cin, collapse=", "), ") # px; b l t r")
        message("   --> par(\"mai\")*2.54 = c(", 
                paste0(op$mai*2.54, collapse=", "), ") # cm; b l t r")
        if (!all(op$oma == 0)) {
            message("sum outer and inner margin sizes")
            message("   c(", paste0(op$omi + op$mai, collapse=", "), ") # inch; b l t r")
            message("   c(", paste0(op$omi*op$cra/op$cin + op$mai*op$cra/op$cin, collapse=", "), ") # px; b l t r")
            message("   c(", paste0((op$omi + op$mai)*2.54, collapse=", "), ") # cm; b l t r")
        }
        message("margin rows:")
        message("   par(\"oma\") = c(", paste0(op$oma, collapse=", "), ") # b l t r")
        message("   par(\"mar\") = c(", paste0(op$mar, collapse=", "), ") # b l t r (default: c(5.1, 4.1, 4.1, 2.1) + 0.1)")
        if (!all(op$oma == 0)) {
            message("sum outer and inner margins rows")
            message("   c(", paste0(op$oma + op$mar, collapse=", "), ") # b l t r")
            message("   c(", paste0(op$oma*op$cra/op$cin + op$mar*op$cra/op$cin, collapse=", "), ") # b l t r")
            message("   c(", paste0((op$oma + op$mar)*2.54, collapse=", "), ") # b l t r")
        }
    }
} # mypar()

par_px2in <- function(px) {
    if (is.null(dev.list())) {
        stop("par_px2in(): no plot open.")
    } else { 
        op <- par()
        inch <- px*op$cin/op$cra
        message("in the current device ", dev.cur(), " (", names(dev.cur()), ")")
        message("   1 px*", op$cin[1], "/", op$cra[1], " = ", op$cin[1]/op$cra[1], " inch wide")
        message("   1 px*", op$cin[2], "/", op$cra[2], " = ", op$cin[2]/op$cra[2], " inch high")
        inch
    }
} # par_px2in 

# load function for packages
load_packages <- function(pkgs) {
    status <- sapply(pkgs, function(pkg) {
                         if (!any(search() == paste0("package:", pkg))) {
                             library(pkg, character.only=T, logical.return=T)
                         } else {
                             message("package '", pkg, "' already loaded.")
                         }})
    if (length(unlist(status)) > 0) {
        stop()
    }
} # load_packages

# Get month names in specific locale
mymonth.name <- function(inds, locales=Sys.getlocale("LC_TIME")) {
 
    ## https://stat.ethz.ch/pipermail/r-help/2004-May/051503.html

    if (any(!(inds %in% 1:12))) {
        stop("mymonth.name(): 'inds' must be in 1:12")
    }

    months <- vector("list", l=length(locales))
    names(months) <- locales

    # save system locale for later
    locale.bak <- Sys.getlocale("LC_TIME")

    for (i in 1:length(locales)) {

        # change locale is needed
        if (Sys.getlocale("LC_TIME") != locales[i]) {
            
            # check if locale can be changed; returns the locale (success) or "" (no success)
            status <- tryCatch(suppressWarnings(Sys.setlocale("LC_TIME", locales[i])),
                               error=function(e) e,
                               warning=function(w) w)
            if (status != "") Sys.setlocale("LC_TIME", locales[i])
        } # if locale change is necessary

        # only evaulate result if system has correct locale
        if (Sys.getlocale("LC_TIME") == locales[i]) {
            months[[i]] <- format(ISOdate(2004, inds, 1), "%B")
        }

    } # for i locales

    if (any(sapply(months, is.null))) {
        locale_avail <- system("localedef --list-archive", intern=T)
        if (length(locale_avail) > 0) {
            months[[i+1]] <- locale_avail
        }
        names(months)[i+1] <- "locale_avail"
    } # if any locales not successful

    # restore original locale if necessary
    if (Sys.getlocale("LC_TIME") != locale.bak) Sys.setlocale("LC_TIME", locale.bak)

    return(months)

} # mymonth.name function


get_encoding <- function(test_symbol="ä", test_ctype="de", verbose=F) {
    
    encoding <- iconvlist()
    n <- length(encoding)
    encoding <- sapply(encoding, function(x) iconv(test_symbol, from=x))
    if (any(is.na(encoding))) encoding <- encoding[!is.na(encoding)]
    if (any(encoding == test_symbol)) {
        encoding <- encoding[which(encoding == test_symbol)]
        if (verbose) {
            message(length(encoding), "/", n, 
                    " encodings can print test_symbol \"", test_symbol, "\":")
            message(paste(names(encoding), collapse=" "))
        }
        encoding <- names(encoding[1])
        message("get_encoding(): first found encoding that matches \"", 
                test_symbol, "\": ", encoding)
    } else {
        message("could not find an encoding to evaluate test_symbol = \"", 
                test_symbol, "\"")
    }
	ctype <- Sys.getlocale("LC_CTYPE")

    # repeat for ctype 
	if (regexpr(test_ctype, ctype, ignore.case=T) == -1) {
		
		# get available langs
		locs <- system("locale -a", intern=T)
		# check if german character type is possible
		if (!any(regexpr(test_ctype, locs, ignore.case=T) != -1)) {
			message("could not find a locale contaning the pattern \"", 
                    test_ctype, "\" (case is ignored)")
		} else {
			locs <- locs[which(regexpr(test_ctype, locs, ignore.case=T) != -1)]
			message("Sys.getlocalte(\"LC_CTYPE\"): ", ctype)
			message("Run Sys.setlocale(\"LC_CTYPE\", \"", locs[1], "\")")
			Sys.setlocale("LC_CTYPE", locs[1])
			message("Sys.getlocale(\"LC_CTYPE\"): ", Sys.getlocale("LC_CTYPE"))
		
			# get available encodings
			encoding <- iconvlist()
            n <- length(encoding)
			encoding <- sapply(encoding, function(x) iconv(test_symbol, from=x))
			if (any(encoding == test_symbol)) {
                if (verbose) {
                    message(length(encoding), "/", n, 
                            " encodings can print test_symbol \"", test_symbol, "\":")
                    message(paste(names(encoding), collapse=" "))
                }
                encoding <- names(encoding[1])
                message("get_encoding(): first found encoding that matches \"", 
                        test_symbol, "\": ", encoding)
			} else {
				message("could not find an encoding to evaluate test_symbol = \"", 
                        test_symbol, "\"")
			}
		} # if pattern test_ctype is contained in locale -a
	} else {
        if (verbose) message("test_ctype = \"", test_ctype, 
                             "\" in `Sys.getlocale(\"LC_CTYPE\")` = ", ctype) 
    } # if current LC_CTYPE does not contain test_ctype
    return(encoding)
} # get_encoding

# check gs command from grDevices::embedFonts()
myembedFonts <- function (file, format, outfile = file, fontpaths = character(), 
    options = character()) 
{
    if (!is.character(file) || length(file) != 1L || !nzchar(file)) 
        stop("'file' must be a non-empty character string")
    gsexe <- tools::find_gs_cmd()
    if (!nzchar(gsexe)) 
        stop("GhostScript was not found")
    if (.Platform$OS.type == "windows") 
        gsexe <- shortPathName(gsexe)
    suffix <- gsub(".+[.]", "", file)
    if (missing(format)) 
        format <- switch(suffix, ps = , eps = "ps2write", pdf = "pdfwrite")
    if (!is.character(format)) 
        stop("invalid output format")
    grDevices:::check_gs_type(gsexe, format)
    tmpfile <- tempfile("Rembed")
    if (length(fontpaths)) 
        fontpaths <- paste0("-sFONTPATH=", shQuote(paste(fontpaths, 
            collapse = .Platform$path.sep)))
    args <- c(paste0("-dNOPAUSE -dBATCH -q -dAutoRotatePages=/None -sDEVICE=", 
        format), paste0(" -sOutputFile=", shQuote(tmpfile)), 
        fontpaths, options, shQuote(file))
    message("run `", gsexe, " ", args, "` ...")
    ret <- system2(gsexe, args)
    if (ret != 0) 
        stop(gettextf("status %d in running command '%s'", ret, 
            cmd), domain = NA)
    if (outfile != file) 
        args[2] <- paste0(" -sOutputFile=", shQuote(outfile))
    cmd <- paste(c(shQuote(gsexe), args), collapse = " ")
    file.copy(tmpfile, outfile, overwrite = TRUE)
    invisible(cmd)
} # from grDevices::embedFonts()

myErrorFun <- function() {
    # default: getOption("error") = NULL
    # weird bug: When running a script from the command line you will have to skip one or two traceback() calls
    # https://stackoverflow.com/questions/1445964/r-script-line-numbers-at-error
    # https://stackoverflow.com/questions/13116099/traceback-for-interactive-and-non-interactive-r-sessions
    # #traceback(2) #recover() #rlang::last_trace(); rlang::last_error(); # library(rlang)
    tmp <- base::.traceback(2)
    #tmp <- base::traceback(2)
    #message("str(tmp)")
    #print(str(tmp))
    if (!is.null(tmp) && # if e.g. not ctrl+c
        #regexpr("stop\\(", tmp[[1]]) != -1 && # if stop() was called last
        !is.null(srcref <- attr(tmp[[1]], "srcref"))) { # if attribute exists
        message("in line ", srcref[1], " in ", basename(attr(srcref, "srcfile")$filename))
    } else {
        #message("str(tmp)")
        #print(str(tmp))
    }
}

mynews <- function() {
    current_version <- paste0(R.version$major, ".", R.version$minor)
    db <- news(Version==current_version)$Text
    db <- gsub("\\\n", " ", db)
    message("R ", current_version, " news:")
    for (i in seq_along(db)) message(i, "/", length(db), ": ", db[i])
}

# paste stuff that I always forget
myhelp <- function() {
	tmp <- c("   Built-in constants ...",
             "      LETTERS, letters, month.abb, month.name, pi",
             "   extrafont ...",
             "      font_import() (loadfonts()), fonts(), font_install(\"fontcm\"), \"CM *\"",
             "      embed_fonts(\"plot.pdf\", outfile=\"plot_embed.pdf\") (PostScript knows only 14 base fonts)",
             "      evince -> File -> Properties -> Fonts -> \"Embedded subset\"",
             "      library(extrafontdb) (reset)",
             "   available locales ...",
             "      locs <- system(\"locale -a\", intern=T)",
             "   read file with specific encoding ...",
             "      iconvlist <- iconvlist()",
             "   POSIX ...",
             "      d <- as.POSIXct(\"2000-01-30\", format=\"%Y-%m-%d\", tz=\"CET\") # = \"2000-01-30 CET\"",
             "      n <- as.numeric(d) # = 949186800",
             "      dd <- as.POSIXct(n, origin=\"1970-01-01\",tz=\"CET\") # = \"2000-01-30 CET\"",
             "          %d	Day of the month (decimal number)",
             "          %m	Month (decimal number)",
             "          %b	Month (abbreviated)",
             "          %B	Month (full name)",
             "          %y	Year (2 digit)",
             "          %Y	Year (4 digit)",
             "   Package options ...",
             "      install: install.packages(\"pkg\", lib=\"/path/\", configure.args=\"--help\")",
             "               install.packages(\"pkg\", lib=\"/path/\", configure.args=\"--with-pkg-lib=/path/ --with-pkg-include=\")",
             "               rgeos:",
             "                  install.packages(\"rgeos\", configure.args=\"--with-geos-config=/sw/rhel6-x64/geos-3.4.2-gcc48/bin/geos-config\")",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/geos-3.6.1-gcc48/lib/:$LD_LIBRARY_PATH",
             "               rgdal:",
             "                  install.packages(\"rgdal\", configure.args=\"--with-gdal-config=/sw/rhel6-x64/gdal-2.1.3-gcc48/bin/gdal-config --with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include --with-proj-lib=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib\")",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/gdal-2.1.3-gcc48/lib/:$LD_LIBRARY_PATH",
             "                  export LD_LIBRARY_PATH=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib/:$LD_LIBRARY_PATH",
             "               devtools::install_github(\"user/package\", args=\"--with-keep.source\")",
             "               withr::with_libpaths(new=\"libpath\", install_github(\"user/package\"))",
             "      compile: R CMD build \"package directory\"",
             "               R CMD INSTALL -l \"lib\" \"pkg.tar.gz\"",
             "               install.packages(\"pkg.tar.gz\", repos=NULL)",
             "      load:    library(pkg, lib=\"lib\")",
             "      unload:  detach(package:pkg, unload=T)",
             "      update:  update.packages(lib=\"lib/to/update\", instlib=\"lib/to/put/updates\", ask=F, checkBuilt=T)",
             "               update.packages(instlib=.libPaths()[1], ask=F, checkBuilt=T)",
             "               old <- old.packages(lib=.libPaths()[1])",
             "               dtupdate::github_update(auto.install=T, ask=T, dependencies=T)",
             "      remove:  remove.packages(\"pkg\", lib=\"lib\")",
             "      which:   packageDescription(\"pkg\"); packageVersion(\"pkg\"); find.package(\"pkg\"); maintainer(\"pkg\"); library(help=\"pkg\")",
             "      archive: https://cran.r-project.org/src/contrib/Archive",
             "      help:    https://cran.r-project.org/doc/FAQ/R-FAQ.html",
             "   Functions ...",
             "      C-object infos: e.g. \"graphics:::C_image\"", 
             "      S3: getAnywhere(fun or \"fun\"); methods(fun or \"fun\")",
             "      S4: showMethods(fun or \"fun\"); getMethods(fun or \"fun\")",
             "      .Internal/.Primitive: pryr::show_c_source, e.g. `show_c_source(.Internal(mean(x)))`",
             "      R source code: https://github.com/wch/r-source.git",
             "   Find R ...",
             "      executable: R.home()/bin/exec/R",
             "   Run R ...",
             "      in background:            $ Rscript script.r > test.log 2>&1 &",
             "      as script:                #!/usr/bin/env Rscript",
             "      without this ~/.Rprofile: $ R --no-init-file (or --vanilla)")
    message(paste0(tmp, collapse="\n"))
}

# works in combi with
#export LD_LIBRARY_PATH=/sw/rhel6-x64/gdal-2.1.3-gcc48/lib/:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib/:$LD_LIBRARY_PATH
#install.packages("rgdal", configure.args=c("--with-gdal-config=/sw/rhel6-x64/gdal-2.1.3-gcc48/bin/gdal-config" 
#                                           ,"--with-proj-lib=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib"
#                                           ,"--with-proj-include=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include"
#                                           ))

# works:
#install.packages("proj4", configure.args=c("PKG_LIBS=-L/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/lib"
#                                           ,"PKG_CPPFLAGS=-I/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48/include"
#                                           ))

# works in combi with export LD_LIBRARY_PATH=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/lib:$LD_LIBRARY_PATH: 
#install.packages("units", configure.args=c("--with-udunits2-lib=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/lib"
#                                           ,"--with-udunits2-include=/sw/rhel6-x64/util/udunits-2.2.26-gcc64/include"
#                                           ))

