## R

## my collection of small R functions

# nicer default pars
# attention: this overwrites the default par()
#par <- function(las=1, ...) {
#    graphics::par(las=las, ..., no.readonly = FALSE)
#} # usage: par(); plot(...)

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
    if (le$ncol < 1) {
        warning("reorder_legend(): le$ncol=", ncol, ". set to 1")
    }
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

    if (verbose > 0) {
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
        
    # case 1
    if (length(years_ge_zero_lt_10000) > 0) { # as.POSIXlt("0-1-1") to as.POSIXlt("9999-12-31") -> ok
        if (verbose > 0) {
            message("years[years_ge_zero_lt_10000]:")
            cat(capture.output(str(years[years_ge_zero_lt_10000])), sep="\n")
        }
        lt_ge_zero_lt_10000 <- as.POSIXlt(paste0(years[years_ge_zero_lt_10000], "-06-30"), tz="UTC") 
        if (verbose > 0) {
            message("lt_ge_zero_lt_10000:")
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
            if (verbose > 1) message("year ", years[years_ge_zero_ge_10000][yeari], ": from 0 by ", by, " --> ", 
                                     lt_ge_zero_ge_10000[yeari], " since ", origin_in)
        } # for yeari
        if (verbose > 0) {
            message("lt_ge_zero_ge_10000:")
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
            if (verbose > 1) message("year ", years[years_lt_zero][yeari], ": from 0 by ", by, " --> ", 
                                     lt_lt_zero[yeari], " since ", origin_in)
        } # for yeari
        if (verbose > 0) {
            message("lt_lt_zero:")
            cat(capture.output(str(lt_lt_zero)), sep="\n")
        }

    } # if any negative years since origin_in

    # combine all cases
    posixlt <- as.POSIXlt(seq.POSIXt(as.POSIXlt("0-1-1", tz="UTC"), b="1 day", l=length(years))) # placeholder
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
    posixlt$origin <- origin_out
    if (verbose > 0) message("class(posixlt) = ", class(posixlt))
    
    # fix time zone
    #posixlt$zone <- rep("UTC", t=length(posixlt)) # todo: this breaks if input `years` is of length 1
    #if (verbose > 0) message("6 class(posixlt) = ", class(posixlt))

    # sort
    #posixlt <- sort(posixlt)

    return(posixlt)

} # make_posixlt_origin_function

# Get the proportion variation explained. See this website for more details: http://goo.gl/jte8X
# http://www.gettinggeneticsdone.com/2011/08/sync-your-rprofile-across-multiple-r.html
rsq <- function(predicted, actual) {
    # "fraction of variance explained by the model"
    1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
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
    if (missing(alpha)) {
        apply(col2rgb(x)/255, 2, function(x) rgb(matrix(x, ncol=3)))
    } else {
        apply(col2rgb(x)/255, 2, function(x) rgb(matrix(x, ncol=3), alpha=alpha))
    }
}

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
            message("--> ", appendLF=F)
        }
    }
    if (any(input_format$value == c("GRIB", "EXTRA  BIGENDIAN", "EXTRA  LITTLEENDIAN"))) {
        if (verbose) message("convert to netcdf ...")
        convert_to_nc <- T
        file_type <- "grb"
    } else if (any(input_format$value == c(# from cdo showformat:
                                           "netCDF", "NetCDF", "NetCDF2", 
                                           "NetCDF4 classic zip", "netCDF-4 classic model",
                                           # from ncdump -k:
                                           "64-bit offset"))) {
        if (verbose) message("no need to convert to netcdf ...")
        convert_to_nc <- F
        file_type <- "nc"
    } else {
        if (verbose) message("not defined in cdo_get_filetype() ",
                             "-> assume that conversion to nc is not needed ",
                             "-> set `convert_to_nc` to F and continue ...")
        convert_to_nc <- F
        file_type <- input_format$value
    }

    return(list(convert_to_nc=convert_to_nc, file_type=file_type))

} # cdo_get_filetype

# set my default plot options
setDefaultPlotOptions <- function(plist=list(plot_type="png", bg_col="white", NA_col="gray65", 
                                             contour_labcex=1,
                                             ts_width=2666, ts_height=1600, 
                                             ts_width_m=2000, ts_height_m=1600,
                                             depth_width=2666, depth_weight=1600,
                                             map_width=2666, map_height=2000,
                                             scatter_width=2666, scatter_height=2666,
                                             useRaster=T, dpi=400, inch=7, 
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


find_encoding <- function(test_symbol="ä", test_ctype="de") {
    
    # find and possibly :
    # value so that "test_symbol" in interpreted in 
    # a good way.
    encoding <- iconvlist()
    encoding <- sapply(encoding, function(x) iconv(test_letter, from=x))
    encoding <- encoding[!is.na(encoding)]
    if (any(encoding == test_letter)) {
        encoding <- encoding[which(encoding == test_letter)]
        message(paste0("encoding ", names(encoding), ": ", encoding, collapse="\n"))
        encoding <- names(encoding[1])
        message("use encoding ", encoding)
    } else {
        message("Could not find a encoding to evaluate 'test_letter'=", test_letter)
    }
	ctype <- Sys.getlocale("LC_CTYPE")
	if (regexpr(test_ctype_pattern, ctype, ignore.case=T) == -1) {
		
		# get available langs
		locs <- system("locale -a", intern=T)
		# check if german character type is possible
		if (!any(regexpr(test_ctype_pattern, locs, ignore.case=T) != -1)) {
			message("Could not find a locale contaning the pattern '", test_ctype_pattern, "' (case is ignored)")
		} else {
			locs <- locs[which(regexpr(test_ctype_pattern, locs, ignore.case=T) != -1)]
			message("Sys.getlocalte(\"LC_CTYPE\"): ", ctype)
			message("Run Sys.setlocale(\"LC_CTYPE\", \"", locs[1], "\")")
			Sys.setlocale("LC_CTYPE", locs[1])
			message("Sys.getlocale(\"LC_CTYPE\"): ", Sys.getlocale("LC_CTYPE"))
		
			# get available encodings
			encoding <- iconvlist()
			encoding <- sapply(encoding, function(x) iconv(test_letter, from=x))
			if (any(encoding == test_letter)) {
				encoding <- encoding[which(encoding == test_letter)[1]]
				message("encoding '", names(encoding), "': ", encoding)
			} else {
				message("Could not find a encoding to evaluate 'test_letter'=", test_letter)
			}
		} # if pattern test_ctype_pattern is contained in locale -a
	} # if current LC_CTYPE does not contain test_ctype_pattern
} # not ready

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
             "               R CMD INSTALL -l \"lib\" \"packagename.tar.gz\"",
             "               install.packages(\"packagename.tar.gz\", repos=NULL)",
             "      load:    library(packagename, lib=\"lib\")",
             "      unload:  detach(package:packagename, unload=T)",
             "      update:  update.packages(lib=\"lib/to/update\", instlib=\"lib/to/put/updates\", ask=F, checkBuilt=T)",
             "               update.packages(instlib=.libPaths()[1], ask=F, checkBuilt=T)",
             "               old <- old.packages(lib=.libPaths()[1])",
             "               dtupdate::github_update(auto.install=T, ask=T, dependencies=T)",
             "      remove:  remove.packages(\"packagename\", lib=\"lib\")",
             "      which:   find.package(\"packagename\")",
             "      version: packageVersion(\"packagename\")",
             "      archive: https://cran.r-project.org/src/contrib/Archive",
             "   Functions ...",
             "      C-object infos: e.g. \"graphics:::C_image\"", 
             "      S3: getAnywhere(fun or \"fun\"); methods(fun or \"fun\")",
             "      S4: showMethods(fun or \"fun\"); getMethods(fun or \"fun\")",
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

