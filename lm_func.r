## R

# calcuate linear model and save statistics in nc file

#lm_func <- function(inx, iny, varx, vary, xunit, yunit, lm_dimids, ) {
    
    if (T) {
        inx <- "/ace/user/cdanek/post/echam5/select/temp2/cosmos-aso-wiso_echam5_holocene_wiso_mm_select_temp2_global_Jan-Dec_0004-6173.nc"
        iny <- "/ace/user/cdanek/post/echam5/select/wisoaprt_d/cosmos-aso-wiso_echam5_holocene_wiso_mm_select_wisoaprt_d_sellevel_2_global_Jan-Dec_0004-6173.nc"
        # or inx and iny are lists of length 1 containing numeric data matrices
        varx <- "temp2"
        vary <- "wisoaprt_d"
        xunit <- "degC"
        yunit <- "o/oo"
        # dimension indices of data that should be regressed:
        lm_dimids <- list(x=c(1, 2), y=c(1, 2, 3)) # counting from left to right from ncdump -h output; starting from zero 
        loop_along_dimids <- c(x=0, y=0) # counting from left to right from ncdump -h output; starting from zero
        loop_along_dimname <- "time"
        fout <- "/ace/user/cdanek/post/echam5/select/temp2_vs_wisoaprt_d/cosmos-aso-wiso_echam5_holocene_lm_wisoaprt_d_sellevel_2_by_temp2_global_Jan-Dec_0004-6173.nc" 
    } # testing

    ####################################################

    library(ncdf4) # needed for output (and possibly input)

    # check arguments
    vars2check <- c("inx", "iny", "varx", "vary", "xunit", "yunit", 
                    "lm_dimids", "loop_along_dimids", "loop_along_dimname",
                    "fout")
    if (!all(sapply(vars2check, exists))) {
        missing_vars <- which(!sapply(vars2check, exists))
        stop("provide \"", paste(vars2check[missing_vars], collapse="\", \""), "\"")
    }
    if (length(loop_along_dimids) != 2) {
        stop("loop_along_dimids must be of length 2")
    } else {
        loop_along_dimids <- as.character(loop_along_dimids)
    }

    # check xdata
    if (is.character(inx) && tools::file_ext(inx) == "nc") {
        inpathx <- dirname(inx)
        infnamex <- basename(inx)
        message("open ", inx)
        inx <- ncdf4::nc_open(inx)
    } else if (is.list(inx)) {

    } else {
        stop("class(inx) = ", class(inx), " not supported")
    }

    # check ydata
    if (is.character(iny) && tools::file_ext(iny) == "nc") {
        inpathy <- dirname(iny)
        infnamey <- basename(iny)
        message("open ", iny)
        iny <- ncdf4::nc_open(iny)
    } else if (is.list(iny)) {

    } else {
        stop("class(iny) = ", class(iny), " not supported")
    }

    # check x,y dims
    if (class(inx) == "ncdf4") {
        xdimids <- inx$var[[varx]]$dimids # nc dim order
        xdims <- rep(NA, t=length(xdimids))
        names(xdims) <- xdimids
        for (di in 1:length(xdims)) {
            xdims[di] <- inx$var[[varx]]$dim[[di]]$len
        }
    } else if (is.list(inx)) {
        xdims <- dim(inx$varx)
    }
    message("xdims = ", paste(xdims, collapse=", "), 
            " (xdimids = ", paste(xdimids, collapse=", "), ")")

    if (class(iny) == "ncdf4") {
        ydimids <- iny$var[[vary]]$dimids # nc dim order
        ydims <- rep(NA, t=length(ydimids))
        names(ydims) <- ydimids
        for (di in 1:length(ydims)) {
            ydims[di] <- iny$var[[vary]]$dim[[di]]$len
        }
    } else if (is.list(iny)) {
        ydims <- dim(iny$vary)
    }
    message("ydims = ", paste(ydims, collapse=", "), 
            " (ydimids = ", paste(ydimids, collapse=", "), ")")

    # check dimensions
    if (!(loop_along_dimids[1] %in% xdimids)) {
        stop("provided loop_along_dimids[1] = ", loop_along_dimids[1], " not in xdimids")
    } else {
        loop_along_dimids_r <- which(xdimids == loop_along_dimids[1])
    }
    if (!(loop_along_dimids[2] %in% ydimids)) {
        stop("provided loop_along_dimids[2] = ", loop_along_dimids[2], " not in ydimids")
    } else {
        loop_along_dimids_r <- c(loop_along_dimids_r, 
                                 which(ydimids == loop_along_dimids[2]))
    }
    if (xdims[loop_along_dimids[1]] != ydims[loop_along_dimids[2]]) {
        stop("xdims[loop_along_dimids[1]=", loop_along_dimids[1], "] = ", 
             xdims[loop_along_dimids[1]], 
             " != ydims[loop_along_dimids[2]=", loop_along_dimids[2], 
             "] = ", ydims[loop_along_dimids[2]])
    } else {
        n <- xdims[loop_along_dimids[1]]
        if (class(inx) == "ncdf4") {
            loop_along_dimvals <- inx$var[[varx]]$dim[[loop_along_dimids_r[1]]]$vals
        } else if (is.list(inx)) {
            stop("loop_along_dimvals need to be provided")
        }
    }
    if (!(all(lm_dimids$x %in% xdimids))) {
        stop("provided lm_dimids$x = ", paste(lm_dimids$x, collapse=", "), 
             " not in xdimids = ", paste(xdimids, collapse=", "))
    } else {
        lm_dimids_r <- list(x=xdimids[lm_dimids$x])
    }
    if (!(all(lm_dimids$y %in% ydimids))) {
        stop("provided lm_dimids$y = ", paste(lm_dimids$y, collapse=", "), 
             " not in ydimids = ", paste(ydimids, collapse=", "))
    } else {
        lm_dimids_r <- c(lm_dimids_r,
                         y=ydimids[lm_dimids$y])
    }

    # start and count indices
    startx_template <- rep(1, t=length(xdims))
    startx_template[loop_along_dimids_r[1]] <- "i"
    countx <- xdims
    countx[loop_along_dimids_r[1]] <- 1
    starty_template <- rep(1, t=length(ydims))
    starty_template[loop_along_dimids_r[2]] <- "i"
    county <- ydims
    county[loop_along_dimids_r[2]] <- 1

    # save linear regression result in nc file
    message("save linear regression results in\n\"", fout, "\"")
    dir.create(dirname(fout), recursive=T, showWarnings=F)
    if (file.access(dirname(fout), mode=2) != 0) {
        stop("do not have writing permission in ", dirname(fout))
    }

    # lm along given dimenson
    #n <- 2
    message("calc lm along dims of common length ", n, " ...")
    intercepts <- intercept_errors <- intercept_pvals <- rep(NA, t=n)
    slopes <- slope_errors <- slope_pvals <- rep(NA, t=n)
    for (i in 1:n) {

        message(i, "/", n)
        
        # read xdata
        startx <- as.numeric(gsub("i", i, startx_template))
        message("   read var: \"", varx, "\" from start=", paste(startx, collapse=","), 
                " (count=", paste(countx, collapse=","), ")")
        if (class(inx) == "ncdf4") {
            xdata <- ncvar_get(inx, varx, start=startx, count=countx, collapse_degen=F)
        } else if (is.list(inx)) {
            cmd <- rep(",", t=length(xdims))
            cmd[loop_along_dimids[1]] <- "i"
            cmd <- paste(cmd, collapse="")
            cmd <- paste0("xdata <- inx$varx[", cmd, "]")
            eval(parse(text=cmd))
        }

        # read ydata
        starty <- as.numeric(gsub("i", i, starty_template))
        message("   read var: \"", vary, "\" from start=", paste(starty, collapse=","), 
                " (count=", paste(county, collapse=","), ")")
        if (class(iny) == "ncdf4") {
            ydata <- ncvar_get(iny, vary, start=starty, count=county, collapse_degen=F)
        } else if (is.list(iny)) {
            cmd <- rep(",", t=length(ydims))
            cmd[loop_along_dimids[2]] <- "i"
            cmd <- paste(cmd, collapse="")
            cmd <- paste0("ydata <- iny$vary[", cmd, "]")
            eval(parse(text=cmd))
        }

        # check dims
        if (length(xdata) != length(ydata)) {
            stop("length(xdata) = ", length(xdata), 
                 " != length(ydata) = ", length(ydata))
        }

        # run linear regression on wanted dimensions
        #lm <- lm(as.vector(ydata) ~ as.vector(xdata))
        cmdx <- rep(",", t=length(xdims))
        cmdx[seq_len(length(xdims))[-lm_dimids$x]] <- 1
        cmdx <- paste0("as.vector(xdata[", paste(cmdx, collapse=""), "])")
        cmdy <- rep(",", t=length(ydims))
        cmdy[seq_len(length(ydims))[-lm_dimids$y]] <- 1
        cmdy <- paste0("as.vector(ydata[", paste(cmdy, collapse=""), "])")
        cmd <- paste0("lm <- lm(", cmdy, " ~ ", cmdx, ")")
        message("   ", cmd)
        eval(parse(text=cmd))

        # linear regression results
        lm_summary <- summary(lm) 
        #print(lm_summary)
        intercepts[i] <- lm_summary$coefficients[1,1]
        intercept_errors[i] <- lm_summary$coefficients[1,2]
        intercept_pvals[i] <- lm_summary$coefficients[1,4]
        slopes[i] <- lm_summary$coefficients[2,1]
        slope_errors[i] <- lm_summary$coefficients[2,2]
        slope_pvals[i] <- lm_summary$coefficients[2,4]

    } # for i n

    # save result
    if (class(inx) == "ncdf4") { # reuse dimension from unput
        loop_along_dim <- inx$dim[[loop_along_dimname]]
    } else {
        loop_along_dim <- ncdim_def(name=loop_along_dimname, units="", vals=loop_along_dimvals[1:2])
    }
    intercept_var <- ncvar_def("intercept", units=yunit, dim=loop_along_dim, missval=NA, prec="double")
    intercept_error_var <- ncvar_def("intercept_error", units=yunit, dim=loop_along_dim, missval=NA, prec="double")
    intercept_pval_var <- ncvar_def("intercept_pval", units="", dim=loop_along_dim, missval=NA, prec="double")
    slope_var <- ncvar_def("slope", units=paste0(yunit, " (", xunit, ")-1"), dim=loop_along_dim, missval=NA, prec="double")
    slope_error_var <- ncvar_def("slope_error", units=paste0(yunit, " (", xunit, ")-1"), dim=loop_along_dim, missval=NA, prec="double")
    slope_pval_var <- ncvar_def("slop_pval", units="", dim=loop_along_dim, missval=NA, prec="double")
    message("\ncreate ", fout)
    outnc <- nc_create(filename=fout,
                       vars=list(intercept_var, intercept_error_var, intercept_pval_var,
                                 slope_var, slope_error_var, slope_pval_var),
                       force_v4=T)
    ncvar_put(outnc, intercept_var, intercepts)
    ncvar_put(outnc, intercept_error_var, intercept_errors)
    ncvar_put(outnc, intercept_pval_var, intercept_pvals)
    ncvar_put(outnc, slope_var, slopes)
    ncvar_put(outnc, slope_error_var, slope_errors)
    ncvar_put(outnc, slope_pval_var, slope_pvals)
    nc_close(outnc)

    message("\nfinished\n")

#} # lm_func 

